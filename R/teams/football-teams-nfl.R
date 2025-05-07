# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Utilties for downloading data from cache
source("R/util-data-download.R")
# Utilties for generating unique team ids
source("R/util-hash-generation.R")
# Utilties for generating reports
source("R/util-data-report.R")
# Utilties for updating markdown
source("R/util-markdown.R")

library(rvest, quietly = TRUE, warn.conflicts = FALSE) # Filter through and parse html objects
library(tidyr, quietly = TRUE, warn.conflicts = FALSE) # Unest list attributed to columns in dataframe
library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / Management of dataframes
library(yaml, quietly = TRUE, warn.conflicts = FALSE) # Load yaml configiugration into program

# Read configuration from configs directory
config <- yaml::read_yaml("configs/football_nfl.yaml")
# File to hold formated data
all_teams_file <- "data/processed/football-teams-nfl.csv"

#' NFL Teams
#'
#' Retrieves NFL team data from ESPN's API and supplements it with additional
#' information scraped from the wiki team pages and official nfl webpages. The combined data
#' is processed into a structured dataframe and saved to a CSV file.
#'
#' @values ../../output/tables/nfl_missing_data.png
#'
#' @source https://site.api.espn.com/
#' @source https://en.wikipedia.org/wiki/
#' @source https://www.nfl.com/teams/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#'
#' @return A dataframe containing the following information for each NBA team
#'  id [string] - A generated unique identifier for each team
#'  espn_id [int] - id used be espn to identify team
#'  type [string] - Always set to NBA for team type
#'  abv [string] - Abreviation of team name (ex. DEN)
#'  full_name [string] - Full name of team (ex. Denver Nuggets)
#'  short_name [string] - Short name of team (ex. Nuggets)
#'  primary [string] - Primary color of team uniforms in Hex format
#'  secondary [string] - Secondary color of team uniforms in Hex format
#'  logo [string] - Link to logo image from ESPN
#'  conference [string] - Conference team is associated with (ex. Western)
#'  division [string] - Division team is associated with (ex. Northwest)
#'  twitter [string] -Twitter handle of team starting with '@'
#'  webiste [string] - Website url for team
#'  head_coach [string] - Current head coach of team
#'  offensive_coordinator [string] - Current offensive coordinator of team
#'  defensive_coordinator [string] - Current defensive coordinator of team
#'  general_manager [string] - Current general manager of team
#'  venue [string] - Current venue where team plays
#'
get_formated_data <- function(verbose = TRUE) {
    
    # Processes raw ESPN team JSON data into structured dataframe
    #
    #' @param team List object containing raw team data from ESPN API
    #'
    #' @return A dataframe containing all information in the espn json structure
    #'
    extract_team <- function(team) {
        # Catalog of base fields to extract from data
        base_fields <- c("id", "uid", "slug", "abbreviation", "displayName",
                        "shortDisplayName", "name", "nickname", "location",
                        "color", "alternateColor", "isActive", "isAllStar")
        # Extract all base fields and set to NA if not found
        base_fields_df <- data.frame(
            lapply(base_fields, function(f) ifelse(is.null(team[[f]]), NA, team[[f]])),
            stringsAsFactors = FALSE
        )
        # Assigns names to all basefields
        names(base_fields_df) <- base_fields
        # Extract first logo from raw team data
        base_fields_df$logo <- ifelse(length(team$logos) > 0, team$logos[[1]]$href, NA)
        # Extract all avalaible links and retrive there text and hrefs
        team_links <- setNames(sapply(team$links, function(x) x$href), make.names(sapply(team$links, function(x) x$text)))
        # Bind base fields / logo with avalaible links
        espn_team_df <- cbind(base_fields_df, t(team_links))
    }
    # Download NFL team data
    nfl_espn_teams <- download_fromJSON(config$LINKS$ESPN_TEAMS, simplifyDataFrame = FALSE)
    if (verbose) cat(paste0("\n\033[32mDownloading NFL Teams: ", config$LINKS$ESPN_TEAMS, "\033[0m"))
    # Extract all teams from ESPN json structure
    espn_teams_list <- lapply(nfl_espn_teams$sports[[1]]$leagues[[1]]$teams, function(x) x$team)
    # Run extract teams function on all teams in data
    nfl_espn_teams <- bind_rows(lapply(espn_teams_list, extract_team))
    # Make a column for each cdf location data point
    nfl_espn_teams <- tidyr::unnest_wider(nfl_espn_teams, location, names_sep = ".")
    
    # Init blank data frame for team details
    nfl_team_details <- data.frame()
    # Loop through team detail webpages and select data
    if (verbose) cat(paste0("\n\033[32mDownloading NFL Team Information: https://www.nfl.com/teams/...\033[0m"))
    for (url in config$LINKS$TEAM_DETAILS) {
        # Download page content from url
        page_content <- download_fromHTML(url)
        # Get name from html
        name <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$TEAM_DETAILS$NAME) %>% rvest::html_text() %>% trimws()
        # Find the Twitter link (href containing twitter.com)
        twitter_link <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$TEAM_DETAILS$TWITTER_LINK)
        # Get the text if the twitter handle
        twitter_handle_text <- twitter_link %>% rvest::html_elements(config$ATTRIBUTES$TEAM_DETAILS$TWITTER_TEXT) %>% rvest::html_text()
        # Find conference from xpath
        conference_text <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$TEAM_DETAILS$CONFERENCE) %>% rvest::html_text() %>% trimws()
        # Extract conference (NFC/AFC)
        conference <- gsub("^\\d+[a-z]+\\s([A-Z]+).*", "\\1", conference_text)
        # Extract division (East/West/North/South)
        division <- gsub("^\\d+[a-z]+\\s[A-Z]+\\s", "", conference_text)
        # Extract venue name from xpath
        venue <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$TEAM_DETAILS$VENUE) %>% rvest::html_text() %>% trimws()
        # Get website link from xpath
        website <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$TEAM_DETAILS$WEBSITE) %>% rvest::html_attr("href") %>% trimws()
        # Head coach from xpath
        head_coach <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$TEAM_DETAILS$HEAD_COACH) %>% rvest::html_text() %>% trimws()
        # Return dataframe with parsed information
        nfl_team_details <- rbind(nfl_team_details, data.frame(
            displayName = name,
            conference = conference,
            division = division,
            twitter = twitter_handle_text,
            website = website,
            head_coach = head_coach,
            venue = venue,
            stringsAsFactors = FALSE
        ))
    }

    # Merge with the scraped team details
    nfl_espn_teams <- nfl_espn_teams %>% dplyr::left_join(nfl_team_details, by = "displayName")
    # Generate Unique team ids for each team
    all_nfl_data <- nfl_espn_teams %>%
        dplyr::rename(espn_id = id) %>%
        dplyr::mutate(id = encode_id(slug, abbreviation)) %>%
        # Put uniquie id as first column in dataset and add new colum for nfl ref
        dplyr::select(id, espn_id, abbreviation, displayName, shortDisplayName, division, conference, dplyr::everything()) %>%
        # Remove all uneeded columns
        dplyr::select(-c(slug, uid, name, nickname, location.1, isActive, isAllStar, Clubhouse, Roster,
            Statistics, Schedule, Tickets, Depth.Chart)) %>%
        # Rename all remaining columns
        dplyr::rename(full_name = displayName, short_name = shortDisplayName, primary = color,
            secondary = alternateColor, abv = abbreviation) %>%
        # Capitalize all hex colors
        dplyr::mutate(primary = toupper(primary), secondary = toupper(secondary)) %>%
        # Add a stanard # before color values if isnt already there and value isnt NA
        dplyr::mutate(dplyr::across(c(primary, secondary), ~ ifelse(!is.na(.) & !str_starts(., "#"), paste0("#", .), .))) %>%
        # Create a type column and move after id
        dplyr::mutate(type = "NFL")

    # Dowload current Offensive Coordinators for the NFL
    page_content <- download_fromHTML(config$LINKS$OFFENSIVE_COORDINATORS)
    if (verbose) cat(paste0("\n\033[32mDownloading NFL Offensive Coordinators: ", config$LINKS$OFFENSIVE_COORDINATORS, "\033[0m"))
    # Extract arenas table from wiki page
    tables <- page_content %>% html_elements("table")
    nfc_coaches <- tables[[1]] %>% rvest::html_table(fill = TRUE) %>% dplyr::select(Team, Coordinator) %>% dplyr::rename(offensive_coordinator = Coordinator)
    afc_coaches <- tables[[2]] %>% rvest::html_table(fill = TRUE) %>% dplyr::select(Team, Coordinator) %>% dplyr::rename(offensive_coordinator = Coordinator)
    # Fromat table for combination with team names
    offensive_coordinators <- rbind(nfc_coaches, afc_coaches)
    # Dowload current Offensive Coordinators for the NFL
    page_content <- download_fromHTML(config$LINKS$DEFENSIVE_COORDINATORS)
    if (verbose) cat(paste0("\n\033[32mDownloading NFL Defensive Coordinators: ", config$LINKS$DEFENSIVE_COORDINATORS, "\033[0m\n"))
    # Extract arenas table from wiki page
    tables <- page_content %>% html_elements("table")
    nfc_coaches <- tables[[1]] %>% rvest::html_table(fill = TRUE) %>% dplyr::select(Team, Coordinator) %>% dplyr::rename(defensive_coordinator = Coordinator)
    afc_coaches <- tables[[2]] %>% rvest::html_table(fill = TRUE) %>% dplyr::select(Team, Coordinator) %>% dplyr::rename(defensive_coordinator = Coordinator)
    # Fromat table for combination with team names
    defensive_coordinators <- rbind(nfc_coaches, afc_coaches)

    # Combine offensive_coordinators data by matching team and full name
    all_nfl_data <- merge(all_nfl_data, offensive_coordinators,
    by.x = "full_name", by.y = "Team", all.x = TRUE, all.y = FALSE)
    # Combine defensive_coordinators data by matching team and full name
    all_nfl_data <- merge(all_nfl_data, defensive_coordinators,
    by.x = "full_name", by.y = "Team", all.x = TRUE, all.y = FALSE)
    # Relocated venue to last column
    all_nfl_data <- all_nfl_data %>%
    dplyr::relocate(twitter, .after = last_col()) %>%
    dplyr::relocate(website, .after = last_col()) %>%
    # Reposition all rows for consistancy
    dplyr::relocate(venue, .after = dplyr::last_col()) %>%
    dplyr::select(id, espn_id, type, abv, full_name, dplyr::everything())

    # Analyze missing data
    analyze_missing_data("NFL", all_nfl_data)
    process_markdown_file("R/teams/football-teams-nfl.R", "R/teams/readme.md", nrow(all_nfl_data))

    if (verbose) cat(paste0("\n\033[90mNFL Football Data Saved To: /", all_teams_file, "\033[0m\n"))
    # Save generated csollege data
    write.csv(all_nfl_data, all_teams_file, row.names = FALSE)
    # Return fornated data
    return(all_nfl_data)
}

# If file is being run stand-alone, run function
if (interactive()) get_formated_data()
