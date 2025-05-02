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

# Hardcoded list of mlb team twitter acounts because couldnt find online
MLB_TWITTER_ACCOUNTS <- yaml::read_yaml("data/mutations/baseball_mlb_mutations.yaml")$TWITTER_ACCOUNTS
# Hardcoded list of mbl team websites because couldnt find online
MLB_WEBSITES <- yaml::read_yaml("data/mutations/baseball_mlb_mutations.yaml")$WEBSITES
# Read configuration from configs directory
CONFIG <- yaml::read_yaml("configs/baseball_mlb.yaml")
# File to hold formated data
ALL_TEAMS_FILE <- "data/processed/baseball-teams-mlb.csv"  

#' MLB Teams
#'
#' Retrieves MLB team data from ESPN's API and supplements it with additional 
#' information scraped from the wiki team pages. The combined data is processed into a structured 
#' dataframe and saved to a CSV file.
#' 
#' @source https://site.api.espn.com/
#' @source https://en.wikipedia.org/wiki/
#'
#' @param VERBOSE Logical indicating whether to print progress messages (default: TRUE)
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
#'  league [string] - League team is associated with (ex. National League)
#'  division [string] - Division team is associated with (ex. Central Division)
#'  twitter [string] - Twitter handle of team starting with '@'
#'  webiste [string] - Website url for team
#'  general_manager [string] - Current general manager of team
#'  manager [string] - Current manager of team 
#'  venue [string] - Current venue where team plays 
#'
get_formated_data <- function(VERBOSE = TRUE) {

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
        return(espn_team_df)
    }

    # Download NFL team data
    all_mlb_teams <- download_fromJSON(CONFIG$LINKS$ESPN_TEAMS, simplifyDataFrame = FALSE)
    if (VERBOSE) cat(paste0("\n\033[32mDownloading MLB Teams: ", CONFIG$LINKS$ESPN_TEAMS, "\033[0m"))
    # Extract all teams from ESPN json structure
    espn_teams_list <- lapply(all_mlb_teams$sports[[1]]$leagues[[1]]$teams, function(x) x$team)
    # Run extract teams function on all teams in data
    all_mlb_teams <- bind_rows(lapply(espn_teams_list, extract_team))
    # Make a column for each cdf location data point
    all_mlb_teams <- tidyr::unnest_wider(all_mlb_teams, location, names_sep = ".")

    # Init blank data frame for team details
    mlb_team_details <- data.frame()
    # Loop through team detail webpages and select data 
    if (VERBOSE) cat(paste0("\n\033[32mDownloading MLB Team Information: https://en.wikipedia.org/wiki/...\n\033[0m"))
    for (url in CONFIG$LINKS$TEAM_DETAILS) {
        # Download page content from url
        page_content <- download_fromHTML(url)
        # Get name from xpath
        name <- page_content %>% rvest::html_element(xpath = CONFIG$ATTRIBUTES$TEAM_DETAILS$NAME) %>% rvest::html_text(trim = TRUE)
        # Get arena from xpath
        venue_text <- page_content %>% rvest::html_element(xpath = CONFIG$ATTRIBUTES$TEAM_DETAILS$ARENA) %>% rvest::html_text(trim = TRUE)
        venue <- venue_text %>% gsub("(\\w+)\\s+(\\w+).*", "\\1 \\2", .)
        # Get head coach data from xpath
        coach_text <- page_content %>% rvest::html_element(xpath = CONFIG$ATTRIBUTES$TEAM_DETAILS$COACH) %>% rvest::html_text(trim = TRUE)
        head_coach <- coach_text %>% gsub("(\\w+)\\s+(\\w+).*", "\\1 \\2", .)
        # Get head coach data from xpath
        manager_text <- page_content %>% rvest::html_element(xpath = CONFIG$ATTRIBUTES$TEAM_DETAILS$MANAGER) %>% rvest::html_text(trim = TRUE)
        manager <- manager_text %>% gsub("(\\w+)\\s+(\\w+).*", "\\1 \\2", .)
        # Get division data from xpath
        division <- page_content %>% rvest::html_element(xpath = CONFIG$ATTRIBUTES$TEAM_DETAILS$DIVISION) %>% rvest::html_text(trim = TRUE)
        # Get conference data from xpath
        conference <- page_content %>% rvest::html_element(xpath = CONFIG$ATTRIBUTES$TEAM_DETAILS$CONFERENCE) %>% rvest::html_text(trim = TRUE)
        # Return dataframe with parsed information
        mlb_team_details <- rbind(mlb_team_details, data.frame(
            displayName = name,
            conference = conference,
            division = division,
            twitter = paste0("@", MLB_TWITTER_ACCOUNTS[name]),
            website = MLB_WEBSITES[[name]],
            manager = head_coach,
            general_manager = manager,
            venue = venue,
            stringsAsFactors = FALSE
        ))
    
    }

    # Merge with the scraped team details
    all_mlb_teams <- all_mlb_teams %>% dplyr::left_join(mlb_team_details, by = "displayName") %>% dplyr::rename(espn_id = id)

    # Generate Unique team ids for each team
    all_mlb_teams <- all_mlb_teams %>% dplyr::mutate(id = encode_id(paste0("B", espn_id), abbreviation)) %>% dplyr::filter(isActive == TRUE) %>%
        # Remove uneeded columns
        dplyr::select(-c(uid, name, nickname, isActive, isAllStar, Clubhouse, Roster,
        Statistics, Schedule, Tickets, Depth.Chart, slug, 'location.1')) %>%
        # Rename all columns to follow standard
        dplyr::rename(full_name = displayName, short_name = shortDisplayName, primary = color, 
            secondary = alternateColor, abv = abbreviation) %>%
        # Capitalize all hex colors
        dplyr::mutate(primary = toupper(primary), secondary = toupper(secondary)) %>%
        # Add a stanard # before color values if isnt already there and value isnt NA
        dplyr::mutate(dplyr::across(c(primary, secondary), ~ ifelse(!is.na(.) & !str_starts(., "#"), paste0("#", .), .))) %>%
        # Create a type column and move after id
        dplyr::mutate(type = "MLB") %>% dplyr::select(id, espn_id, type, dplyr::everything()) %>% dplyr::relocate(venue, .after = last_col()) 

    # Analyze missing data
    analyze_missing_data("MLB", all_mlb_teams)
    process_markdown_file("R/teams/baseball-teams-mlb.R", "R/teams/readme.md", nrow(all_mlb_teams))

    if (VERBOSE) cat(paste0("\n\033[90mMLB Baseball Data Saved To: /", ALL_TEAMS_FILE, "\033[0m\n"))
    # Save any created name bindings to file
    write.csv(all_mlb_teams, ALL_TEAMS_FILE, row.names = FALSE)
    # Return formated data
    return(all_mlb_teams)
}

# If file is being run stand-alone, run function
if (interactive()) get_formated_data()



