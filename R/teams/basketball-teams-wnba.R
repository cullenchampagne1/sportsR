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

# Hardcoded list of nba team twitter acounts because couldnt find online
wnba_twitter_accounts <- readRDS(url("https://github.com/cullenchampagne1/sportsR/releases/download/misc/wnba-twitter-bindings.rds", open = "rb"))
# Read configuration from configs directory
config <- yaml::read_yaml("configs/basketball-wnba.yaml")
# File to hold formated data
all_teams_file <- "data/processed/basketball-teams-wnba.csv"

#' WNBA Teams
#'
#' Retrieves WNBA team data from ESPN's API and supplements it with additional
#' information scraped from the wiki team pages. The combined data is processed into a
#' structured dataframe and saved to a CSV file.
#'
#' @values ../../output/tables/wnba_missing_data.png
#'
#' @source https://site.api.espn.com/
#' @source https://en.wikipedia.org/wiki/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param save Logical indicating weather to save data to data/processed folder
#'
#' @return A dataframe containing the following information for each NBA team
#'  id [string] - A generated unique identifier for each team
#'  espn_id [int] - id used be espn to identify team
#'  type [string] - Always set to NBA for team type
#'  abv [string] - Abreviation of team name (ex. DEN)
#'  full_name [string] - Full name of team (ex. Denver Nuggets)
#'  short_name [string] - Short name of team (ex. Nuggets)
#'  conference [string] - Conference team is associated with (ex. Western)
#'  primary [string] - Primary color of team uniforms in Hex format
#'  secondary [string] - Secondary color of team uniforms in Hex format
#'  logo [string] - Link to logo image from ESPN
#'  head_coach [string] - Current head coach of team
#'  general_manager [string] - Current general manager of team
#'  twitter [string] - Twitter handle of team starting with '@'
#'  webiste [string] - Website url for team
#'  venue [string] - Current venue where team plays
#'
get_formated_teams <- function(verbose = TRUE, save = TRUE) {
    # Grab College Football data from ESPN
    all_espn_teams <- download_fromJSON(config$LINKS$ESPN_TEAMS, simplifyDataFrame = FALSE)
    if (verbose) cat(paste0("\n\033[32mDownloading WNBA Teams: ", config$LINKS$ESPN_TEAMS, "\033[0m"))

    # Extract all teams from ESPN json structure
    espn_teams_list <- lapply(all_espn_teams$sports[[1]]$leagues[[1]]$teams, function(x) x$team)

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
    # Run extract teams function on all teams in data
    espn_wnba_teams <-  dplyr::bind_rows(lapply(espn_teams_list, extract_team)) %>% dplyr::rename(espn_id = id)

    # Init blank data frame for team details
    wnba_team_details <- data.frame()
    # Loop through team detail webpages and select data
    if (verbose) cat(paste0("\n\033[32mDownloading WNBA Team Information: https://en.wikipedia.org/wiki/...\n\033[0m"))
    for (url in config$LINKS$TEAM_DETAILS) {
        # Download page content from url
        page_content <- download_fromHTML(url)
        # Get name from xpath
        name <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$TEAM_DETAILS$NAME) %>% rvest::html_text(trim = TRUE)
        # Get arena data from xpath
        arena_text <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$TEAM_DETAILS$ARENA) %>% rvest::html_text(trim = TRUE)
        venue <- arena_text %>% gsub("\\[[a-z0-9]+\\]", "", .)
        # Get head coach data from xpath
        coach_text <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$TEAM_DETAILS$COACH) %>% rvest::html_text(trim = TRUE)
        head_coach <- coach_text %>% gsub("(\\w+)\\s+(\\w+).*", "\\1 \\2", .)
        # Get general manager data from xpath
        manager_text <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$TEAM_DETAILS$GENERAL_MANAGER) %>% rvest::html_text(trim = TRUE)
        manager <- coach_text %>% gsub("(\\w+)\\s+(\\w+).*", "\\1 \\2", .)
        # Get conference data from xpath
        conference <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$TEAM_DETAILS$CONFERENCE) %>% rvest::html_text(trim = TRUE)
        # Return dataframe with parsed information
        wnba_team_details <- rbind(wnba_team_details, data.frame(
            displayName = name,
            conference = conference,
            twitter = wnba_twitter_accounts[name],
            website = paste0("https://", tolower(sub(".* ", "", name)), ".wnba.com/"),
            head_coach = head_coach,
            general_manager = manager,
            venue = venue,
            stringsAsFactors = FALSE
        ))
    }

    # Merge with the scraped team details
    espn_wnba_teams <- espn_wnba_teams %>% dplyr::left_join(wnba_team_details, by = "displayName")

    # Generate Unique team ids for each team
    all_wnba_teams <- espn_wnba_teams %>%
        dplyr::mutate(id = encode_id(paste0("W", espn_id), abbreviation)) %>%
        dplyr::filter(isActive == TRUE) %>%
        # Remove uneeded columns
        dplyr::select(-c(uid, name, nickname, isActive, isAllStar, Clubhouse, Roster,
        Statistics, Schedule, Tickets, location, slug)) %>%
        # Rename all columns to follow standard
        dplyr::rename(full_name = displayName, short_name = shortDisplayName, primary = color,
            secondary = alternateColor, abv = abbreviation) %>%
        # Capitalize all hex colors
        dplyr::mutate(primary = toupper(primary), secondary = toupper(secondary)) %>%
        # Add a stanard # before color values if isnt already there and value isnt NA
        dplyr::mutate(dplyr::across(c(primary, secondary), ~ ifelse(!is.na(.) & !str_starts(., "#"), paste0("#", .), .))) %>%
        # Create a type column and move after id - reorder all columns to match standard
        dplyr::mutate(type = "WBKB") %>%
        dplyr::select(id, espn_id, type, abv, full_name, short_name, conference, dplyr::everything()) %>%
        dplyr::relocate(twitter, .after = last_col()) %>%
        dplyr::relocate(website, .after = last_col()) %>%
        dplyr::relocate(venue, .after = last_col())

    # Analyze missing data
    analyze_missing_data("WNBA", all_wnba_teams)
    process_markdown_file("R/teams/basketball-teams-wnba.R", "R/teams/readme.md", nrow(all_wnba_teams))

    if (verbose && save) cat(paste0("\n\033[90mWNBA Basketball Data Saved To: /", all_teams_file, "\033[0m\n"))
    # Save any created name bindings to file
    if (save) write.csv(all_wnba_teams, all_teams_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(all_wnba_teams, sub("\\.csv$", ".rds", all_teams_file))
    # Return formated data
    return(all_wnba_teams)
}
