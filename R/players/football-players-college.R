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

# Utilities for downloading data from cache
source("R/util-data-download.R")
# Utilities for generating unique team ids
source("R/util-hash-generation.R")
# Utilities for generating reports
source("R/util-data-report.R")
# Utilities for updating markdown
source("R/util-markdown.R")

library(rvest, quietly = TRUE, warn.conflicts = FALSE) # Filter through and parse html objects
library(tidyr, quietly = TRUE, warn.conflicts = FALSE) # Unnest list attributes to columns in dataframe
library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / management of dataframes
library(yaml, quietly = TRUE, warn.conflicts = FALSE) # Load YAML configuration into program
library(purrr, quietly = TRUE, warn.conflicts = FALSE)  # Map functions to values in dataframe

# Read configuration from configs directory
config <- yaml::read_yaml("configs/football-college.yaml")
# File to hold formatted data
all_players_file <- "data/processed/football-players-college.csv"

#' College Football Players
#'
#' Retrieves All NCAA football players from espn's API and other sources. The combined data
#' is processed into a structured dataframe and saved to a CSV file.
#'
#' @values ../../output/tables/college_football_players_missing_data.png
#'
#' @source https://site.api.espn.com/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param save Logical indicating whether to save data to data/processed folder
#'
#' @return A dataframe containing the following information for each football player
#'  id [int] - A generated unique identifier for each team
#'  espn_id [int] - ID used by ESPN to identify the player
#'  first_name [string] - first name of player
#'  last_name [string] - last name of player
#'  full_name [string] - first and last name of player
#'  short_name [string] - shorthand version of the player's name
#'  headshot [string] - url to players headshot
#'  jersey [int] - jersey number for player
#'  weight [int] - weight of player
#'  height [int] - height of player
#'  position [string] - position abv of player
#'  team_espn_id [int] - ID used by ESPN to identify the player's team
#'
get_formated_players <- function(verbose = TRUE, save = TRUE) {

    # Grab College Football data from ESPN
    college_espn_teams <- download_fromJSON(config$LINKS$ESPN_TEAMS, force_refresh = FALSE, simplifyDataFrame = FALSE)
    if (verbose) cat(paste0("\n\033[32mDownloading ESPN Football Teams: ", config$LINKS$ESPN_TEAMS, "\033[0m"))
    # Extract all team ids from ESPN json structure
    college_espn_team_ids <- sapply(college_espn_teams$sports[[1]]$leagues[[1]]$teams, function(x) x$team$id)
    # Create blank dataframe for code to append too
    espn_players <- data.frame()
    # Loop through each team to collect roster information
    for (team_id in college_espn_team_ids) {
        url <- paste0("https://site.api.espn.com/apis/site/v2/sports/football/college-football/teams/", team_id, "/roster")
        if (verbose) cat(paste0("\n\033[32mDownloading Football Roster: ", url, "\033[0m"))
        # Download roster for current team
        roster_data <- download_fromJSON(url, force_refresh = FALSE, simplifyDataFrame = FALSE)
        # Extract player details from roster_data
        players <- purrr::map_dfr(roster_data$athletes, function(group) {
            purrr::map_dfr(group$items, function(player) {
                tibble::tibble(
                    espn_id = player$id,
                    first_name = player$firstName,
                    last_name = player$lastName,
                    full_name = player$fullName,
                    short_name = player$shortName,
                    headshot =  player$headshot$href %||% NA_character_,
                    jersey = player$jersey %||% NA,
                    weight = player$weight %||% NA,
                    height = player$height %||% NA,
                    position = player$position$abbreviation,
                    team_espn_id = team_id
                )
            })
        })
        espn_players <- dplyr::bind_rows(espn_players, players)
    }
    # Create unique ID for all players and put as first column
    espn_players <- espn_players %>% dplyr::mutate(id = encode_id(paste0("F", espn_id), first_name)) %>% select(id, dplyr::everything())

    # Analyze missing data
    analyze_missing_data("College Football Players", espn_players)
    process_markdown_file("R/players/football-players-college.R", "R/players/readme.md", nrow(espn_players), "players")

    if (verbose) cat(paste0("\n\n\033[90mCollege Football Data Saved To: /", all_players_file, "\033[0m\n"))
    # Save any created name bindings to file
    if (save) write.csv(espn_players, all_players_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(espn_players, sub("\\.csv$", ".rds", all_players_file))
    # Return formatted data
    return(espn_players)
}
