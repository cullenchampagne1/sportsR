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
library(purrr, quietly = TRUE, warn.conflicts = FALSE)  # Map functions to values in dataframe

# Read configuration from configs directory
config <- yaml::read_yaml("configs/football-nfl.yaml")
# File to hold formated data
all_players_file <- "data/processed/football-players-nfl.csv"

#' NFL Players
#'
#' Retrieves National Football League players from espn's API and other
#' sources. The combined data is processed into a structured dataframe
#' and saved to a CSV file.
#'
#' @values ../../output/tables/nfl_players_missing_data.png
#'
#' @source https://site.api.espn.com/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param save Logical indicating weather to save data to data/processed folder
#'
#' @return A dataframe containing the following information for each basketball team
#'  id [int] - A generated unique identifier for each team
#'  espn_id [int] - id used be espn to identify player
#'  first_name [string] - first name of player
#'  last_name [string] - last name of player
#'  full_name [string] - first and last name of player
#'  short_name [string] - shortand version of the players name
#'  headshot [string] - url to players headshot
#'  jersey [int] - jersey number for player
#'  weight[int] - weight of player
#'  height [int] - height of player
#'  position [string] - position abv of player
#'  team_espn_id [int] - id used be espn to identify players team
#'  college_espn_id [int] - id used be espn to identify players college team
#'
get_formated_data <- function(verbose = TRUE, save = TRUE) {
    
    # Grab College Football data from ESPN
    espn_players <- download_fromJSON(config$LINKS$PLAYERS_ESPN, force_refresh = FALSE, simplifyDataFrame = FALSE)
    if (verbose) cat(paste0("\n\033[32mDownloading ESPN Football Players: ", config$LINKS$ESPN_TEAMS, "\033[0m"))
    # Extract the items list from json returned
    espn_items <- espn_players$items[!sapply(espn_players$items, function(x) grepl("^\\[", x$lastName))]
    # Filter by only active players
    espn_items <- espn_items[sapply(espn_items, function(x) isTRUE(x$active))]

    #' Helper function to retrieve detailed player information from espn api
    #'
    #' @param espn_id id of the player
    #' @param name name of player for logging
    #'
    fetch_player_details <- function(espn_id, name) {
        player_url <- gsub("\\{id\\}", espn_id, config$LINKS$ESPN_DETAILED)
        if (verbose) cat(paste0("\n\033[32mDownloading ", name, " Detailed: ", player_url, "\033[0m"))
        player_details <- download_fromJSON(player_url, force_refresh = FALSE)
        # Return detailed information as a tribble
        tibble(
            position = player_details$athlete$position$abbreviation %||% NA_character_,
            team_espn_id = player_details$athlete$team$id %||% NA_character_,
            college_espn_id = player_details$athlete$collegeTeam$id %||% NA_character_,
        )
    }

    # Convert the list to a dataframe and extract details for each
    espn_players <- purrr::map_dfr(espn_items, function(player) {
        details <- fetch_player_details(player$id, player$fullName)
        tibble(
            espn_id = player$id %||% NA_character_,
            first_name = player$firstName %||% NA_character_,
            last_name = player$lastName %||% NA_character_,
            full_name = player$fullName %||% NA_character_,
            short_name = player$shortName %||% NA_character_,
            headshot = paste0("https://a.espncdn.com/i/headshots/nfl/players/full/", espn_id, ".png"),
            jersey = player$jersey %||% NA_character_,
            weight = player$weight %||% NA,
            height = player$height %||% NA
        ) %>% bind_cols(details)
    }) %>%
    # Only keep players associated with a valid team
    filter(!is.na(team_espn_id)) %>%
    dplyr::mutate(id = encode_id(paste0("B", espn_id), first_name)) %>%
    # Reorder columns and remove active data
    dplyr::select(id, dplyr::everything())

    # Analyze missing data
    analyze_missing_data("NFL Players", espn_players)
    process_markdown_file("R/players/football-players-nfl.R", "R/players/readme.md", nrow(espn_players), "players")

    if (verbose) cat(paste0("\n\n\033[90mNFL Football Data Saved To: /", all_players_file, "\033[0m\n"))
    # Save any created name bindings to file
    if (save) write.csv(espn_players, all_players_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(espn_players, sub("\\.csv$", ".rds", all_players_file))
    # Return formated data
    return(espn_players)
}

# If file is being run stand-alone, run function
invisible(get_formated_data())
