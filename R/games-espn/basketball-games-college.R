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
config <- yaml::read_yaml("configs/basketball-college.yaml")
# File to hold formatted data
all_games_file <- "data/processed/basketball-games-college.csv"

#' College Basketball Games
#'
#' Retrieves college basketball game data from ESPN's API and other sources. The combined data
#' is processed into a structured dataframe and saved to a CSV file.
#'
#' @values ../../output/tables/nfl_football_games_missing_data.png
#'
#' @source https://site.api.espn.com/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param save Logical indicating whether to save data to data/processed folder
#'
#' @return A dataframe containing the following information for each college basketball game:
#'  id [string] - A generated unique identifier for each game
#'  espn_id [string] - ESPN-assigned game ID
#'  type [string] - Sport type code (e.g., "FB" for football)
#'  date [string] - Date and time of the game
#'  season [int] - Season year
#'  title [string] - Full title of the game
#'  short_tile [string] - Shortened title of the game
#'  venue [string] - Venue where the game is played
#'  home_espn_id [string] - ESPN ID for the home team
#'  away_espn_id [string] - ESPN ID for the away team
#'  play_by_play [string] - URL to the game's play-by-play JSON data
#'
get_formated_games <- function(verbose = TRUE, save = TRUE) {

    # Construct a sequence of dates from January 1, 2023 to December 31, 2024
    start_date <- as.Date("2023-01-01")
    end_date <- as.Date("2024-12-31")
    all_dates <- seq.Date(start_date, end_date, by = "day")
    # Empty dataframe to hold all team information
    games <- data.frame()
    # Loop over each date and fetch games
    for (i in seq_along(all_dates)) {
        the_date <- all_dates[i]
        date_str <- format(the_date, "%Y%m%d")
        url <- paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?dates=", date_str, "&limit=500")
        if (verbose) cat(paste0("\n\033[32mDownloading Game Information: ", url, "\033[0m"))
        # Try to download the scoreboard for this date
        scoreboard_json <- tryCatch(download_fromJSON(url, simplifyDataFrame = FALSE), error = function(e) NULL)
        if (is.null(scoreboard_json) || is.null(scoreboard_json$events)) next
        # Loop over each event (game)
        for (game in scoreboard_json$events) {
            # Defensive: skip if missing id or competitions
            if (is.null(game$id) || is.null(game$competitions) || length(game$competitions) == 0) next
            comp <- game$competitions[[1]]
            # Identify home and away competitors
            home_idx <- which(purrr::map_chr(comp$competitors, "homeAway") == "home")[1]
            away_idx <- which(purrr::map_chr(comp$competitors, "homeAway") == "away")[1]
            if (is.na(home_idx) || is.na(away_idx)) next
            home_comp <- comp$competitors[[home_idx]]
            away_comp <- comp$competitors[[away_idx]]
            # Play-by-play URL
            pbp_url <- paste0("http://sports.core.api.espn.com/v2/sports/basketball/leagues/mens-college-basketball/events/", game$id, "/competitions/", game$id, "/plays?lang=en&region=us")
            # Return gathered info as list
            game_info <- list(
              espn_id = game$id,
              type = "CBK",
              date = game$date,
              season = as.integer(format(as.Date(game$date), "%Y")),
              title = game$name,
              short_tile = game$shortName,
              venue = if (!is.null(comp$venue) && !is.null(comp$venue$fullName)) comp$venue$fullName else NA,
              home_espn_id = home_comp$id,
              away_espn_id = away_comp$id,
              play_by_play = pbp_url,
              odds_ref = paste0("https://sports.core.api.espn.com/v2/sports/basketball/leagues/mens-college-basketball/events/", game$id, "/competitions/", game$id, "/odds"),
              home_score_ref = paste0("https://sports.core.api.espn.com/v2/sports/basketball/leagues/mens-college-basketball/events/", game$id, "/competitions/", game$id, "/competitors/", home_comp$id, "/score"),
              away_score_ref = paste0("https://sports.core.api.espn.com/v2/sports/basketball/leagues/mens-college-basketball/events/", game$id, "/competitions/", game$id, "/competitors/", away_comp$id, "/score")
            )
            games <- dplyr::bind_rows(games, game_info)
        }
    }
    # Extract actual scores and winner info
    scores <- purrr::pmap_dfr(list(games$home_score_ref, games$away_score_ref, games$home_espn_id, games$away_espn_id), function(home_url, away_url, home_id, away_id) {
      home_score_json <- tryCatch(download_fromJSON(home_url, simplifyDataFrame = TRUE), error = function(e) NULL)
      away_score_json <- tryCatch(download_fromJSON(away_url, simplifyDataFrame = TRUE), error = function(e) NULL)

      tibble::tibble(
        home_score = if (!is.null(home_score_json)) home_score_json$value else NA,
        away_score = if (!is.null(away_score_json)) away_score_json$value else NA,
        winner = dplyr::case_when(
          !is.null(home_score_json) && "winner" %in% names(home_score_json) && is.logical(home_score_json$winner) && home_score_json$winner ~ home_id,
          !is.null(away_score_json) && "winner" %in% names(away_score_json) && is.logical(away_score_json$winner) && away_score_json$winner ~ away_id,
          TRUE ~ NA_character_
        )
      )
    })

    # Extract odds information
    odds_data <- purrr::pmap_dfr(list(games$odds_ref, games$home_espn_id, games$away_espn_id), function(odds_url, home_id, away_id) {
      odds_json <- tryCatch(download_fromJSON(odds_url, simplifyDataFrame = FALSE), error = function(e) NULL)

      if (is.null(odds_json) || is.null(odds_json$items) || length(odds_json$items) == 0) {
        return(tibble::tibble(
          over_under_total = NA,
          home_spread = NA,
          away_spread = NA,
          home_moneyline = NA,
          away_moneyline = NA
        ))
      }

      item <- odds_json$items[[1]]

      tibble::tibble(
        over_under_total = item$current$total$alternateDisplayValue %||% NA,
        home_spread = item$homeTeamOdds$current$pointSpread$american %||% NA,
        away_spread = item$awayTeamOdds$current$pointSpread$american %||% NA,
        home_moneyline = item$homeTeamOdds$current$moneyLine$alternateDisplayValue %||% NA,
        away_moneyline = item$awayTeamOdds$current$moneyLine$alternateDisplayValue %||% NA
      )
    })

    games <- dplyr::bind_cols(games, scores, odds_data)

    # Generate a uniquie internal id for each game
    games <- games %>%
      dplyr::mutate(id = encode_id(paste0("CB", espn_id), short_tile, 8)) %>%
      dplyr::mutate(
        home_spread = ifelse(is.na(home_spread) & !is.na(away_spread), -as.numeric(away_spread), home_spread),
        away_spread = ifelse(is.na(away_spread) & !is.na(home_spread), -as.numeric(home_spread), away_spread),
        home_moneyline = ifelse(
          is.na(home_moneyline) & !is.na(away_moneyline),
          as.character(-as.numeric(away_moneyline)),
          ifelse(home_moneyline == "EVEN", as.character(away_moneyline), home_moneyline)
        ),
        away_moneyline = ifelse(
          is.na(away_moneyline) & !is.na(home_moneyline),
          as.character(-as.numeric(home_moneyline)),
          ifelse(away_moneyline == "EVEN", as.character(home_moneyline), away_moneyline)
        )
      ) %>%
      dplyr::select(id, espn_id, type, date, season, title, short_tile, venue,
                    home_espn_id, away_espn_id, home_score, away_score, winner, over_under_total, home_spread,
                    away_spread, home_moneyline, away_moneyline, play_by_play)

    # Analyze missing data
    analyze_missing_data("College Basketball Games", games)
    # process_markdown_file("R/games/basketball-games-college.R", "R/games/readme.md", nrow(games), "games")

    if (verbose) cat(paste0("\n\n\033[90mCollege Basketball Data Saved To: /", all_games_file, "\033[0m\n"))
    # Save any created name bindings to file
    if (save) write.csv(games, all_games_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(games, sub("\\.csv$", ".rds", all_games_file))
    # Return formatted data
    return(games)
}
