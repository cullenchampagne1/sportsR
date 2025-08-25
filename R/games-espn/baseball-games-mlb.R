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

library(rvest, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(yaml, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)

# Read configuration from configs directory
config <- yaml::read_yaml("configs/baseball-mlb.yaml")
# File to hold formatted data
all_games_file <- "data/processed/baseball-games-mlb.csv"

#' MLB Baseball Games
#'
#' Retrieves MLB baseball game data from ESPN's API and other sources. The combined data
#' is processed into a structured dataframe and saved to a CSV file.
#'
#' @values ../../output/tables/mlb_baseball_games_missing_data.png
#'
#' @source https://site.api.espn.com/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param save Logical indicating whether to save data to data/processed folder
#'
#' @return A dataframe containing the following information for each MLB baseball game:
#'  id [string] - A generated unique identifier for each game
#'  espn_id [string] - ESPN-assigned game ID
#'  type [string] - Sport type code (e.g., "BS" for baseball)
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

    # Read configuration from configs directory
    config <- yaml::read_yaml("configs/baseball-mlb.yaml")
    # File to hold formatted data
    all_games_file <- "data/processed/baseball-games-mlb.csv"

    # Get current year to backlog data 2 years
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    # List to hold all available game links
    all_game_links <- list()
    # Loop through types and previous years to get historical games (paginated, type-aware, safe)
    for (type_id in 1:4) {
      for (i in (current_year - 2):(current_year - 1)) {
        page <- 1
        repeat {
          url <- paste0("https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/seasons/", i, "/types/", type_id, "/events?page=", page)
          season_events <- tryCatch(download_fromJSON(url, simplifyDataFrame = FALSE), error = function(e) NULL)
          if (is.null(season_events) || is.null(season_events$items) || !is.list(season_events$items) || length(season_events$items) == 0) break
          refs <- purrr::map_chr(season_events$items, ~ .x$`$ref`)
          meta_refs <- purrr::map(refs, ~ list(
            url = .x,
            season = i
          ))
          all_game_links <- append(all_game_links, meta_refs)
          if (!is.null(season_events$pageCount) && page >= season_events$pageCount) break
          page <- page + 1
        }
      }
    }

    # Empty dataframe to hold all game information
    games <- data.frame()
    # Loop through all available links with error handling and updated play-by-play URL
    for (entry in all_game_links) {
      link <- entry$url
      season <- entry$season
      if (verbose) cat(paste0("\n\033[32mDownloading Game Information: ", link, "\033[0m"))
      game_info <- tryCatch({
        game_json <- download_fromJSON(link, simplifyDataFrame = FALSE)
        comp <- game_json$competitions[[1]]
        home_idx <- which(purrr::map_chr(comp$competitors, "homeAway") == "home")[1]
        away_idx <- which(purrr::map_chr(comp$competitors, "homeAway") == "away")[1]
        home_comp <- comp$competitors[[home_idx]]
        away_comp <- comp$competitors[[away_idx]]
        play_by_play_url <- paste0("https://site.api.espn.com/apis/site/v2/sports/baseball/mlb/playbyplay?event=", game_json$id)
        list(
          espn_id = game_json$id,
          type = "BSB",
          date = game_json$date,
          season = season,
          title = game_json$name,
          short_tile = game_json$shortName,
          venue = comp$venue$fullName,
          home_espn_id = home_comp$id,
          away_espn_id = away_comp$id,
          play_by_play = play_by_play_url,
          odds_ref = paste0("https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/events/", game_json$id, "/competitions/", game_json$id, "/odds"),
          home_score_ref = paste0("https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/events/", game_json$id, "/competitions/", game_json$id, "/competitors/", home_comp$id, "/score"),
          away_score_ref = paste0("https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/events/", game_json$id, "/competitions/", game_json$id, "/competitors/", away_comp$id, "/score")
        )
      }, error = function(e) NULL)
      if (!is.null(game_info)) {
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

    games <- games %>%
      dplyr::mutate(id = encode_id(paste0("M", espn_id), short_tile, 8)) %>%
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
                    home_espn_id, away_espn_id, home_score, away_score, winner,
                    over_under_total, home_spread, away_spread, home_moneyline, away_moneyline, play_by_play)

    analyze_missing_data("MLB Baseball Games", games)
    # process_markdown_file("R/games/baseball-games-mlb.R", "R/games/readme.md", nrow(games), "games")

    if (verbose) cat(paste0("\n\n\033[90mMLB Baseball Data Saved To: /", all_games_file, "\033[0m\n"))

    if (save) write.csv(games, all_games_file, row.names = FALSE)
    if (save) saveRDS(games, sub("\\.csv$", ".rds", all_games_file))

    return(games)
}
