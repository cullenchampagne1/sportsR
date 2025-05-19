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
library(fuzzyjoin, quietly = TRUE, warn.conflicts = FALSE)

# Read configuration from configs directory
config <- yaml::read_yaml("configs/baseball-mlb.yaml")
# File to hold formated data
all_players_file <- "data/processed/baseball-players-mlb.csv"

get_formated_data <- function(verbose = TRUE, save = TRUE) {
    
    # Grab College Football data from ESPN
    espn_players <- download_fromJSON(config$LINKS$PLAYERS_ESPN, force_refresh = FALSE, simplifyDataFrame = FALSE)
    if (verbose) cat(paste0("\n\033[32mDownloading ESPN Baseball Players: ", config$LINKS$ESPN_TEAMS, "\033[0m"))
    # Extract the items list from json returned
    espn_items <- espn_players$items[!sapply(espn_players$items, function(x) grepl("^\\[", x$lastName))]
    # Convert to list to a dataframe
    espn_players <- purrr::map_dfr(espn_items, function(player) {
        tibble(
            espn_id = player$id %||% NA_character_,
            first_name = player$firstName %||% NA_character_,
            last_name = player$lastName %||% NA_character_,
            full_name = player$fullName %||% NA_character_,
            short_name = player$shortName %||% NA_character_,
            active = player$active %||% NA
        )
    }) %>%
    dplyr::mutate(id = encode_id(paste0("B", espn_id), first_name)) %>%
    # Only keep active players
    dplyr::filter(active == TRUE) %>%
    # Reorder columns and remove active data
    dplyr::select(id, dplyr::everything(), -active)

    if (verbose) cat(paste0("\n\033[90mMLB Baseball Data Saved To: /", all_players_file, "\033[0m\n"))
    # Save any created name bindings to file
    if (save) write.csv(espn_players, all_players_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(espn_players, sub("\\.csv$", ".rds", all_players_file))
    # Return formated data
    return(espn_players)
}

# If file is being run stand-alone, run function
invisible(get_formated_data())
