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

library(rvest, quietly = TRUE, warn.conflicts = FALSE) # Filter through and parse html objects
library(tidyr, quietly = TRUE, warn.conflicts = FALSE) # Unest list attributed to columns in dataframe
library(purrr, quietly = TRUE, warn.conflicts = FALSE)  # Map functions to values in dataframe
library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / Management of dataframes
library(httr, quietly = TRUE, warn.conflicts = FALSE) # Header management for downloading data
library(yaml, quietly = TRUE, warn.conflicts = FALSE) # Load yaml configiugration into program
library(stringdist, quietly = TRUE, warn.conflicts = FALSE) # Used by fastlink
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE) # Used by fastlink
library(caret, quietly = TRUE, warn.conflicts = FALSE) # Used by fastlink

# Utilties for downloading data from cache
source("R/util-data-download.R")

# get_formated_data <- function(sport) {
#     # Load configuration file for the specified sport
#     config <- yaml::read_yaml(paste0("configs/", sport, "_college.yaml"))
#     # Grab College sport data from ESPN
#     college_espn_teams <- download_fromJSON(config$LINKS$ESPN_TEAMS, force_refresh = FALSE, simplifyDataFrame = FALSE)

#     # Extract all teams from ESPN json structure
#     espn_teams_list <- lapply(college_espn_teams$sports[[1]]$leagues[[1]]$teams, function(x) x$team)

#     # Processes raw ESPN team JSON data into structured dataframe
#     #
#     #' @param team List object containing raw team data from ESPN API
#     #'
#     #' @return A dataframe containing all information in the espn json structure
#     #'
#     extract_team <- function(team) {
#         # Catalog of base fields to extract from data
#         base_fields <- c("id", "uid", "slug", "abbreviation", "displayName",
#                         "shortDisplayName", "name", "nickname", "location",
#                         "color", "alternateColor", "isActive", "isAllStar")
#         # Extract all base fields and set to NA if not found
#         base_fields_df <- data.frame(
#             lapply(base_fields, function(f) ifelse(is.null(team[[f]]), NA, team[[f]])),
#             stringsAsFactors = FALSE
#         )
#         # Assigns names to all basefields
#         names(base_fields_df) <- base_fields
#         # Extract first logo from raw team data
#         base_fields_df$logo <- ifelse(length(team$logos) > 0, team$logos[[1]]$href, NA)
#         # Extract all avalaible links and retrive there text and hrefs
#         team_links <- setNames(sapply(team$links, function(x) x$href), make.names(sapply(team$links, function(x) x$text)))
#         # Bind base fields / logo with avalaible links
#         espn_team_df <- cbind(base_fields_df, t(team_links))
#     }

#     # Run extract teams function on all teams in data
#     college_espn_teams <- dplyr::bind_rows(lapply(espn_teams_list, extract_team)) %>%
#     mutate(id = as.character(id)) %>%
#     rename(espn_id = id) %>%
#     select(espn_id, slug, abbreviation, displayName, shortDisplayName, name,	nickname, location)

#     # Dataframe to hold all parsed NCAA teams
#     college_ncaa_teams <- data.frame()
#     # Loop through each division
#     for (link in config$LINKS$NCAA_TEAMS) {
#         # Get division by index
#         division <- sub(paste0("^.*/", sport, "/([^/]+).*"), "\\1", link)
#         # Download first page of current division teams
#         page_content <- download_fromHTML(link, force_refresh = TRUE)
#         # Initialize an empty list to store data
#         team_data <- list()
#         # Get all rows on the current page
#         current_team_rows <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_TEAMS$ROWS)
#         # Loop through each row
#         for (row in current_team_rows) {
#             team_columns <- row %>% rvest::html_elements(config$ATTRIBUTES$NCAA_TEAMS$COLUMNS)
#             if (length(team_columns) < 2) next
#             img_src <- team_columns[config$ATTRIBUTES$NCAA_TEAMS$COLUMN_NUMBER] %>%
#                 rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_TEAMS$IMG_SRC) %>%
#                 rvest::html_text(trim = TRUE)
#             school_url <- team_columns[config$ATTRIBUTES$NCAA_TEAMS$COLUMN_NUMBER] %>%
#                 rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_TEAMS$SCHOOL_URL) %>%
#                 rvest::html_text(trim = TRUE)
#             school_name <- team_columns[config$ATTRIBUTES$NCAA_TEAMS$COLUMN_NUMBER] %>%
#                 rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_TEAMS$SCHOOL_NAME) %>%
#                 rvest::html_text(trim = TRUE)
#             team_data[[length(team_data) + 1]] <- data.frame(
#                 school_name = school_name,
#                 division = division,
#                 img_src = img_src,
#                 school_url = paste0("https://www.ncaa.com", school_url),
#                 stringsAsFactors = FALSE
#             )
#         }
#             # Add team to data frame
#         college_ncaa_teams <- rbind(college_ncaa_teams, dplyr::bind_rows(team_data))
#     }

#     # Headers used to visit ncaa stats webiste and avoid blocking
#     headers <- httr::add_headers(
#         `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.6 Safari/605.1.15",
#         `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
#         `Accept-Language` = "en-US,en;q=0.9",
#         `Referer` = "https://stats.ncaa.org/",
#         `Connection` = "keep-alive"
#     )

#     # Init blank dataframe to hold NCAA ids
#     ncaa_ids <- data.frame()
#     # Loop through each division
#     for (link in config$LINKS$NCAA_IDS) {
#         page_content <- download_fromHTML(link, "data/raw", FALSE, headers)
#         # Extract all <a> tags with class="skipMask" (team links)
#         team_links <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_TEAM_REF)
#         # Add ids to a dataframe
#         ids <- data.frame(
#             team_name = team_links %>% rvest::html_text() %>% gsub("\\([^)]*\\)$", "", .) %>% trimws(),
#             ncaa_id = team_links %>% rvest::html_attr("href") %>% gsub("/teams/", "", .),
#             stringsAsFactors = FALSE
#         )
#         # Add ids to total id dataset
#         ncaa_ids <- rbind(ncaa_ids, ids)
#     }

#     # Combine ncaa ids by matching school names
#     college_ncaa_teams <- merge(college_ncaa_teams, ncaa_ids,
#     by.x = "school_name", by.y = "team_name", all.x = TRUE, all.y = FALSE) %>%
#     mutate(ncaa_id = as.character(ncaa_id))

#      # Helper function to handle NA values in data
#     `%||%` <- function(x, y) if (length(x) > 0) x else y

#     #' Processes raw NCAA team html data into structured dataframe
#     #'
#     #' @param team_url url to nfl team information
#     #'
#     scrape_ncaa_team_data <- function(team_url) {
#         # Download page content from url
#         page_content <- download_fromHTML(team_url)
#         # Extract Conference Text
#         conference <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_DETAILED$CONFERENCE) %>% rvest::html_text()
#         # Extract Nickname Text
#         nickname <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_DETAILED$NICKNAME) %>% rvest::html_text()
#         # Extract Colors Text
#         colors <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_DETAILED$COLORS) %>% rvest::html_text()
#         # Extract School Name Text
#         school_name <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_DETAILED$SCHOOL) %>% rvest::html_text()
#         # Extract Website
#         website <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_DETAILED$WEBSITE) %>% rvest::html_text(trim = TRUE)
#         # Extract Twitter handle
#         twitter <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_DETAILED$TWITTER) %>% rvest::html_text(trim = TRUE)
#         # Return dataframe with parsed information
#         data.frame(
#             conference = conference %||% NA_character_,
#             nickname = nickname %||% NA_character_,
#             colors = colors %||% NA_character_,
#             name = school_name %||% NA_character_,
#             website = website %||% NA_character_,
#             twitter = twitter %||% NA_character_,
#             stringsAsFactors = FALSE
#         )
#     }

#     # Scrape each teams detailed data (Takes a while to load)
#     college_ncaa_teams <- college_ncaa_teams %>%
#     dplyr::mutate(scraped_data = purrr::map(school_url, ~ {
#         scrape_ncaa_team_data(.x)
#     })) %>%
#     tidyr::unnest(scraped_data) %>%
#     select(ncaa_id, school_name, nickname, name) %>%
#     rename(nickname_ncaa = nickname, name_ncaa = name)

#     data <- c(college_ncaa_teams, college_espn_teams)
# }


# matching_model <- readRDS("training/xgb_model.rds")
# # Read the new baseball bindings and add the sport column
# baseball_bindings <- read.csv("data/bindings/ncaa_espn_football_bindings.csv") %>% mutate(sport = "football") %>% mutate(across(c(espn_id, ncaa_id), as.character))
# # Filter the existing model bindings for basketball and football
# bindings <- matching_model$sport_bindings %>% filter(sport == "basketball" | sport == "baseball")
# # Combine the existing basketball and football bindings with the new baseball bindings
# matching_model$sport_bindings <- bind_rows(bindings, baseball_bindings)
# # Save the updated model with the new bindings
# saveRDS(matching_model, "training/xgb_model.rds")

# Generate all possible pairs
espn_data <- read.csv(paste0("training/football_espn_teams.csv"), stringsAsFactors = FALSE)
ncaa_data <- read.csv(paste0("training/football_ncaa_teams.csv"), stringsAsFactors = FALSE)

# Extract bindings for previous matched data devided by sport name
sport_bindings <- matching_model$sport_bindings %>% filter(sport == "football") %>% select(espn_id, ncaa_id)
# Combine previous matched data onto df
combined_espn <- espn_data %>% filter(espn_id %in% sport_bindings$espn_id) %>% mutate(across(c(espn_id), as.character))
combined_ncaa <- ncaa_data %>% filter(ncaa_id %in% sport_bindings$ncaa_id) %>% mutate(across(c(ncaa_id), as.character))
combined_data <- combined_espn %>%
    inner_join(sport_bindings, by = "espn_id") %>%
    inner_join(combined_ncaa, by = "ncaa_id")

# Only pass unbounded espn and ncaa data into model
unbound_espn <- espn_data %>% filter(!espn_id %in% sport_bindings$espn_id)
unbound_ncaa <- ncaa_data %>% filter(!ncaa_id %in% sport_bindings$ncaa_id)
# Generare features for unbounded data and drop NA values
candidate_pairs <- unbound_espn %>%
    cross_join(unbound_ncaa) %>%
    mutate(
        name_jw = 1 - stringdist(displayName, school_name, method = "jw"),
        name_osa = 1 - stringdist(displayName, school_name, method = "osa"),
        location_in_name = as.numeric(str_detect(name_ncaa, fixed(location))),
        nickname_in_name = as.numeric(str_detect(displayName, fixed(nickname_ncaa))),
        name_length_diff = abs(nchar(displayName) - nchar(school_name)),
        first_word_match = as.numeric(word(displayName, 1) == word(school_name, 1)),
        last_word_match = as.numeric(word(displayName, -1) == word(school_name, -1))
    ) %>%
    tidyr::drop_na()
# Track original columns to add back later
original_cols <- candidate_pairs %>% select(-c(name_jw, name_osa, location_in_name, nickname_in_name, name_length_diff, first_word_match, last_word_match))
# Only output generate features to model
model_features <- candidate_pairs %>% select(name_jw, name_osa, location_in_name, nickname_in_name, name_length_diff, first_word_match, last_word_match)
# Predict probability on all unmatched data
model_features$pred_prob <- predict(matching_model, newdata = model_features, type = "prob")$yes
# Add origibal columns to matched data filtering by best result above threshold
final_output <- bind_cols(original_cols, model_features) %>%
    group_by(ncaa_id) %>%
    slice_max(pred_prob, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    filter(pred_prob >= 4.58e-05)
# Remove features and mutate id columns for join
final_output <- final_output %>% 
    select(-c(pred_prob, name_jw, name_osa, location_in_name, nickname_in_name, name_length_diff, first_word_match, last_word_match)) %>% 
    mutate(across(c(espn_id, ncaa_id), as.character))
# Get new bindings for model generated output 
new_bindings <- final_output %>% select(espn_id, ncaa_id) %>% mutate(sport = "football")
# Add newly generated bindings to model hardcoded bindings for later use
matching_model$sport_bindings <- bind_rows(matching_model$sport_bindings, new_bindings) %>%
  distinct(espn_id, ncaa_id, sport, .keep_all = TRUE)
# Save updated model
saveRDS(matching_model, "training/xgb_model.rds")
# Join newly matched data with previous bindings
final_combined_output <- bind_rows(combined_data, final_output)

unmatched_ncaa <- ncaa_data %>% filter(!ncaa_id %in% final_combined_output$ncaa_id)
unmatched_espn <- espn_data %>%filter(!espn_id %in% final_combined_output$espn_id)
# Save unmatched data to output folder
write.csv(unmatched_ncaa, "output/unmatched_ncaa.csv", row.names = FALSE)
write.csv(unmatched_espn, "output/unmatched_espn.csv", row.names = FALSE)
write.csv(final_combined_output, "output/canidate_pairs.csv", row.names = FALSE)