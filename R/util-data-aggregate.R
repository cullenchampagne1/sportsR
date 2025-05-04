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

get_formated_data <- function(sport) {
    config <- yaml::read_yaml(paste0("configs/", sport, "_college.yaml"))
    bindings <- paste0("data/bindings/ncaa_espn_", sport, "_bindings.csv")

    # Grab College baseball data from ESPN
    college_espn_teams <- download_fromJSON(config$LINKS$ESPN_TEAMS, force_refresh = FALSE, simplifyDataFrame = FALSE)

    # Extract all teams from ESPN json structure
    espn_teams_list <- lapply(college_espn_teams$sports[[1]]$leagues[[1]]$teams, function(x) x$team)

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
    college_espn_teams <- dplyr::bind_rows(lapply(espn_teams_list, extract_team)) %>%
    mutate(id = as.character(id)) %>%
    rename(espn_id = id) %>%
    select(espn_id, slug, abbreviation, displayName, shortDisplayName, name,	nickname, location)

    # Dataframe to hold all parsed NCAA teams
    college_ncaa_teams <- data.frame()
    # Loop through each division
    for (link in config$LINKS$NCAA_TEAMS) {
        # Get division by index
        division <- sub(paste0("^.*/", sport, "/([^/]+).*"), "\\1", link)
        # Download first page of current division teams
        page_content <- download_fromHTML(link, force_refresh = TRUE)
        # Initialize an empty list to store data
        team_data <- list()
        # Get all rows on the current page
        current_team_rows <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_TEAMS$ROWS)
        # Loop through each row
        for (row in current_team_rows) {
            team_columns <- row %>% rvest::html_elements(config$ATTRIBUTES$NCAA_TEAMS$COLUMNS)
            if (length(team_columns) < 2) next
            img_src <- team_columns[config$ATTRIBUTES$NCAA_TEAMS$COLUMN_NUMBER] %>%
                rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_TEAMS$IMG_SRC) %>%
                rvest::html_text(trim = TRUE)
            school_url <- team_columns[config$ATTRIBUTES$NCAA_TEAMS$COLUMN_NUMBER] %>%
                rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_TEAMS$SCHOOL_URL) %>%
                rvest::html_text(trim = TRUE)
            school_name <- team_columns[config$ATTRIBUTES$NCAA_TEAMS$COLUMN_NUMBER] %>%
                rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_TEAMS$SCHOOL_NAME) %>%
                rvest::html_text(trim = TRUE)
            team_data[[length(team_data) + 1]] <- data.frame(
                school_name = school_name,
                division = division,
                img_src = img_src,
                school_url = paste0("https://www.ncaa.com", school_url),
                stringsAsFactors = FALSE
            )
        }
            # Add team to data frame
        college_ncaa_teams <- rbind(college_ncaa_teams, dplyr::bind_rows(team_data))
    }

    # Headers used to visit ncaa stats webiste and avoid blocking
    headers <- httr::add_headers(
        `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.6 Safari/605.1.15",
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `Accept-Language` = "en-US,en;q=0.9",
        `Referer` = "https://stats.ncaa.org/",
        `Connection` = "keep-alive"
    )

    # Init blank dataframe to hold NCAA ids
    ncaa_ids <- data.frame()
    # Loop through each division
    for (link in config$LINKS$NCAA_IDS) {
        page_content <- download_fromHTML(link, "data/raw", FALSE, headers)
        # Extract all <a> tags with class="skipMask" (team links)
        team_links <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_TEAM_REF)
        # Add ids to a dataframe
        ids <- data.frame(
            team_name = team_links %>% rvest::html_text() %>% gsub("\\([^)]*\\)$", "", .) %>% trimws(),
            ncaa_id = team_links %>% rvest::html_attr("href") %>% gsub("/teams/", "", .),
            stringsAsFactors = FALSE
        )
        # Add ids to total id dataset
        ncaa_ids <- rbind(ncaa_ids, ids)
    }

    # Combine ncaa ids by matching school names
    college_ncaa_teams <- merge(college_ncaa_teams, ncaa_ids,
    by.x = "school_name", by.y = "team_name", all.x = TRUE, all.y = FALSE) %>%
    mutate(ncaa_id = as.character(ncaa_id))

     # Helper function to handle NA values in data
    `%||%` <- function(x, y) if (length(x) > 0) x else y

    #' Processes raw NCAA team html data into structured dataframe
    #'
    #' @param team_url url to nfl team information
    #'
    scrape_ncaa_team_data <- function(team_url) {
        # Download page content from url
        page_content <- download_fromHTML(team_url)
        # Extract Conference Text
        conference <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_DETAILED$CONFERENCE) %>% rvest::html_text()
        # Extract Nickname Text
        nickname <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_DETAILED$NICKNAME) %>% rvest::html_text()
        # Extract Colors Text
        colors <- page_content %>% rvest::html_elements(xpath = config$ATTRIBUTES$NCAA_DETAILED$COLORS) %>% rvest::html_text()
        # Extract School Name Text
        school_name <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_DETAILED$SCHOOL) %>% rvest::html_text()
        # Extract Website
        website <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_DETAILED$WEBSITE) %>% rvest::html_text(trim = TRUE)
        # Extract Twitter handle
        twitter <- page_content %>% rvest::html_elements(config$ATTRIBUTES$NCAA_DETAILED$TWITTER) %>% rvest::html_text(trim = TRUE)
        # Return dataframe with parsed information
        data.frame(
            conference = conference %||% NA_character_,
            nickname = nickname %||% NA_character_,
            colors = colors %||% NA_character_,
            name = school_name %||% NA_character_,
            website = website %||% NA_character_,
            twitter = twitter %||% NA_character_,
            stringsAsFactors = FALSE
        )
    }

    # Scrape each teams detailed data (Takes a while to load)
    college_ncaa_teams <- college_ncaa_teams %>%
    dplyr::mutate(scraped_data = purrr::map(school_url, ~ {
        scrape_ncaa_team_data(.x)
    })) %>%
    tidyr::unnest(scraped_data) %>%
    select(ncaa_id, school_name, nickname, name) %>%
    rename(nickname_ncaa = nickname, name_ncaa = name)
    
    bindings <- read.csv(bindings, stringsAsFactors = FALSE) %>% mutate(ncaa_id = as.character(ncaa_id)) %>% mutate(espn_id = as.character(espn_id))

    combined_data <- college_espn_teams %>%
    # Join ESPN data with bindings using ESPN ID
    inner_join(bindings, by = c("espn_id" = "espn_id")) %>%
    # Join result with NCAA data using NCAA ID
    inner_join(college_ncaa_teams, by = c("ncaa_id" = "ncaa_id"))
}

football_espn <- read.csv("training/football_espn_teams.csv", stringsAsFactors = FALSE) %>% select(id, displayName, nickname, location, abbreviation) %>% rename(espn_id = id)
football_ncaa <- read.csv("training/football_ncaa_teams.csv", stringsAsFactors = FALSE) %>% select(ncaa_id, school_name, nickname_ncaa, name_ncaa)

football_espn <- football_espn %>% cross_join(football_ncaa)

football_espn <- football_espn %>%
  mutate(
    sport = "football",
    name_jw = 1 - stringdist(displayName, school_name, method = "jw"),
    name_osa = 1 - stringdist(displayName, school_name, method = "osa"),
    nickname_match = as.numeric(nickname == nickname_ncaa),
    location_in_name = as.numeric(str_detect(name_ncaa, fixed(location))),
    nickname_in_name = as.numeric(str_detect(displayName, fixed(nickname_ncaa))),
    name_length_diff = abs(nchar(displayName) - nchar(school_name)),
    first_word_match = as.numeric(word(displayName, 1) == word(school_name, 1)),
    last_word_match = as.numeric(word(displayName, -1) == word(school_name, -1)),
  ) %>%
  # Remove rows with NA in ANY feature used by the model
  tidyr::drop_na()  # This replaces your original filter

model <- readRDS("training/xgb_model.rds")
predictions <- predict(model, newdata = football_espn)

# Now football_espn and predictions will have the same number of rows
football_espn <- football_espn %>%
  mutate(prediction = predictions) %>%
  filter(prediction == 1)

write.csv(football_espn, "training/football_espn_ncaa_combined.csv", row.names = FALSE)

# football_data <- get_formated_data("football")
# basketball_data <- get_formated_data("basketball")
# baseball_data <- get_formated_data("baseball")

# # Combine the three data frames into one
# all_sports_data <- dplyr::bind_rows(
#     football_data %>% dplyr::mutate(sport = "football"),
#     basketball_data %>% dplyr::mutate(sport = "basketball"),
#     baseball_data %>% dplyr::mutate(sport = "baseball")
# )

# all_sports_data <- all_sports_data %>%
#   mutate(across(c(displayName, school_name, nickname, nickname_ncaa, location, name),
#                 ~str_remove_all(., "\\.|University|College") %>% tolower()))

# # POSITIVE PAIRS
# all_sports_data_positive <- all_sports_data %>%
#   mutate(
#     name_jw = 1 - stringdist(displayName, school_name, method = "jw"),
#     name_osa = 1 - stringdist(displayName, school_name, method = "osa"),
#     location_in_name = as.numeric(str_detect(name, fixed(location))),
#     nickname_in_name = as.numeric(str_detect(displayName, fixed(nickname_ncaa))),
#     name_length_diff = abs(nchar(displayName) - nchar(school_name)),
#     first_word_match = as.numeric(word(displayName, 1) == word(school_name, 1)),
#     last_word_match = as.numeric(word(displayName, -1) == word(school_name, -1)),
#     label = 1
#   )

# # NEGATIVE PAIRS
# ncaa_info <- all_sports_data %>% select(sport, ncaa_id, school_name, nickname_ncaa, name_ncaa)
# espn_info <- all_sports_data %>% select(sport, espn_id, displayName, nickname, location, abbreviation)

# set.seed(42)
# negative_pairs <- ncaa_info %>%
#   group_by(sport) %>%
#   mutate(tmp_id = sample(n())) %>%
#   ungroup() %>%
#   rename_with(~ paste0(., "_ncaa"), -c(sport, tmp_id)) %>%
#   inner_join(
#     espn_info %>%
#       group_by(sport) %>%
#       mutate(tmp_id = sample(n())) %>%
#       ungroup(),
#     by = c("sport", "tmp_id")
#   ) %>%
#   filter(ncaa_id_ncaa != espn_id) %>%
#   select(-tmp_id)

# message(names(negative_pairs))

# all_sports_data_negative <- negative_pairs %>%
#   mutate(
#     name_jw = 1 - stringdist(displayName, school_name_ncaa, method = "jw"),
#     name_osa = 1 - stringdist(displayName, school_name_ncaa, method = "osa"),
#     location_in_name = as.numeric(str_detect(name_ncaa_ncaa, fixed(location))),
#     nickname_in_name = as.numeric(str_detect(displayName, fixed(nickname_ncaa_ncaa))),
#     name_length_diff = abs(nchar(displayName) - nchar(school_name_ncaa)),
#     first_word_match = as.numeric(word(displayName, 1) == word(school_name_ncaa, 1)),
#     last_word_match = as.numeric(word(displayName, -1) == word(school_name_ncaa, -1)),
#     label = 0
#   ) %>%
#   rename(
#     espn_id = espn_id,
#     ncaa_id = ncaa_id_ncaa,
#     school_name = school_name_ncaa,
#     nickname_ncaa = nickname_ncaa_ncaa
#   ) %>%
#   select(espn_id, ncaa_id, sport, name_jw, name_osa, nickname_in_name,
#          location_in_name, name_length_diff, first_word_match, last_word_match,
#          label)

# # COMBINE & FILTER
# all_sports_data <- bind_rows(
#     all_sports_data_positive %>%
#       select(espn_id, ncaa_id, sport, name_jw, name_osa, nickname_in_name,
#              location_in_name, name_length_diff, first_word_match, last_word_match,
#              label),
#     all_sports_data_negative
#   ) %>%
#   distinct() %>%
#   filter(!is.na(name_jw), !is.na(nickname_in_name),
#          !is.na(location_in_name),
#          !is.na(name_length_diff),
#          !is.na(first_word_match),
#          !is.na(last_word_match))



# # Save the combined data to a CSV file
# write.csv(all_sports_data, "training/all_sports_data.csv", row.names = FALSE)

# set.seed(42)
# train_index <- createDataPartition(all_sports_data$label, p = 0.8, list = FALSE)
# train_data <- all_sports_data[train_index, ]
# test_data <- all_sports_data[-train_index, ]

# # Train a model
# model <- train(
#   factor(label) ~ .,
#   data = train_data %>% select(-espn_id, -ncaa_id),
#   method = "xgbTree",
#   trControl = trainControl(method = "cv", number = 5)
# )
# saveRDS(model, file = "training/xgb_model.rds")
# # Evaluate
# predictions <- predict(model, newdata = test_data)
# cm <- confusionMatrix(predictions, factor(test_data$label))

# print(cm)


# name_jw = 1 - stringdist(displayName, school_name, method = "jw"),
# name_osa = 1 - stringdist(displayName, school_name, method = "osa"),
# nickname_match = as.numeric(nickname == nickname_ncaa),
# location_in_name = as.numeric(str_detect(school_name, fixed(location))),
# name_length_diff = abs(nchar(displayName) - nchar(school_name)),
# first_word_match = as.numeric(word(displayName, 1) == word(school_name, 1)),
# last_word_match = as.numeric(word(displayName, -1) == word(school_name, -1)),
# initials_school = str_c(str_extract_all(school_name, "\\b\\w")[[1]], collapse = ""),
# abbreviation_match = as.numeric(tolower(abbreviation) == tolower(initials_school)),