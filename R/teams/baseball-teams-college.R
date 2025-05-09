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

library(yaml, quietly = TRUE, warn.conflicts = FALSE) # Load yaml configiugration into program
library(rvest, quietly = TRUE, warn.conflicts = FALSE) # Filter through and parse html objects
library(tidyr, quietly = TRUE, warn.conflicts = FALSE) # Unest list attributed to columns in dataframe
library(purrr, quietly = TRUE, warn.conflicts = FALSE)  # Map functions to values in dataframe
library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / Management of dataframes
library(Matrix, quietly = TRUE, warn.conflicts = FALSE) # Used by fastlink
library(dotenv, quietly = TRUE, warn.conflicts = FALSE) # Get env variables
library(stringdist, quietly = TRUE, warn.conflicts = FALSE) # Used by fastlink

# Read configuration from configs directory
config <- yaml::read_yaml("configs/baseball-college.yaml")
# File to hold formated data
all_teams_file <- "data/processed/baseball-teams-college.csv"

#' College Baseball Teams
#'
#' Retrieves college baseball team data from ESPN's API and supplements
#' it with additional information scraped from NCAA and Wiki. The combined
#' data is processed into a structured dataframe and saved to a CSV file.
#'
#' @values ../../output/tables/college_baseball_missing_data.png
#'
#' @source https://site.api.espn.com/
#' @source https://www.ncaa.com/stats/basketball-men/
#' @source https://en.wikipedia.org/wiki/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param save Logical indicating weather to save data to data/processed folder
#'
#' @return A dataframe containing the following information for each basketball team
#'  id [int] - A generated unique identifier for each team
#'  espn_id [string] - id used be espn to identify team
#'  ncaa_id [string] - id used be ncaa to identify team
#'  type [string] - Always set to NCAAB for team type
#'  slug [string] - Slug used to identify teams
#'  abv [string] - Abreviation of team name (ex. TOW)
#'  full_name [string] - Full name of team (ex. Towson Tigers)
#'  short_name [string] - Short name of team (ex. Tigers)
#'  university [string] - University team is located at (ex. Towson)
#'  division [string] - Division team is associated with (ex. I)
#'  conference [string] - Conference team is associated with (ex. Big West)
#'  primary [string] - Primary color of team uniforms in Hex format
#'  secondary [string] - Secondary color of team uniforms in Hex format
#'  logo [string] - Link to logo image from ESPN
#'  head_coach [string] - Current head coach of team
#'  school_url [string] - NCAA url for team
#'  website [string] - Website url for teams school
#'  twitter [string] - Twitter handle of team starting with '@'
#'  venue [string] - Current venue where team plays
#'
get_formated_data <- function(verbose = TRUE, save = TRUE) {
    # Grab College Baskteball data from ESPN
    college_espn_teams <- download_fromJSON(config$LINKS$ESPN_TEAMS, force_refresh = TRUE, simplifyDataFrame = FALSE)
    if (verbose) cat(paste0("\n\033[32mDownloading ESPN Baseball Teams: ", config$LINKS$ESPN_TEAMS, "\033[0m"))

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
    # Run extract teams function on all teams in data and rename id
    college_espn_teams <- dplyr::bind_rows(lapply(espn_teams_list, extract_team)) %>% rename(espn_id = id)

    if (verbose) cat(paste0("\n\033[32mDownloading NCAA Baseball Teams: https://www.ncaa.com/stats/baskteball/...\033[0m"))
    # Dataframe to hold all parsed NCAA teams
    college_ncaa_teams <- data.frame()
    # Loop through each division
    for (link in config$LINKS$NCAA_TEAMS) {
        # Get division by index
        division <- sub("^.*/baseball/([^/]+).*", "\\1", link)
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
        page_content <- download_fromHTML(link, "data/raw", TRUE, headers)
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
    by.x = "school_name", by.y = "team_name", all.x = TRUE, all.y = FALSE)

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
            nickname_ncaa = nickname %||% NA_character_,
            colors = colors %||% NA_character_,
            name_ncaa = school_name %||% NA_character_,
            website = website %||% NA_character_,
            twitter = twitter %||% NA_character_,
            stringsAsFactors = FALSE
        )
    }

    if (verbose) cat(paste0("\n\033[32mDownloading NCAA Team Details: https://www.ncaa.com/schools/...\033[0m"))
    # Scrape each teams detailed data (Takes a while to load)
    college_ncaa_teams <- college_ncaa_teams %>% dplyr::mutate(scraped_data = purrr::map(school_url, ~ {
        scrape_ncaa_team_data(.x)
    })) %>%
    tidyr::unnest(scraped_data)
    
    matching_model <- readRDS("data/models/espn-ncaa-binding-model.rds")
    # Extract bindings for previous matched data devided by sport name
    sport_bindings <- matching_model$sport_bindings %>% dplyr::filter(sport == "baseball") %>% select(espn_id, ncaa_id)
    # Combine previous matched data onto df
    combined_espn <- college_espn_teams %>% dplyr::filter(espn_id %in% sport_bindings$espn_id) %>% dplyr::mutate(dplyr::across(c(espn_id), as.character))
    combined_ncaa <- college_ncaa_teams %>% dplyr::filter(ncaa_id %in% sport_bindings$ncaa_id) %>% dplyr::mutate(dplyr::across(c(ncaa_id), as.character))
    combined_data <- combined_espn %>%
        dplyr::inner_join(sport_bindings, by = "espn_id") %>%
        dplyr::inner_join(combined_ncaa, by = "ncaa_id")

    # Only pass unbounded espn and ncaa data into model
    unbound_espn <- college_espn_teams %>% dplyr::filter(!espn_id %in% sport_bindings$espn_id)
    unbound_ncaa <- college_ncaa_teams %>% dplyr::filter(!ncaa_id %in% sport_bindings$ncaa_id)
    # Generare features for unbounded data and drop NA values
    candidate_pairs <- unbound_espn %>%
        dplyr:::cross_join(unbound_ncaa) %>%
        dplyr:::mutate(
            name_jw = coalesce(1 - stringdist::stringdist(displayName, school_name, method = "jw"), 0),
            name_osa = coalesce(1 - stringdist::stringdist(displayName, school_name, method = "osa"), 0),
            location_in_name = suppressWarnings(coalesce(as.numeric(str_detect(name_ncaa, fixed(location))), 0)),
            nickname_in_name = suppressWarnings(coalesce(as.numeric(str_detect(displayName, fixed(nickname_ncaa))), 0)),
            name_length_diff = coalesce(abs(nchar(displayName) - nchar(school_name)), 0),
            first_word_match = coalesce(as.numeric(word(displayName, 1) == word(school_name, 1)), 0),
            last_word_match = coalesce(as.numeric(word(displayName, -1) == word(school_name, -1)), 0)
        ) %>%
        tidyr::drop_na()
    # Track original columns to add back later
    original_cols <- candidate_pairs %>% dplyr::select(-c(name_jw, name_osa, location_in_name, nickname_in_name, name_length_diff, first_word_match, last_word_match))
    # Only output generate features to model
    model_features <- candidate_pairs %>% dplyr::select(name_jw, name_osa, location_in_name, nickname_in_name, name_length_diff, first_word_match, last_word_match)
    # Predict probability on all unmatched data
    model_features$pred_prob <- suppressWarnings(predict(matching_model, newdata = model_features, type = "prob")$yes)
    # Add origibal columns to matched data filtering by best result above threshold
    final_output <- dplyr::bind_cols(original_cols, model_features) %>%
        dplyr::group_by(ncaa_id) %>%
        dplyr::slice_max(pred_prob, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::filter(pred_prob >= 4.58e-05)
    # Remove features and mutate id columns for join
    final_output <- final_output %>%
        dplyr::select(-c(pred_prob, name_jw, name_osa, location_in_name, nickname_in_name, name_length_diff, first_word_match, last_word_match)) %>%
        dplyr::mutate(dplyr::across(c(espn_id, ncaa_id), as.character))
    # Get new bindings for model generated output
    new_bindings <- final_output %>% dplyr::select(espn_id, ncaa_id) %>% dplyr::mutate(sport = "baseball")
    # Add newly generated bindings to model hardcoded bindings for later use
    matching_model$sport_bindings <- dplyr::bind_rows(matching_model$sport_bindings, new_bindings) %>%
    dplyr::distinct(espn_id, ncaa_id, sport, .keep_all = TRUE)
    # Save updated model
    saveRDS(matching_model, "data/models/espn-ncaa-binding-model.rds")
    # Join newly matched data with previous bindings
    all_college_data <- dplyr::bind_rows(combined_data, final_output)
    # Get unmatched data from both sources
    unbounded_ncaa_data <- college_ncaa_teams %>% dplyr::filter(!ncaa_id %in% all_college_data$ncaa_id)
    unbounded_espn_data <- college_espn_teams %>% dplyr::filter(!espn_id %in% all_college_data$espn_id)

    # Reformat combined data into standard format and only select relevant columns
    all_college_data <- all_college_data %>%
        # Rename columns to standardize
        dplyr::rename(
            abv = abbreviation,
            full_name = displayName,
            short_name = name,
            university = name_ncaa,
            primary = color,
            secondary = alternateColor,
        ) %>%
        # Create a type column to identify sport and venue column for later
        dplyr::mutate(type = "NCAAF", venue = NA_character_, head_coach = NA_character_) %>%
        # Set values based on Na counditions for NCAA teams
        dplyr::mutate(university = university %||% location) %>%
        # Create a unique id for each team
        dplyr::mutate(id = encode_id(paste0("F", espn_id), abv)) %>%
        # Select only relevant columns
        dplyr::select(id, espn_id, ncaa_id, type, slug, abv, full_name, short_name, university,
            division, conference, primary, secondary, logo, head_coach, img_src, school_url, website, twitter, venue)

    # Read bindings from repository
    color_bindings <- read.csv("https://github.com/cullenchampagne1/sportsR/releases/download/misc/ncaa_logo_color_bindings.csv")

    #' Processes and svg logo and returns top 2 colors
    #'
    #' @param svg_url url to svg
    #'
    get_dominant_colors <- function(svg_url) {
        # Check if we already have these colors
        if (svg_url %in% color_bindings$url) {
            cached <- color_bindings %>% dplyr::filter(url == !!svg_url) %>% dplyr::pull(colors)
            if (!is.na(cached)) return(cached)
        }
        # If not proceed to download svg and generate color codes
        colors <- tryCatch({
            # Download and convert SVG to raster
            raw_svg <- httr::content(httr::GET(svg_url), as = "raw")
            png_bytes <- rsvg::rsvg_png(raw_svg)
            img_array <- png::readPNG(png_bytes)
            # Reshape to RGB matrix (excluding alpha if exists)
            if (dim(img_array)[3] == 4) rgb_matrix <- img_array[, , 1:3]
            else  rgb_matrix <- img_array
            # Convert to hex and count frequencies
            hex_colors <- apply(rgb_matrix, 1:2, function(pixel) {
                rgb(pixel[1], pixel[2], pixel[3])
            })
            # Filter and count colors (exclude white/black/transparent)
            color_counts <- table(hex_colors) %>%
            as.data.frame() %>%
            dplyr::filter(!hex_colors %in% c("#FFFFFF", "#000000", "#00000000")) %>%
            dplyr::arrange(dplyr::desc(Freq))
            # Return top 2 colors
            if (nrow(color_counts) >= 2) result <- paste(color_counts$hex_colors[1:2], collapse = ", ")
            else if (nrow(color_counts) == 1) result <- as.character(color_counts$hex_colors[1])
            else result <- NA_character_

            # Update bindings with new colors
            if (!is.na(result)) color_bindings <<- color_bindings %>% dplyr::add_row(url = svg_url, colors = result)

            # Return result of nat failed
            result
        }, error = function(e) { NA_character_ })
        colors
    }

    if (verbose) cat(paste0("\n\033[32mDownloading NCAA Team Logos: https://www.ncaa.com/sites/default/files/images/logos/schools/bgl/...\033[0m"))
    all_college_data <- all_college_data %>%
        # Replace logo url when avalaible
        dplyr::mutate(logo = if_else(is.na(img_src), logo, img_src)) %>%
        # Get primary and secondary colors from logo when not avalaible through espn
        dplyr::mutate(dominant_colors = purrr::map_chr(img_src, get_dominant_colors)) %>%
        dplyr::mutate(
            temp_colors = str_split_fixed(dominant_colors, ", ", 2),
            primary = ifelse(
                !is.na(dominant_colors) & (is.na(primary) | primary == "000000"),
                temp_colors[, 1],
                primary
            ),
            secondary = ifelse(
                !is.na(dominant_colors) & (is.na(secondary) | secondary == "000000"),
                temp_colors[, 2],
                secondary
            )
        ) %>%
        dplyr::mutate(primary = toupper(primary), secondary = toupper(secondary)) %>%
        # Add a stanard # before color values if isnt already there and value isnt NA
        dplyr::mutate(dplyr::across(c(primary, secondary), ~ ifelse(!is.na(.) & !str_starts(., "#"), paste0("#", .), .))) %>%
        # Standard website format
        mutate(website = {
            # Remove trailing slashes/question marks and whitespace
            urls <- trimws(website)
            urls <- sub("[/?]+$", "", urls)
            # Add https:// if no protocol exists
            ifelse(grepl("^https?://", urls, ignore.case = TRUE), urls, paste0("https://", urls))
        }) %>%
        # Removed uneeded columns
        dplyr::select(-img_src, -temp_colors, -dominant_colors)

    #' Processes ncaa webpages and extract head coach
    #'
    #' @param ncaa_id id of ncaa team
    #'
    get_coach_name <- function(ncaa_id) {
        # Create url from ncaa_id
        url <- paste0("https://stats.ncaa.org/teams/", ncaa_id)
        # Download page content from url
        page_content <- download_fromHTML(url, "data/raw", TRUE, headers)
        # Get coach from xpath
        coach_name <- page_content %>% html_element(xpath = config$ATTRIBUTES$NCAA_STAT$HEAD_COACH) %>% html_text(trim = TRUE)
    }

    if (verbose) cat(paste0("\n\033[32mDownloading Additional NCAA Coaches: https://stats.ncaa.org/team/...\n\033[0m"))
    # Filter and retrieve unknown coaches from ncaa website
    unknown_coaches <- all_college_data %>% dplyr::filter(is.na(head_coach)) %>% dplyr::select(ncaa_id, head_coach)
    updated_coaches <- unknown_coaches %>% mutate(head_coach = map_chr(ncaa_id, ~{ get_coach_name(.x) }))
    # Update coaches back into dataset
    all_college_data <- all_college_data %>% dplyr::rows_update(updated_coaches, by = "ncaa_id")

    #' Processes ncaa webpages and extract venues
    #'
    #' @param ncaa_id id of ncaa team
    #'
    get_venue <- function(ncaa_id) {
        # Create url from ncaa_id
        url <- paste0("https://stats.ncaa.org/teams/", ncaa_id)
        # Download page content from url
        page_content <- download_fromHTML(url, "data/raw", FALSE, headers)
        # Get coach from xpath
        venue <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$NCAA_STAT$VENUE) %>% rvest::html_text(trim = TRUE)
    }

    if (verbose) cat(paste0("\n\033[32mDownloading Additional NCAA Venues: https://stats.ncaa.org/team/...\n\033[0m"))
    # Filter and retrieve unknown coaches from ncaa website
    unknown_venues <- all_college_data %>% dplyr::filter(is.na(venue)) %>% dplyr::select(ncaa_id, venue)
    updated_venues <- unknown_venues %>% dplyr::mutate(venue = map_chr(ncaa_id, ~{ get_venue(.x) }))
    # Update coaches back into dataset
    all_college_data <- all_college_data %>% dplyr::rows_update(updated_venues, by = "ncaa_id")

    # Move head coach to after logo
    all_college_data <- all_college_data %>% relocate(head_coach, .after = logo)

    # Analyze missing data
    analyze_missing_data("College Baseball", all_college_data)
    process_markdown_file("R/teams/baseball-teams-college.R", "R/teams/readme.md", nrow(all_college_data))

    if (verbose) cat(paste0("\n\033[90m", nrow(unbounded_ncaa_data), " NCAA Teams and ", nrow(unbounded_espn_data), " ESPN Teams Could Not be Binded: /output/csv/unmatched_...\033[0m"))
    if (verbose) cat(paste0("\n\033[90mCollege Baseball Data Saved To: /", all_teams_file, "\033[0m\n"))

    # Save any created name bindings to file
    if (save) write.csv(all_college_data, all_teams_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(all_college_data, sub("\\.csv$", ".rds", all_teams_file))
    # Return formated data
    return(all_college_data)

}

# If file is being run stand-alone, run function
invisible(get_formated_data())