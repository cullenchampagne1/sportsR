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
library(purrr, quietly = TRUE, warn.conflicts = FALSE)  # Map functions to values in dataframe
library(fastLink, quietly = TRUE, warn.conflicts = FALSE) # Weighted dataset matching for NCAA and ESPN
library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / Management of dataframes
library(httr, quietly = TRUE, warn.conflicts = FALSE) # Header management for downloading data
library(yaml, quietly = TRUE, warn.conflicts = FALSE) # Load yaml configiugration into program
library(magick, quietly = TRUE, warn.conflicts = FALSE) # Working with svgs from color data
library(stringdist, quietly = TRUE, warn.conflicts = FALSE) # Used by fastlink
library(Matrix, quietly = TRUE, warn.conflicts = FALSE) # Used by fastlink
library(dotenv, quietly = TRUE, warn.conflicts = FALSE) # Get env variables

dotenv::load_dot_env()  # Loads variables from .env into the R environment

# Read configuration from configs directory
config <- yaml::read_yaml("configs/football_college.yaml")
# API key for College Football API
college_api_key <- Sys.getenv("COLLEGE_API_KEY")
# File to hold formated data
all_teams_file <- "data/processed/football-teams-college.csv"

#' College Football Teams
#'
#' Retrieves college football team data from ESPN's API and supplements
#' it with additional information scraped from NCAA and CollegeFootballlDB. The combined
#' data is processed into a structured dataframe and saved to a CSV file.
#'
#' @source https://site.api.espn.com/
#' @source https://api.collegefootballdata.com/teams
#' @source https://www.ncaa.com/stats/football/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#'
#' @return A dataframe containing the following information for each football team
#'  id [string] - A generated unique identifier for each team
#'  espn_id [int] - id used be espn to identify team
#'  ncaa_id [string] - id used be ncaa to identify team
#'  type [string] - Always set to NCAAB for team type
#'  abv [string] - Abreviation of team name (ex. TOW)
#'  full_name [string] - Full name of team (ex. Towson Tigers)
#'  short_name [string] - Short name of team (ex. Tigers)
#'  university [string] - Uniersity team is located (ex. Towson)
#'  division [string] - Division team is associated with (ex. I)
#'  conference [string] - Conference team is associated with (ex. Big West)
#'  primary [string] - Primary color of team uniforms in Hex format
#'  secondary [string] - Secondary color of team uniforms in Hex format
#'  logo [string] - Link to logo image from ESPN
#'  head_coach [string] - Current head coach of team
#'  offensive_coordinator [string] - Current offensive coordinator of team
#'  defensive_coordinator [string] - Current defensive coordinator of team
#'  school_url [string] - NCAA url for team
#'  website [string] - Website url for teams school
#'  twitter [string] -Twitter handle of team starting with '@'
#'  venue [string] - Current venue where team plays
#'
get_formated_data <- function(verbose = TRUE) {
    # Grab College Football data from ESPN
    college_espn_teams <- download_fromJSON(config$LINKS$ESPN_TEAMS, force_refresh = TRUE, simplifyDataFrame = FALSE)
    if (verbose) cat(paste0("\n\033[32mDownloading ESPN Football Teams: ", config$LINKS$ESPN_TEAMS, "\033[0m"))
    # Grab College Football API team info
    college_cfd_teams <- download_fromJSON(config$LINKS$CFDB_TEAMS, force_refresh = TRUE, auth = paste("Bearer", college_api_key))
    if (verbose) cat(paste0("\n\033[32mDownloading CFDB Football Teams: ", config$LINKS$CFDB_TEAMS, "\033[0m"))

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
    college_espn_teams <- dplyr::bind_rows(lapply(espn_teams_list, extract_team))
    # Extract cdf logo list from list to string
    college_cfd_teams$logos <- sapply(college_cfd_teams$logos, paste, collapse = ", ")
    # Make a column for each cdf location data point
    college_cfd_teams <- tidyr::unnest_wider(college_cfd_teams, location, names_sep = ".")

    # Prefix columns with "ESPN_" and convert to uppercase
    names(college_espn_teams) <- paste0("ESPN_", toupper(names(college_espn_teams)))
    # Prefix columns with "CFD_" and convert to uppercase
    names(college_cfd_teams) <- paste0("CFD_", toupper(names(college_cfd_teams)))

    # Combine data from both locations, keeping any espn data that cant find a match while
    # Removing CFD data that isnt show on espn
    all_college_data <- merge(college_espn_teams, college_cfd_teams,
    by.x = "ESPN_ID", by.y = "CFD_ID", all.x = TRUE, all.y = FALSE)

    # Generate Unique team ids for each team
    all_college_data <- all_college_data %>%
        dplyr::mutate(id = encode_id(paste0("F", ESPN_ID), ESPN_ABBREVIATION)) %>%
        # Put uniquie id as first column in dataset and filter by only active colleges
        dplyr::select(id, dplyr::everything()) %>%
        dplyr::filter(ESPN_ISACTIVE == TRUE) %>%
        # Remove all uneeded columns
        dplyr::select(-c(ESPN_SLUG, CFD_CLASSIFICATION, CFD_COLOR, CFD_ALT_COLOR, ESPN_ROSTER, ESPN_SCHEDULE, ESPN_TICKETS,
            CFD_SCHOOL, CFD_ABBREVIATION, CFD_ALT_NAME1, CFD_ALT_NAME2, CFD_ALT_NAME3, ESPN_CLUBHOUSE, ESPN_STATISTICS,
            ESPN_ISACTIVE, ESPN_ISALLSTAR, ESPN_UID, CFD_LOGOS, CFD_LOCATION.VENUE_ID, CFD_LOCATION.CITY, CFD_LOCATION.STATE,
            CFD_LOCATION.ZIP, CFD_LOCATION.COUNTRY_CODE, CFD_LOCATION.TIMEZONE, CFD_LOCATION.LATITUDE,
            CFD_LOCATION.LONGITUDE, CFD_LOCATION.ELEVATION, CFD_LOCATION.CAPACITY, CFD_LOCATION.YEAR_CONSTRUCTED,
            CFD_LOCATION.GRASS, CFD_LOCATION.DOME, ESPN_NAME, ESPN_NICKNAME, CFD_TWITTER, CFD_CONFERENCE)) %>%
        # Rename all remaining columns
        dplyr::rename(full_name = ESPN_DISPLAYNAME, short_name = ESPN_SHORTDISPLAYNAME, primary = ESPN_COLOR,
            secondary = ESPN_ALTERNATECOLOR, logo = ESPN_LOGO, venue = CFD_LOCATION.NAME,
            abv = ESPN_ABBREVIATION, owner = ESPN_LOCATION, espn_id = ESPN_ID, nickname = CFD_MASCOT) %>%
        # Create a type column
        dplyr::mutate(type = "NCAAF")

    if (verbose) cat(paste0("\n\033[32mDownloading NCAA Coaches List: https://en.wikipedia.org/wiki/List_of_current_NCAA_Division_... \033[0m"))
    # Initialize blank dataframe for coaches
    college_football_coaches <- data.frame()
    # Loop through coach links and extract tables
    for (coach_link in config$LINKS$NCAA_COACHES) {
        # Dowload current D1 coaches for means college football
        page_content <- download_fromHTML(coach_link, force_refresh = TRUE)
        # Get table attribute from config
        table_selector <- config$ATTRIBUTES[[coach_link]]$TABLE
        # Extract arenas table from wiki page
        table <- page_content %>% rvest::html_element(xpath = table_selector) %>% rvest::html_table(fill = TRUE)
        # Select relevenant columns from dataset
        coaches <- table %>% dplyr::select(Team, "Head coach", "Offensive coordinator(s)", "Defensive coordinator(s)")
        # Add downloaded coaches to data frame
        college_football_coaches <- rbind(college_football_coaches, coaches)
    }

    # Combine both data by matching team and owner
    all_college_data <- merge(all_college_data, college_football_coaches,
    by.x = "full_name", by.y = "Team", all.x = TRUE, all.y = FALSE)

    # Reformat to standard after merge
    all_college_data <- all_college_data %>%
        # Rename collumns
        dplyr::rename(head_coach = "Head coach", offensive_coordinator = "Offensive coordinator(s)",
            defensive_coordinator = "Defensive coordinator(s)") %>%
        # Remove all external links from parsed values
        dplyr::mutate(head_coach = head_coach %>% gsub("\\[[a-z0-9]+\\]", "", .),
            offensive_coordinator = offensive_coordinator %>% gsub("\\[[a-z0-9]+\\]", "", .),
            defensive_coordinator = defensive_coordinator %>% gsub("\\[[a-z0-9]+\\]", "", .))

    if (verbose) cat(paste0("\n\033[32mDownloading NCAA Football Teams: https://www.ncaa.com/stats/football/...\033[0m"))
    # Dataframe to hold all parsed NCAA teams
    ncaa_football_teams <- data.frame()
    # Loop through each division
    for (link in config$LINKS$NCAA_TEAMS) {
        # Get division by index
        division <- sub("^.*/football/([^/]+).*", "\\1", link)
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
        ncaa_football_teams <- rbind(ncaa_football_teams, dplyr::bind_rows(team_data))
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
    ncaa_football_teams <- merge(ncaa_football_teams, ncaa_ids,
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
            nickname = nickname %||% NA_character_,
            colors = colors %||% NA_character_,
            name = school_name %||% NA_character_,
            website = website %||% NA_character_,
            twitter = twitter %||% NA_character_,
            stringsAsFactors = FALSE
        )
    }

    if (verbose) cat(paste0("\n\033[32mDownloading NCAA Team Details: https://www.ncaa.com/schools/...\033[0m"))
    # Scrape each teams detailed data (Takes a while to load)
    ncaa_football_teams <- ncaa_football_teams %>% dplyr::mutate(scraped_data = purrr::map(school_url, ~ {
        scrape_ncaa_team_data(.x)
    })) %>%
    tidyr::unnest(scraped_data) %>%
    dplyr::rename(owner = school_name, full_name = name)

    # Not all teams can be automaticaly matched, load a maual mutations file to assist with remaining teams
    mutations <- yaml::read_yaml("data/mutations/football_college_mutations.yaml")
    # List of state and university abreveations that are swapped for consistancy
    abbreviations <- yaml::read_yaml("data/mutations/university_abbreviations.yaml")$ABBREVIATIONS %>%
        unlist() %>%
        set_names(names(.))

    # Iterate through custom rules for NCAA
    for (rule in mutations$NCAA) {
        matches <- stringr::str_match(rule, "IF (\\w+) == '(.*?)' THEN (\\w+) = '(.*?)'")
        ncaa_football_teams <- ncaa_football_teams %>% dplyr::mutate(
            !!matches[4] := if_else(.data[[matches[2]]] == matches[3], matches[5], .data[[matches[4]]]))
    }
    # Iterate through custom rules for ESPN
    for (rule in mutations$ESPN) {
        matches <- stringr::str_match(rule, "IF (\\w+) == '(.*?)' THEN (\\w+) = '(.*?)'")
        all_college_data <- all_college_data %>% dplyr::mutate(
            !!matches[4] := if_else(.data[[matches[2]]] == matches[3], matches[5], .data[[matches[4]]]))
    }

    # Apply abreviation replacements to both dataframes
    all_college_data <- all_college_data %>% dplyr::mutate(dplyr::across(c(owner, full_name), ~ str_replace_all(.x, abbreviations)))
    ncaa_football_teams <- ncaa_football_teams %>% dplyr::mutate(dplyr::across(c(owner, full_name), ~ str_replace_all(.x, abbreviations)))

    sink(tempfile())
    # Use fast link to combine both datasets using string simulity over multiple columns
    matches_out <- fastLink::fastLink(
        dfA = all_college_data, dfB = ncaa_football_teams,
        varnames = c("owner", "full_name", "nickname"),
        stringdist.match = c("owner", "full_name", "nickname"),
        partial.match = c("nickname"),
        dedupe.matches = TRUE,
        threshold.match = 0.2,
        jw.weight = 0.2,
        cut.p = 0.75,
        linprog.dedupe = TRUE,
        verbose = FALSE
    )
    sink()
    # Combine data based on matches
    all_college_data_matched <- getMatches(
        dfA = all_college_data,
        dfB = ncaa_football_teams,
        fl.out = matches_out
    )

    # Go back and inherit correct columns after matching
    all_college_data_matched <- all_college_data_matched %>%
        dplyr::left_join(ncaa_football_teams %>%
        dplyr::select(ncaa_id, ncaa_owner = owner), by = "ncaa_id") %>%
        dplyr::mutate(owner = ifelse(!is.na(ncaa_owner) & nchar(ncaa_owner) > nchar(owner),  ncaa_owner, owner)) %>%
        dplyr::select(-ncaa_owner)

    # Create a bindings file for later use
    bindings <- all_college_data_matched %>% dplyr::select(ncaa_id, espn_id)
    write.csv(bindings, "data/bindings/ncaa_espn_football_bindings.csv", row.names = FALSE)
    # Identify unmatched records from dfA (all_college_data)
    unmatched_df_a <- all_college_data[!seq_len(nrow(all_college_data)) %in% matches_out$matches$inds.a, ]
    # Identify unmatched records from dfB (ncaa_football_teams)
    unmatched_df_b <- ncaa_football_teams[!seq_len(nrow(ncaa_football_teams)) %in% matches_out$matches$inds.b, ]
    # Save unmatched records to files
    if (nrow(unmatched_df_a) > 0) write.csv(unmatched_df_a, "output/csv/unmatched_college_football_espn.csv", row.names = FALSE)
    if (nrow(unmatched_df_b) > 0) write.csv(unmatched_df_b, "output/csv/unmatched_college_football_ncaa.csv", row.names = FALSE)

    # Initialize bindings file if it doesn't exist for logo colors
    if (!file.exists("data/bindings/ncaa_logo_color_bindings.csv")) color_bindings <- tibble::tibble(url = character(), colors = character())
    else color_bindings <- read.csv("data/bindings/ncaa_logo_color_bindings.csv")


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
            img <- magick::image_read_svg(svg_url) %>% magick::image_convert(format = "png")
            # Get pixel data from downloaded image
            png_data <- magick::image_write(img, tempfile(fileext = ".png"), format = "png")
            img_array <- png::readPNG(png_data)
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
    all_college_data <- all_college_data_matched %>%
        # Replace university names when avalaible and rename to university
        dplyr::rename(university = owner) %>%
        # Replace logo url when avalaible
        dplyr::mutate(logo = if_else(is.na(img_src), logo, img_src)) %>%
        # Reorder column for consistancy
        dplyr::select(id, espn_id, ncaa_id, type, abv, full_name, nickname, university, division, conference, dplyr::everything()) %>%
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
        dplyr::select(-img_src, -short_name, -temp_colors, -dominant_colors, -colors, -gamma.1, -gamma.2, -gamma.3, -posterior) %>%
        dplyr::rename(short_name = nickname) %>%
        # Put venue back to last position
        dplyr::relocate(venue, .after = last_col())

    # Save color bindings back to file
    write.csv(color_bindings, "data/bindings/ncaa_logo_color_bindings.csv")

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

    if (verbose) cat(paste0("\n\033[32mDownloading Additional NCAA Coaches: https://stats.ncaa.org/team/...\033[0m"))
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
        page_content <- download_fromHTML(url, "data/raw", TRUE, headers)
        # Get coach from xpath
        venue <- page_content %>% html_element(xpath = config$ATTRIBUTES$NCAA_STAT$VENUE) %>% html_text(trim = TRUE)
    }

    if (verbose) cat(paste0("\n\033[32mDownloading Additional NCAA Venues: https://stats.ncaa.org/team/...\n\033[0m"))
    # Filter and retrieve unknown coaches from ncaa website
    unknown_venues <- all_college_data %>% dplyr::filter(is.na(venue)) %>% dplyr::select(ncaa_id, venue)
    updated_venues <- unknown_venues %>% mutate(venue = map_chr(ncaa_id, ~{ get_venue(.x) }))
    # Update coaches back into dataset
    all_college_data <- all_college_data %>% dplyr::rows_update(updated_venues, by = "ncaa_id")

    # Analyze missing data
    analyze_missing_data("College Football", all_college_data)
    process_markdown_file("R/teams/football-teams-college.R", "R/teams/readme.md", nrow(all_college_data))

    if (verbose) cat(paste0("\n\033[90m", nrow(unmatched_df_b), " NCAA Teams and ", nrow(unmatched_df_a), " ESPN Teams Could Not be Binded: /output/csv/unmatched_...\033[0m"))
    if (verbose) cat(paste0("\n\033[90mCollege Football Data Saved To: /", all_teams_file, "\033[0m\n"))
    # Save generated csollege data
    write.csv(all_college_data, all_teams_file, row.names = FALSE)
    # Return fornated data
    return(all_college_data)
}

# If file is being run stand-alone, run function
if (interactive()) get_formated_data()
