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
library(fastLink, quietly = TRUE, warn.conflicts = FALSE) # Weighted dataset matching for NCAA and ESPN
library(stringdist, quietly = TRUE, warn.conflicts = FALSE) # Used by fastlink

# Read configuration from configs directory
config <- yaml::read_yaml("configs/baseball_college.yaml")
# File to hold formated data
all_teams_file <- "data/processed/baseball-teams-college.csv"

#' College Basketball Teams
#'
#' Retrieves college baseball team data from ESPN's API and supplements
#' it with additional information scraped from NCAA. The combined
#' data is processed into a structured dataframe and saved to a CSV file.
#'
#' @source https://site.api.espn.com/
#' @source https://www.ncaa.com/stats/basketball-men/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#'
#' @return A dataframe containing the following information for each baseball team
#'
get_formated_data <- function(verbose = TRUE) {
    # Grab College basketball data from ESPN
    if (verbose) cat(paste0("\n\033[32mDownloading ESPN Baseball Teams: ", config$LINKS$ESPN_TEAMS, "\033[0m"))
    college_basketball_teams <- download_fromJSON(config$LINKS$ESPN_TEAMS, simplifyDataFrame = FALSE)

    # Extract all teams from ESPN json structure
    espn_teams_list <- lapply(college_basketball_teams$sports[[1]]$leagues[[1]]$teams, function(x) x$team)

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
    college_baseball_teams <- bind_rows(lapply(espn_teams_list, extract_team)) %>%
            dplyr::rename(espn_id = id)

    if (verbose) cat(paste0("\n\033[32mDownloading NCAA Baseball Teams: https://www.ncaa.com/stats/baseball/...\033[0m"))
    # Dataframe to hold all parsed NCAA teams
    ncaa_baseball_teams <- data.frame()
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
        ncaa_baseball_teams <- rbind(ncaa_baseball_teams, dplyr::bind_rows(team_data))
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
    ncaa_baseball_teams <- merge(ncaa_baseball_teams, ncaa_ids,
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
    ncaa_baseball_teams <- ncaa_baseball_teams %>%
    dplyr::mutate(scraped_data = purrr::map(school_url, ~ {
        scrape_ncaa_team_data(.x)
    })) %>%
    tidyr::unnest(scraped_data) %>%
    dplyr::rename(displayName = school_name, nickname_old = nickname, nickname = name)

    # Generate a slug column for matching by removing the base url and trailing slashes
    ncaa_baseball_teams$slug <- gsub("https://www.ncaa.com/schools/", "", ncaa_baseball_teams$school_url)
    ncaa_baseball_teams$slug <- gsub("/.*", "", ncaa_baseball_teams$slug)

    # List of state and university abreveations that are swapped for consistancy
    abbreviations <- yaml::read_yaml("data/mutations/university_abbreviations.yaml")$ABBREVIATIONS %>% unlist() %>% set_names(names(.))
    # Not all teams can be automaticaly matched, load a maual mutations file to assist with remaining teams
    mutations <- yaml::read_yaml("data/mutations/baseball_college_mutations.yaml")
    # Iterate through custom rules for NCAA
    for (rule in mutations$NCAA) {
        matches <- stringr::str_match(rule, "IF (\\w+) == '(.*?)' THEN (\\w+) = '(.*?)'")
        ncaa_baseball_teams <- ncaa_baseball_teams %>% dplyr::mutate(
            !!matches[4] := if_else(.data[[matches[2]]] == matches[3], matches[5], .data[[matches[4]]]))
    }
    # Iterate through custom rules for ESPN
    for (rule in mutations$ESPN) {
        matches <- stringr::str_match(rule, "IF (\\w+) == '(.*?)' THEN (\\w+) = '(.*?)'")
        college_baseball_teams <- college_baseball_teams %>% dplyr::mutate(
            !!matches[4] := if_else(.data[[matches[2]]] == matches[3], matches[5], .data[[matches[4]]]))
    }

    # Apply abreviation replacements to both dataframes
    college_baseball_teams <- college_baseball_teams %>% dplyr::mutate(dplyr::across(c(displayName), ~ str_replace_all(.x, abbreviations)))
    ncaa_baseball_teams <- ncaa_baseball_teams %>% dplyr::mutate(dplyr::across(c(displayName), ~ str_replace_all(.x, abbreviations)))

    sink(tempfile())
    # Use fast link to combine both datasets using string simulity over multiple columns
    matches_out <- fastLink::fastLink(
        dfA = college_baseball_teams, dfB = ncaa_baseball_teams,
        varnames = c("displayName", "nickname", "slug"),
        stringdist.match = c("displayName", "nickname", "slug"),
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
        dfA = college_baseball_teams,
        dfB = ncaa_baseball_teams,
        fl.out = matches_out
    )

     # Create a bindings file for later use
    bindings <- all_college_data_matched %>% dplyr::select(ncaa_id, espn_id)
    write.csv(bindings, "data/bindings/ncaa_espn_baseball_bindings.csv", row.names = FALSE)
    # Identify unmatched records from dfA (college_basketball_teams)
    unmatched_df_a <- college_baseball_teams[!seq_len(nrow(college_baseball_teams)) %in% matches_out$matches$inds.a, ]
    # Identify unmatched records from dfB (ncaa_basketball_teams)
    unmatched_df_b <- ncaa_baseball_teams[!seq_len(nrow(ncaa_baseball_teams)) %in% matches_out$matches$inds.b, ]
    # Save unmatched records to files
    if (nrow(unmatched_df_a) > 0) write.csv(unmatched_df_a, "output/csv/unmatched_college_baseball_espn.csv", row.names = FALSE)
    if (nrow(unmatched_df_b) > 0) write.csv(unmatched_df_b, "output/csv/unmatched_college_baseball_ncaa.csv", row.names = FALSE)



    if (verbose) cat(paste0("\n\033[90m", nrow(unmatched_df_b), " NCAA Teams and ", nrow(unmatched_df_a), " ESPN Teams Could Not be Binded: /output/csv/unmatched_...\033[0m"))
    if (verbose) cat(paste0("\n\033[90mCollege Baseball Data Saved To: /", all_teams_file, "\033[0m\n"))

    # Save any created name bindings to file
    write.csv(all_college_data_matched, all_teams_file, row.names = FALSE)
    # Return formated data
    return(all_college_data_matched)

}

# If file is being run stand-alone, run function
if (interactive()) get_formated_data()