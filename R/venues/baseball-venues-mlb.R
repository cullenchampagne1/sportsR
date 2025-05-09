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

# Read configuration from configs directory
config <- yaml::read_yaml("configs/baseball-mlb.yaml")
# File to hold formated data
all_venues_file <- "data/processed/baseball-venues-mlb.csv"

#' MLB Venues
#'
#' Retrieves MLB venues data wiki pages. The combined data is processed
#' into a structured dataframe and saved to a CSV file.
#'
#' @values ../../output/tables/mlb_venues_missing_data.png
#'
#' @source https://en.wikipedia.org/wiki/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param save Logical indicating whether to save data to data/processed folder
#'
#' @return A dataframe containing the following information for each MLB venue:
#'  id [string] - A generated unique identifier for each venue
#'  full_name [string] - Full name of the venue
#'  address [string] - Venue address including city and state
#'  latitude [string] - Latitude in decimal degrees (e.g., 27.98028°N)
#'  longitude [string] - Longitude in decimal degrees (e.g., 82.50667°W)
#'  capacity [string] - Current official seating capacity
#'  surface [string] - Standardized playing surface type
#'  field_size_left [string] - Left field distance in feet
#'  field_size_left_center [string] - Left-center field distance in feet
#'  field_size_center [string] - Center field distance in feet
#'  field_size_right_center [string] - Right-center field distance in feet
#'  field_size_right [string] - Right field distance in feet
#' 
get_formated_data <- function(verbose = TRUE, save = TRUE) {
    # Init blank data frame for team details
    mlb_venue_details <- data.frame()
    # Loop through team detail webpages and select data
    if (verbose) cat(paste0("\n\033[32mDownloading MLB Venue Information: https://en.wikipedia.org/wiki/...\n\033[0m"))
    for (url in config$LINKS$VENUES) {
        # Download page content from url
        page_content <- download_fromHTML(url)
        # Get name from xpath
        name <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$NAME) %>% rvest::html_text(trim = TRUE)
        # Get address from xpath
        address <- page_content %>% 
        rvest::html_element(xpath = config$ATTRIBUTES$VENUES$ADDRESS) %>%
        rvest::html_text(trim = TRUE) %>%
        sub("\\[.*$", "", .) %>%
        trimws()
        # Hardcoded fix for the one non working address
        if (is.na(address)) address <- "1 Steinbrenner Drive"
        # Get location from xpath
        location <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$LOCATION) %>% rvest::html_text(trim = TRUE)
        # Extract latitude and longitude in decimal format
        latlon_decimal <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$LATLON_DECIMAL) %>% rvest::html_text(trim = TRUE)
        split_decimal <- strsplit(latlon_decimal, " ")[[1]]
        latitude <- if (length(split_decimal) >= 1) trimws(split_decimal[1]) else NA_character_
        longitude <- if (length(split_decimal) >= 2) trimws(split_decimal[2]) else NA_character_
        # Get capacity from xpath
        capacity <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$CAPACITY) %>% rvest::html_text(trim = TRUE)
        capacity_matches <- regmatches(capacity, gregexpr("\\d{1,3}(,\\d{3})*", capacity))
        capacity <- sapply(capacity_matches, function(m) if (length(m) > 0) m[1] else NA_character_)
        # Extract all field size values in sequence, excluding those inside links
        field_sizes <- page_content %>%
          rvest::html_elements(xpath = paste0(config$ATTRIBUTES$VENUES$FIELD_SIZE_ALL, "[not(ancestor::a)]")) %>%
          rvest::html_text(trim = TRUE)
        # Remove parenthetical year ranges like "(1973-1994)" or "(2004–present)"
        field_sizes <- field_sizes[!grepl("\\(\\d{4}[-–]\\d{0,4}\\)", field_sizes)]
        # First try to capture values with units (e.g., "342 ft", "100 m")
        field_sizes_with_units <- field_sizes[grepl("\\d+\\s*(feet|ft|m)", field_sizes, ignore.case = TRUE)]
        # If none found, fall back to any digit-containing values
        if (length(field_sizes_with_units) > 0) field_sizes <- field_sizes_with_units
        else field_sizes <- field_sizes[grepl("\\d+", field_sizes)]
        # Assign values by position if available
        field_size_left <- if (length(field_sizes) >= 1) field_sizes[1] else NA
        field_size_left_center <- if (length(field_sizes) >= 2) field_sizes[2] else NA
        field_size_center <- if (length(field_sizes) >= 3) field_sizes[3] else NA
        field_size_right_center <- if (length(field_sizes) >= 4) field_sizes[4] else NA
        field_size_right <- if (length(field_sizes) >= 5) field_sizes[5] else NA
        # Extract just the first number from each
        field_size_left <- sub("^[^0-9-]*[-–]?[ ]*(\\d+).*", "\\1", field_size_left)
        field_size_left_center <- sub("^[^0-9-]*[-–]?[ ]*(\\d+).*", "\\1", field_size_left_center)
        field_size_center <- sub("^[^0-9-]*[-–]?[ ]*(\\d+).*", "\\1", field_size_center)
        field_size_right_center <- sub("^[^0-9-]*[-–]?[ ]*(\\d+).*", "\\1", field_size_right_center)
        field_size_right <- sub("^[^0-9-]*[-–]?[ ]*(\\d+).*", "\\1", field_size_right)
        # Get surface from xpath
        surface <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$SURFACE) %>% rvest::html_text(trim = TRUE) %>% tolower()
        # Standardize surface categories
        if (grepl("artificial|astro|fieldturf|shaw", surface)) surface <- "Artificial Turf"
        else if (grepl("bermuda", surface)) surface <- "Bermuda Grass"
        else if (grepl("paspalum", surface)) surface <- "Paspalum Grass"
        else if (grepl("ryegrass", surface)) surface <- "Ryegrass Blend"
        else if (grepl("bluegrass|kentucky", surface)) surface <- "Kentucky Bluegrass"
        else if (grepl("grass", surface)) surface <- "Natural Grass"
        else surface <- "Other"
        # Return dataframe with parsed information
        mlb_venue_details <- rbind(mlb_venue_details, data.frame(
            full_name = name %||% NA_character_,
            address = paste(address, location, sep = ", ") %||% NA_character_,
            latitude = latitude %||% NA_character_,
            longitude = longitude %||% NA_character_,
            capacity = capacity %||% NA_character_,
            surface = surface %||% NA_character_,
            field_size_left = field_size_left %||% NA_character_,
            field_size_left_center = field_size_left_center %||% NA_character_,
            field_size_center = field_size_center %||% NA_character_,
            field_size_right_center = field_size_right_center %||% NA_character_,
            field_size_right = field_size_right %||% NA_character_,
            stringsAsFactors = FALSE
        ))
    }

    # Create a unique stadium id from full name
    mlb_venue_details <- mlb_venue_details %>% dplyr::mutate(id = encode_id(paste0("B", full_name), full_name)) %>% select(id, dplyr::everything())

    # Analyze missing data
    analyze_missing_data("MLB Venues", mlb_venue_details)
    process_markdown_file("R/venues/baseball-venues-mlb.R", "R/venues/readme.md", nrow(mlb_venue_details))

    if (verbose) cat(paste0("\n\033[90mMLB Baseball Data Saved To: /", all_venues_file, "\033[0m\n"))
    # Save any created name bindings to file
    if (save) write.csv(mlb_venue_details, all_venues_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(mlb_venue_details, sub("\\.csv$", ".rds", all_venues_file))
    # Return formated data
    return(mlb_venue_details)
}

# If file is being run stand-alone, run function
invisible(get_formated_data())
