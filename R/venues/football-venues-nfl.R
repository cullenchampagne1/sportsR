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
config <- yaml::read_yaml("configs/football-nfl.yaml")
# File to hold formated data
all_venues_file <- "data/processed/football-venues-nfl.csv"

#' NFL Venues
#'
#' Retrieves NFL venues data from Wikipedia pages. The combined data is processed
#' into a structured dataframe and saved to a CSV and RDS file.
#'
#' @values ../../output/tables/nfl_venues_missing_data.png
#'
#' @source https://en.wikipedia.org/wiki/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param save Logical indicating whether to save data to data/processed folder
#'
#' @return A dataframe containing the following information for each NFL venue:
#'  id [string] - A generated unique identifier for each venue
#'  full_name [string] - Full name of the venue
#'  street_address [string] - Street address of the venue
#'  city [string] - City where the venue is located
#'  state [string] - State where the venue is located
#'  zip [string] - Postal code of the venue
#'  latitude [string] - Latitude in decimal degrees (e.g., 27.98028)
#'  longitude [string] - Longitude in decimal degrees (e.g., -82.50667)
#'  elevation [numeric] - Elevation in meters above sea level
#'  capacity [string] - Current official seating capacity
#'  opened [string] - Year the venue was opened
#'  surface [string] - Standardized playing surface type
#'  roof [string] - Venue roof type
#'  website [string] - Official venue website URL
#'
get_formated_venues <- function(verbose = TRUE, save = TRUE) {

    # Helper function to handle NA values in data
    `%||%` <- function(x, y) if (length(x) > 0) x else y
    
    # Init blank data frame for team details
    nfl_venue_details <- data.frame()
    # Loop through team detail webpages and select data
    for (url in config$LINKS$VENUES) {
        # Download page content from url
        page_content <- download_fromHTML(url)
        # Get name from xpath
        name <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$NAME) %>% rvest::html_text(trim = TRUE)
        if (verbose) cat(paste0("\n\033[32mDownloading ", name, " Information: ", url, "\033[0m"))
        # Extract latitude and longitude in decimal format
        latlon_decimal <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$LATLON_DECIMAL) %>% rvest::html_text(trim = TRUE)
        split_decimal <- strsplit(latlon_decimal, " ")[[1]]
        # Parse latitude
        lat_value <- gsub("[^0-9.]", "", split_decimal[1])
        lat_hemisphere <- ifelse(grepl("S", split_decimal[1], ignore.case = TRUE), -1, 1)
        latitude <- as.numeric(lat_value) * lat_hemisphere
        # Parse longitude
        lon_value <- gsub("[^0-9.]", "", split_decimal[2])
        lon_hemisphere <- ifelse(grepl("W", split_decimal[2], ignore.case = TRUE), -1, 1)
        longitude <- as.numeric(lon_value) * lon_hemisphere
        clean_latitude <- gsub("[^0-9.-]", "", latitude)
        clean_longitude <- gsub("[^0-9.-]", "", longitude)

        # Get address using OpenStreetMap API and seperate into segments
        osm_url <- paste0("https://nominatim.openstreetmap.org/reverse?lat=", clean_latitude, "&lon=", clean_longitude, "&format=json")
        osm_data <- download_fromJSON(osm_url)
        address_parts <- osm_data$address
        street_address <- paste(na.omit(c(address_parts$house_number, address_parts$road)), collapse = " ") %||% NA_character_
        city <- address_parts$city %||% address_parts$town %||% address_parts$village %||% NA_character_
        state <- address_parts$state %||% NA_character_
        postcode <- address_parts$postcode %||% NA_character_

        # Get website from xpath
        website <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$WEBSITE) %>% rvest::html_text(trim = TRUE)
        # Get capacity from xpath
        capacity <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$CAPACITY) %>% rvest::html_text(trim = TRUE)
        capacity_matches <- regmatches(capacity, gregexpr("\\d{1,3}(,\\d{3})*", capacity))
        capacity <- sapply(capacity_matches, function(m) if (length(m) > 0) m[1] else NA_character_)
        # Get surface from xpath
        surface <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$SURFACE) %>% rvest::html_text(trim = TRUE) %>% tolower()
        # Standardize surface categories
        if (grepl("artificial|astro|fieldturf|shaw|hellas", surface)) surface <- "Artificial Turf"
        else if (grepl("bermuda", surface)) surface <- "Bermuda Grass"
        else if (grepl("paspalum", surface)) surface <- "Paspalum Grass"
        else if (grepl("ryegrass", surface)) surface <- "Ryegrass Blend"
        else if (grepl("bluegrass|kentucky", surface)) surface <- "Kentucky Bluegrass"
        else if (grepl("grass", surface)) surface <- "Natural Grass"
        else surface <- "Other"

        # Get elevation using Open-Elevation API
        elevation_url <- paste0("https://api.open-elevation.com/api/v1/lookup?locations=", clean_latitude, ",", clean_longitude)
        elevation_data <- download_fromJSON(elevation_url)
        elevation <- elevation_data$results$elevation[1] %||% NA_real_

        # Return dataframe with parsed information
        nfl_venue_details <- rbind(nfl_venue_details, data.frame(
            full_name = name %||% NA_character_,
            street_address = street_address,
            city = city,
            state = state,
            zip = postcode,
            latitude = latitude %||% NA_character_,
            longitude = longitude %||% NA_character_,
            elevation = elevation,
            capacity = capacity %||% NA_character_,
            surface = surface %||% NA_character_,
            website = website %||% NA_character_,
            stringsAsFactors = FALSE
        ))
    }

    # Download NFL venue list to get roof type and opening year
    page_content <- download_fromHTML(config$LINKS$VENUE_ROOF)
    if (verbose) cat(paste0("\n\033[32mDownloading Addional NFL Venue Information: ", config$LINKS$VENUE_ROOF, "\033[0m\n"))
    # Get second table on webpage
    tables <- page_content %>% rvest::html_elements("table")
    venue_roofs <- tables[[2]] %>%
    rvest::html_table(fill = TRUE) %>%
    # Extract only relevant columns
    dplyr::select(Name, "Roof type", Opened) %>%
    dplyr::rename(full_name = Name, roof = "Roof type", opened = Opened) %>%
    # Remove special characters from venue name
    dplyr::mutate(full_name = gsub("‡", "", full_name)) %>%
    # Remove all external links from opening year
    dplyr::mutate(opened = opened %>% gsub("\\[[a-z0-9]+\\]", "", .))

    # Join new info with venue details
    nfl_venue_details <- nfl_venue_details %>% dplyr::left_join(venue_roofs, by = "full_name")

    # Create a unique stadium id from full name
    nfl_venue_details <- nfl_venue_details %>%
    dplyr::mutate(id = encode_id(paste0("F", full_name), full_name)) %>%
    # Reorder columns to standard
    select(id, dplyr::everything()) %>%
    relocate(roof, .after = surface) %>%
    relocate(opened, .after = capacity)

    # Analyze missing data and process markdown file
    analyze_missing_data("NFL Venues", nfl_venue_details)
    plot_coordinates_map("NFL Venues", nfl_venue_details)
    if (sys.nframe() == 0) process_markdown_file("R/venues/football-venues-nfl.R", "R/venues/readme.md", nrow(nfl_venue_details), "venues")

    if (verbose && save) cat(paste0("\n\033[90mNFL Football Data Saved To: /", all_venues_file, "\033[0m\n"))
    # Save any created name bindings to file
    if (save) write.csv(nfl_venue_details, all_venues_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(nfl_venue_details, sub("\\.csv$", ".rds", all_venues_file))
    # Return formated data
    return(nfl_venue_details)
}

# If file is being run stand-alone, run function
if (sys.nframe() == 0) invisible(get_formated_venues())
