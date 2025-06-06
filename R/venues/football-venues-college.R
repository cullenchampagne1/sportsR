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
config <- yaml::read_yaml("configs/football-college.yaml")
# File to hold formated data
all_venues_file <- "data/processed/football-venues-college.csv"

#' College Football Venues
#'
#' Retrieves College Football venues data from Wikipedia pages. The combined 
#' data is processed into a structured dataframe and saved to a CSV and RDS file.
#'
#' @values ../../output/tables/college_football_venues_missing_data.png
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
#'
get_formated_venues <- function(verbose = TRUE, save = TRUE) {
     # Helper function to handle NA values in data
    `%||%` <- function(x, y) if (length(x) > 0) x else y
    
    # Download list of venues page content from url
    page_content <- download_fromHTML(config$LINKS$FCS_VENUES)
    # Select all wikitable.sortable tables
    fcs_tables <- page_content %>% rvest::html_elements(".wikitable.sortable")
    # Select the second table
    fcs_table <- fcs_tables[1] %>% rvest::html_elements("tr")
    # Parse each row while skipping header
    venue_details <- lapply(fcs_table[-1], function(row) {
        # Extract all columns in current row
        cols <- row %>% rvest::html_elements("td")
        if (length(cols) >= 7) {
            # Check if stadium title is outside link
            stadium_link_elements <- cols[2] %>% html_elements("a")
            # Get capacity and trim any external links
            stadium_capacity <- cols[7] %>% rvest::html_text(trim = TRUE)
            capacity_matches <- regmatches(stadium_capacity, gregexpr("\\d{1,3}(,\\d{3})*", stadium_capacity))
            stadium_capacity <- sapply(capacity_matches, function(m) if (length(m) > 0) m[1] else NA_character_)
            # Get opened year and trim any external links
            stadium_opened <- cols[9] %>% rvest::html_text(trim = TRUE)
            opened_matches <- regmatches(stadium_opened, gregexpr("\\d{1,4}(,\\d{4})*", stadium_opened))
            stadium_opened <- sapply(opened_matches, function(m) if (length(m) > 0) m[1] else NA_character_)

            # If so extract name and href location
            if (length(stadium_link_elements) > 0) {
                stadium_name <- stadium_link_elements %>% html_text(trim = TRUE)
                stadium_href <- paste0("https://en.wikipedia.org", stadium_link_elements %>% html_attr("href"))
                # Return name, href and other data formated in list
                list(
                    stadium_name = stadium_name,
                    stadium_href = stadium_href,
                    capacity = stadium_capacity,
                    opened = stadium_opened
                    )
            }
        } else {
            NULL
        }
    }) %>%
    # Compect created list and convert into dataframe
    purrr::compact() %>%
    bind_rows()

    # Download list of venues page content from url
    page_content <- download_fromHTML(config$LINKS$FBS_VENUES)
    # Select all wikitable.sortable tables
    fbs_tables <- page_content %>% rvest::html_elements(".wikitable.sortable")
    # Select the second table
    fbs_table <- fbs_tables[1] %>% rvest::html_elements("tr")
    # Parse each row while skipping header
    fbs_details <- lapply(fbs_table[-1], function(row) {
        # Extract all columns in current row
        cols <- row %>% rvest::html_elements("td")
        if (length(cols) >= 7) {
            # Check if stadium title is outside link
            stadium_link_elements <- cols[2] %>% html_elements("a")
            # Get capacity and trim any external links
            stadium_capacity <- cols[7] %>% rvest::html_text(trim = TRUE)
            capacity_matches <- regmatches(stadium_capacity, gregexpr("\\d{1,3}(,\\d{3})*", stadium_capacity))
            stadium_capacity <- sapply(capacity_matches, function(m) if (length(m) > 0) m[1] else NA_character_)
            # Get opened year and trim any external links
            stadium_opened <- cols[9] %>% rvest::html_text(trim = TRUE)
            opened_matches <- regmatches(stadium_opened, gregexpr("\\d{1,4}(,\\d{4})*", stadium_opened))
            stadium_opened <- sapply(opened_matches, function(m) if (length(m) > 0) m[1] else NA_character_)

            # If so extract name and href location
            if (length(stadium_link_elements) > 0) {
                stadium_name <- stadium_link_elements %>% html_text(trim = TRUE)
                stadium_href <- paste0("https://en.wikipedia.org", stadium_link_elements %>% html_attr("href"))
                # Return name, href and other data formated in list
                list(
                    stadium_name = stadium_name,
                    stadium_href = stadium_href,
                    capacity = stadium_capacity,
                    opened = stadium_opened
                    )
            }
        } else {
            NULL
        }
    }) %>%
    # Compect created list and convert into dataframe
    purrr::compact() %>%
    bind_rows()

    # Combine both venues details before proceeding
    venue_details <- bind_rows(venue_details, fbs_details)

    #' Helper function to retrieve details for any college basketball stadium
    #'
    #' @param url link to venues wiki page
    #'
    #' @return list containing info retrieved
    #'
    get_stadium_details <- function(url) {
        # Download page content from url
        page_content <- download_fromHTML(url)
        # Get name from xpath
        name <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$NAME) %>% rvest::html_text(trim = TRUE)
        # Get location from xpath
        location <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$LOCATION) %>% rvest::html_text(trim = TRUE)
        if (verbose) cat(paste0("\n\033[32mDownloading ", name, " Information: ", url, "\033[0m"))
        # If cannot find name (not valid link) then return NA values
        if (is.na(name)) return(data.frame(
            street_address = NA_character_,
            city = NA_character_,
            state = NA_character_,
            zip = NA_character_,
            latitude = NA_real_,
            longitude = NA_real_,
            elevation = NA_real_,
            surface = NA_character_,
            stringsAsFactors = FALSE
        ))
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
        if (!is.na(clean_longitude)) {
            osm_url <- paste0("https://nominatim.openstreetmap.org/reverse?lat=", clean_latitude, "&lon=", clean_longitude, "&format=json")
            osm_data <- download_fromJSON(osm_url)
            address_parts <- osm_data$address
            street_address <- paste(na.omit(c(address_parts$house_number, address_parts$road)), collapse = " ") %||% NA_character_
            city <- address_parts$city %||% 
                    address_parts$town %||% 
                    address_parts$village %||% 
                    address_parts$suburb %||% 
                    address_parts$hamlet %||% 
                    address_parts$county %||% 
                    NA_character_
            state <- address_parts$state %||% NA_character_
            postcode <- address_parts$postcode %||% NA_character_
        } else {
            street_address <- NA_character_
            city <- NA_character_
            state <- NA_character_
            postcode <- NA_character_
        }

        # Get elevation using Open-Elevation API
        if (!is.na(clean_longitude)) {
            elevation_url <- paste0("https://api.open-elevation.com/api/v1/lookup?locations=", clean_latitude, ",", clean_longitude)
            elevation_data <- download_fromJSON(elevation_url)
            elevation <- elevation_data$results$elevation[1] %||% NA_real_
        } else {
            elevation <- NA_real_
        }

        # Get surface from xpath and convert to standard
        surface <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$SURFACE) %>% rvest::html_text(trim = TRUE) %>% tolower()
        if (grepl("artificial|astro|fieldturf|shaw|hellas|turf", surface)) surface <- "Artificial Turf"
        else if (grepl("bermuda", surface)) surface <- "Bermuda Grass"
        else if (grepl("paspalum", surface)) surface <- "Paspalum Grass"
        else if (grepl("ryegrass", surface)) surface <- "Ryegrass Blend"
        else if (grepl("bluegrass|kentucky", surface)) surface <- "Kentucky Bluegrass"
        else if (grepl("grass", surface)) surface <- "Natural Grass"
        else surface <- "Unknonwn"

        # Append to venue details dataframe
        data.frame(
            street_address = ifelse(nchar(street_address) > 2, street_address, strsplit(location, ",")[[1]][1]),
            city = city %||% NA_character_,
            state = state %||% NA_character_,
            zip = postcode %||% NA_character_,
            latitude = if (!is.na(latitude)) latitude else NA_real_,
            longitude = if (!is.na(longitude)) longitude else NA_real_,
            elevation = elevation,
            surface = surface %||% NA_character_,
            stringsAsFactors = FALSE
        )
    }

    # Scrape each teams detailed value data
    venue_details <- venue_details %>% dplyr::mutate(scraped_data = purrr::map(stadium_href, ~ {
        get_stadium_details(.x)
    })) %>%
    tidyr::unnest(scraped_data) %>%
    filter(!is.na(street_address))

    # Format data to match all venues
    venue_details <- venue_details %>%
        dplyr::rename(full_name = stadium_name) %>%
        dplyr::mutate(id = encode_id(paste0("F", full_name), full_name)) %>%
        dplyr::select(id, full_name, dplyr::everything(), -stadium_href) %>%
        relocate(capacity, opened, .after = elevation)

    # Analyze missing data and process markdown file
    analyze_missing_data("College Football Venues", venue_details)
    plot_coordinates_map("College Football Venues", venue_details)
    process_markdown_file("R/venues/football-venues-college.R", "R/venues/readme.md", nrow(venue_details), "venues")

    if (verbose && save) cat(paste0("\n\n\033[90mCollege Football Data Saved To: /", all_venues_file, "\033[0m\n"))
    # Save any created name bindings to file
    if (save) write.csv(venue_details, all_venues_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(venue_details, sub("\\.csv$", ".rds", all_venues_file))
    # Return formated data
    return(venue_details)
}
