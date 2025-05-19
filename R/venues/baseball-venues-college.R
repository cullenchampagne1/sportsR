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
config <- yaml::read_yaml("configs/baseball-college.yaml")
# File to hold formated data
all_venues_file <- "data/processed/baseball-venues-college.csv"

#' College Baseball Venues
#'
#' Retrieves college baseball venues data wiki pages. The combined data is processed
#' into a structured dataframe and saved to a CSV file.
#'
#' @values ../../output/tables/college_baseball_venues_missing_data.png
#'
#' @source https://en.wikipedia.org/wiki/
#'
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param save Logical indicating whether to save data to data/processed folder
#'
#' @return A dataframe containing the following information for each MLB venue:
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
#'  surface [string] - Standardized playing surface type
#'
get_formated_data <- function(verbose = TRUE, save = TRUE) {

    # Helper function to handle NA values in data
    `%||%` <- function(x, y) if (length(x) > 0) x else y

    # Init blank data frame for team details
    college_venue_details <- data.frame()
    # Download list of venues page content from url
    page_content <- download_fromHTML(config$LINKS$VENUES)

    # Select all rows of the wikitable sortable object
    venue_table <- page_content %>% rvest::html_element(".wikitable.sortable") %>% rvest::html_elements("tr")
    # Parse each row while skipping header
    venue_details <- lapply(venue_table[-1], function(row) {
        # Extract all columns in current row
        cols <- row %>% rvest::html_elements("td")
        if (length(cols) >= 7) {
            # Check if stadium title is outside link
            stadium_link_elements <- cols[2] %>% html_elements("a")
            # If so extract name and href location
            if (length(stadium_link_elements) > 0) {
                stadium_name <- stadium_link_elements %>% html_text(trim = TRUE)
                stadium_href <- paste0("https://en.wikipedia.org", stadium_link_elements %>% html_attr("href"))
                # Return name and href formated in list
                list(stadium_name = stadium_name, stadium_href = stadium_href)
            }
        } else {
            NULL
        }
    }) %>%
    # Compect created list and convert into dataframe
    purrr::compact() %>%
    bind_rows()

    #' Helper function to retrieve details for any college baseball stadium
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
            capacity = NA_character_,
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

        # Get capacity from xpath
        capacity <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$CAPACITY) %>% rvest::html_text(trim = TRUE)
        capacity_matches <- regmatches(capacity, gregexpr("\\d{1,3}(,\\d{3})*", capacity))
        capacity <- sapply(capacity_matches, function(m) if (length(m) > 0) m[1] else NA_character_)

        # Get surface from xpath and convert to standard
        surface <- page_content %>% rvest::html_element(xpath = config$ATTRIBUTES$VENUES$SURFACE) %>% rvest::html_text(trim = TRUE) %>% tolower()
        if (grepl("artificial|astro|fieldturf|shaw|hellas|turf", surface)) surface <- "Artificial Turf"
        else if (grepl("bermuda", surface)) surface <- "Bermuda Grass"
        else if (grepl("paspalum", surface)) surface <- "Paspalum Grass"
        else if (grepl("ryegrass", surface)) surface <- "Ryegrass Blend"
        else if (grepl("bluegrass|kentucky", surface)) surface <- "Kentucky Bluegrass"
        else if (grepl("grass", surface)) surface <- "Natural Grass"
        else surface <- "Unknonwn"

        # Get elevation using Open-Elevation API
        if (!is.na(clean_longitude)) {
            elevation_url <- paste0("https://api.open-elevation.com/api/v1/lookup?locations=", clean_latitude, ",", clean_longitude)
            elevation_data <- download_fromJSON(elevation_url)
            elevation <- elevation_data$results$elevation[1] %||% NA_real_
        } else {
            elevation <- NA_real_
        }

        # Append to venue details dataframe
        data.frame(
            street_address = ifelse(nchar(street_address) > 2, street_address, strsplit(location, ",")[[1]][1]),
            city = city %||% NA_character_,
            state = state %||% NA_character_,
            zip = postcode %||% NA_character_,
            latitude = if (!is.na(latitude)) latitude else NA_real_,
            longitude = if (!is.na(longitude)) longitude else NA_real_,
            elevation = elevation,
            capacity = capacity %||% NA_character_,
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

    # Join dimensions with the current data
    venue_details <- venue_details %>%
        dplyr::rename(full_name = stadium_name) %>%
        dplyr::mutate(id = encode_id(paste0("B", full_name), full_name)) %>%
        dplyr::select(id, full_name, dplyr::everything(), -stadium_href)

    # Analyze missing data and process markdown file
    analyze_missing_data("College Baseball Venues", venue_details)
    plot_coordinates_map("College Baseball Venues", venue_details)
    process_markdown_file("R/venues/baseball-venues-college.R", "R/venues/readme.md", nrow(venue_details), "venues")

    if (verbose) cat(paste0("\n\n\033[90mCollege Baseball Data Saved To: /", all_venues_file, "\033[0m\n"))
    # Save any created name bindings to file
    if (save) write.csv(venue_details, all_venues_file, row.names = FALSE)
    # Save rds file of data
    if (save) saveRDS(venue_details, sub("\\.csv$", ".rds", all_venues_file))
    # Return formated data
    return(venue_details)
    
}

# If file is being run stand-alone, run function
invisible(get_formated_data())
