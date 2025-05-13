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
#'  opened [string] - Year the venue was opened
#'  surface [string] - Standardized playing surface type
#'  roof [string] - Venue roof type
#'  website [string] - Official venue website URL
#'  field_size_left [string] - Left field distance in feet
#'  field_size_left_center [string] - Left-center field distance in feet
#'  field_size_center [string] - Center field distance in feet
#'  field_size_right_center [string] - Right-center field distance in feet
#'  field_size_right [string] - Right field distance in feet
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
                stadium_href <- stadium_link_elements %>% html_attr("href")
            # Else only extract the text for stadium name
            } else {
                stadium_name <- cols[2] %>% html_text(trim = TRUE)
                stadium_href <- NA_character_
            }
            # Extract team conference and capacity for better matching later
            list(
            stadium_name = stadium_name,
            stadium_href = stadium_href,
            team = cols[5] %>% html_element("a") %>% html_text(trim = TRUE) %||% NA_character_,
            conference = cols[6] %>% html_element("a") %>% html_text(trim = TRUE) %||% NA_character_,
            capacity = cols[7] %>% html_text(trim = TRUE) %||% NA_character_
            )
      } else {
        NULL
      }
    }) %>%
    # Compect created list and convert into dataframe
    purrr::compact() %>%
    bind_rows()

    # TODO: GET DETAILED DATA FROM INDIVIDUAL HREF

    write.csv(venue_details, all_venues_file, row.names = FALSE)
    
}

get_formated_data()