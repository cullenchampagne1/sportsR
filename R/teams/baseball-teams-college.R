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

# Read configuration from configs directory
CONFIG <- yaml::read_yaml("configs/baseball_college.yaml")
# File to hold formated data
ALL_TEAMS_FILE <- "data/processed/baseball-teams-college.csv" 


#' College Basketball Teams
#'
#' Retrieves college baseball team data from ESPN's API and supplements 
#' it with additional information scraped from NCAA. The combined 
#' data is processed into a structured dataframe and saved to a CSV file.
#' 
#' @source https://site.api.espn.com/
#' @source https://www.ncaa.com/stats/basketball-men/
#'
#' @param VERBOSE Logical indicating whether to print progress messages (default: TRUE)
#' 
#' @return A dataframe containing the following information for each baseball team
#'
get_formated_data <- function(VERBOSE = TRUE) {
    # Grab College basketball data from ESPN
    if (VERBOSE) cat(paste0("\n\033[32mDownloading ESPN Baseball Teams: ", CONFIG$LINKS$ESPN_TEAMS, "\033[0m"))
    college_basketball_teams <- download_fromJSON(CONFIG$LINKS$ESPN_TEAMS, simplifyDataFrame = FALSE)

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
        return(espn_team_df)
    }

    # Run extract teams function on all teams in data
    college_baseball_teams <- bind_rows(lapply(espn_teams_list, extract_team))

}