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

requireNamespace("digest", quietly = TRUE)
requireNamespace("here", quietly = TRUE)
library(stringr, quietly = TRUE, warn.conflicts = FALSE) # String Manipulation

`%||%` <- function(a, b) { if (!is.null(a)) a else b }

# New enviroment to hold hisrory of generated ids
.id_history <- new.env(parent = emptyenv())
# Holds all generated ids and there input string
.id_history$records <- data.frame(
  input_string = character(), # String used for HASH
  prefix_string = character(), # String used to generate prefix
  encoded_id = character(), # Generated id
  timestamp = character(), # When id was generated
  stringsAsFactors = FALSE
)
# Filename where reports are stored 
generated_report_file <- file.path(here::here(), "output/csv", "generated_ids_all.csv")

#' Takes in a input string and prefix string and generates a uniue ID for use in
#' a relational databse, all generated ids are saved, see generate_id_report()
#'
#' @param input_string A string to use for hash
#' @param prefix_string A string to use for begining prefix
#'
#' @returns A string representing both values 
encode_id <- function(input_string, prefix_string) {
    # Generate 5 digit uniue hash from input string
    hash = sprintf("%05d", strtoi(substr(sapply(
        as.character(input_string), function(x) digest::digest(x, algo = "xxhash32")), 1, 4), 16L) %% 100000)
    # Generate a perfix for ID using first 3 letters of prefix string
    prefix = substr(str_pad(gsub("[^A-Z]", "", toupper(prefix_string)), 3, "right", "X"), 1, 3)
    # Generate encoded Id form prefix and hash
    encoded_id <- paste0(prefix, hash)
    # Generate record with all data used in generation
    hash_record <- data.frame(
        input_string = as.character(input_string),
        prefix_string = as.character(prefix_string),
        encoded_id = encoded_id,
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
    )
    # Add hash record to stored history
    .id_history$records <- rbind(.id_history$records, hash_record)
    # Return ID in form (PREFIX)HASH of input string
    return(encoded_id)
}

#' Generates a report all of generated ids and input values which is 
#' saved to ./reports/generated_ids.csv
generate_id_report <- function() write.csv(.id_history$records, generated_report_file, row.names = FALSE)

#' Shows summary statistics of generated IDs to consle
generate_id_summary <- function() {  
  cat("Total IDs generated: ", nrow(.id_history$records), "\n")
  cat("Unique prefixes: ", length(unique(.id_history$records$prefix_string)), "\n")
  cat("First generated: ", min(.id_history$records$timestamp), "\n")
  cat("Last generated: ", max(.id_history$records$timestamp), "\n")
}