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
requireNamespace("httr", quietly = TRUE)
requireNamespace("jsonlite", quietly = TRUE)

#' Takes in url to remote json file and check if file has already been
#' downloaded into cache, if cache contains file then returns cache to avoid 
#' repeated api calls
#' 
#' @param url A url to the remote json data
#' @param cache_dir A string for any custom cache directory
#' @param force_refresh Set true to donwload all data no matter what
#' @param ... Any additional variables passed to JsonLite
#' 
#' @returns Json value from url
download_fromJSON <- function(url, cache_dir = "data/raw", auth = NULL, force_refresh = FALSE, ...) {
    # Check and record source URL and add if not already included
    sources_file <- file.path("data/sources.yaml")
    if (!file.exists(sources_file)) file.create(sources_file)
    current_sources <- readLines(sources_file, warn = FALSE)
    if (!paste0("  - ", url) %in% current_sources) write(paste0("  - ", url), file = sources_file, append = TRUE)
    # If cache directory has not been created yet then create one
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    # Generate a file name from urls
    cache_file <- file.path(cache_dir, paste0(digest::digest(url, algo = "md5"), ".json"))
    # If file exist in cache and force reload isnt enable - return cache
    if (file.exists(cache_file) && !force_refresh) return(jsonlite::fromJSON(cache_file, ...))
    # Get httr response from GET request
    response <- httr::GET(url, if (!is.null(auth)) httr::add_headers(Authorization = auth) else NULL)
    # Extract json data from request
    json_data <- httr::content(response, as = "text", encoding = "UTF-8")
    # Save to chache for later use
    writeLines(json_data, cache_file)
    # Return json data with any extra variable to jsonlite
    return(jsonlite::fromJSON(json_data, ...))
}

#' Takes in url to remote html file and check if file has already been
#' downloaded into cache, if cache contains file then returns cache to avoid 
#' repeated api calls
#' 
#' @param url A url to the remote json data
#' @param cache_dir A string for any custom cache directory
#' @param force_refresh Set true to donwload all data no matter what
#' @param ... Any additional variables passed to JsonLite
#' 
#' @returns HTML value from url
download_fromHTML <- function(url, cache_dir = "data/raw", force_refresh = FALSE, ...) {
    # Check and record source URL and add if not already included
    sources_file <- file.path("data/sources.yaml")
    if (!file.exists(sources_file)) file.create(sources_file)
    current_sources <- readLines(sources_file, warn = FALSE)
    if (!paste0("  - ", url) %in% current_sources) write(paste0("  - ", url), file = sources_file, append = TRUE)
    # If cache directory has not been created yet then create one
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    # Generate a file name from urls
    cache_file <- file.path(cache_dir, paste0(digest::digest(url, algo = "md5"), ".html"))
    # If file exist in cache and force reload isnt enable - return cache
    if (file.exists(cache_file) && !force_refresh) return(xml2::read_html(cache_file))
    # Get httr response from GET request
    response <- httr::GET(url, ...)  
    # Extract html data from request
    html_content <- httr::content(response, as = "text", encoding = "UTF-8")
    # Save to chache for later use
    writeLines(html_content, cache_file)
    # Return html data
    xml2::read_html(cache_file) 
}