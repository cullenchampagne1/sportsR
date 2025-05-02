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


#' Process Markdown File with Script Comment Block
#'
#' This function extracts a comment block associated with a specific function in an R script,
#' formats it into a structured Markdown block, and then updates an existing Markdown file
#' with this new block or modifies an existing section if the title matches.
#'
#' @param script_file_path A string representing the file path of the R script to extract comments from.
#' @param md_file_path A string representing the file path of the Markdown file to update.
#' @param records A numeric value representing the number of records to include in the Markdown block (e.g., the number of teams).
#' 
#' @return None. The function updates the given Markdown file in-place.
#' 
process_markdown_file <- function(script_file_path, md_file_path, records) {
    function_name <- paste0(tools::file_path_sans_ext(basename(script_file_path)), "::get_formated_data()")
    extract_comment_block <- function(file_path, fun_name) {
        lines <- readLines(file_path)
        roxy_blocks <- list()
        current_block <- c()
        for (i in seq_along(lines)) {
            if (grepl("^\\s*#'", lines[i]))  current_block <- c(current_block, lines[i])
            else if (length(current_block) > 0) {
                roxy_blocks[[length(roxy_blocks) + 1]] <- current_block
                current_block <- c()
            }
        }
        for (block in roxy_blocks) {
            if (any(grepl(paste0("^", fun_name, "\\s*<-\\s*function"), lines))) return(paste(block, collapse = "\n")) 
        }
    }
    comment <- extract_comment_block(script_file_path, "get_formated_data")
    lines <- sub("^#' ?", "", strsplit(comment, "\n")[[1]])
    title <- lines[1]
    desc_end <- which(grepl("^@source", lines))
    if (length(desc_end) == 0) desc_end <- length(lines) 
    else desc_end <- desc_end[1] - 1  
    desc <- paste(lines[2:desc_end], collapse = " ")
    sources <- sub("^@source ", "", lines[grepl("^@source", lines)])
    ret_start <- which(grepl("^@return", lines))
    ret_lines <- lines[ret_start:length(lines)]
    return_desc <- sub("^@return ", "", ret_lines[1])
    df_lines <- trimws(ret_lines[-1])
    df <- do.call(rbind, lapply(df_lines, function(x) {
        m <- regmatches(x, regexec("^(\\w+) \\[(\\w+)\\] - (.+)$", x))[[1]]
        if (length(m) == 4) data.frame(Column = m[2], Type = m[3], Description = m[4])
    }))
    block <- paste(sprintf("## %s\n\n%s\n\n", title, desc),
                    sprintf("**Function:** `%s` \n**Records:** %s teams\n\n### Returned Data Structure\n\n", function_name, records),
                    "| Column | Type | Description |\n|--------|------|-------------|\n",
                    paste(apply(df, 1, \(r) sprintf("| %s | %s | %s |", r[1], r[2], r[3])), collapse = "\n"),
                    "\n\n| Sources |\n|--------|\n", 
                    paste(sprintf("| %s |", sources), collapse = "\n"), "\n", sep = "")
    md_lines <- readLines(md_file_path, warn = FALSE)
    new_block_lines <- strsplit(block, "\n")[[1]]
    title_line <- new_block_lines[grep("^##\\s+", new_block_lines)][1]
    title <- trimws(sub("^##\\s+", "", title_line))
    start_idx <- grep(paste0("^##\\s+", title, "$"), md_lines)
    if (length(start_idx) == 0) writeLines(c(md_lines, "", block), md_file_path)
    else {
        next_headers <- grep("^##\\s+", md_lines)
        next_start_idx <- next_headers[next_headers > start_idx]
        end_idx <- ifelse(length(next_start_idx) > 0, next_start_idx[1] - 1, length(md_lines))
        updated_lines <- c(md_lines[1:(start_idx - 1)], new_block_lines, md_lines[(end_idx):length(md_lines)])
        writeLines(updated_lines, md_file_path)
    }
}