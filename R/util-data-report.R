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

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE) 

#' Generate and Save Missing Data Bar Chart to output folder
#'
#' @param name Character string for the title/report name (e.g., "NBA Teams")
#' @param df Dataframe to analyze
#' 
analyze_missing_data <- function(name, df) {
    #Calculate missing values per column
    missing_data <- df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    arrange(desc(missing_count))
    # Create the bar chart
    missing_barchart <- ggplot(missing_data, aes(x = reorder(column, -missing_count), y = missing_count)) +
    geom_col(fill = "#1E88E5", width = 0.7) +  # NBA-style blue
    geom_text(aes(label = missing_count), vjust = -0.5, size = 3.5) +  # Add counts on top
    labs(
        title = paste0("Missing Values in ", name, " Dataset"),
        subtitle = "Columns ordered by most missing data",
        x = "Column",
        y = "Number of Missing Values"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank()
    )
    file_slug <- str_to_lower(str_replace_all(name, "\\s+", "_"))
    # Save bar chart to file
    ggsave(paste0("output/tables/", file_slug,"_missing_data.png"), plot = missing_barchart, width = 10, 
        height = 6, dpi = 300, bg = "white")
}