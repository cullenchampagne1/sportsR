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

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(stringr, quietly = TRUE, warn.conflicts = FALSE)


#' Analyze Missing Data in a Data Frame
#'
#' Generates a bar chart showing the number of missing values per column in the provided data frame.
#' The plot is saved as a PNG file in the 'output/tables/' directory.
#'
#' @param name A character string used in the plot title and output file name.
#' @param df A data frame to analyze for missing values.
#'
#' @return Saves a PNG file of the missing data plot. No object is returned.
#'
analyze_missing_data <- function(name, df) {
    # Calculate missing values per column and add column numbers
    missing_data <- df %>%
        summarise(across(everything(), ~ sum(is.na(.)))) %>%
        pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
        mutate(col_number = row_number()) %>%  # Add column numbers
        arrange(desc(missing_count))
    
    # Create the bar chart with refined styling
    missing_barchart <- ggplot(missing_data, aes(x = reorder(col_number, -missing_count), y = missing_count)) +
        geom_col(fill = "#333333", width = 0.6) +  # Dark gray bars
        geom_text(aes(label = missing_count), vjust = -0.3, size = 2.8, color = "black") +
        labs(
            title = paste("Missing Values:", name),
            x = "Column Number",
            y = NULL
        ) +
        theme_minimal(base_size = 10) +
        theme(
            plot.title = element_text(face = "bold", size = 11, margin = margin(b = 3)),
            plot.margin = unit(c(2, 2, 2, 2), "mm"),
            panel.background = element_rect(fill = "#f8f8f8", color = NA),
            plot.background = element_rect(fill = "#f8f8f8", color = NA),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black")
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

    file_slug <- str_to_lower(str_replace_all(name, "\\s+", "_"))
    output_dir <- "output/tables/"
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    ggsave(
        paste0(output_dir, file_slug, "_missing_data.png"),
        plot = missing_barchart,
        width = 8,
        height = 2.5,
        dpi = 300,
        bg = "#f8f8f8" 
    )
}

#' Plot Latitude and Longitude Points on a Map of the United States
#'
#' Creates a map plot with provided latitude and longitude points overlaid on a map of the United States.
#' The plot is saved as a PNG file in the 'output/figures/' directory.
#'
#' @param name A character string used in the plot title and output file name.
#' @param df A data frame with 'latitude' and 'longitude' columns.
#'
#' @return Saves a PNG file of the map plot. No object is returned.
#'
plot_coordinates_map <- function(name, df) {
    df <- df %>%
        filter(longitude >= -125, longitude <= -65, latitude >= 25, latitude <= 50) %>%
        group_by(longitude, latitude) %>%
        summarise(count = n(), .groups = "drop")
    coord_plot <- ggplot(df, aes(x = longitude, y = latitude)) +
        borders("state", colour = "gray80", fill = "gray95") +
        geom_point(color = "#333333", size = 1) +
        labs(title = paste("Map Coordinates:", name), x = "Longitude", y = "Latitude") +
        theme_minimal(base_size = 10) +
        theme(
            plot.title = element_text(face = "bold", size = 11, margin = margin(b = 3)),
            plot.margin = unit(c(2, 2, 2, 2), "mm"),
            panel.background = element_rect(fill = "#f8f8f8", color = NA),
            plot.background = element_rect(fill = "#f8f8f8", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(color = "black")
        )

    file_slug <- str_to_lower(str_replace_all(name, "\\s+", "_"))
    output_dir <- "output/figures/"
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    ggsave(
        paste0(output_dir, file_slug, "_map_plot.png"),
        plot = coord_plot,
        width = 8,
        height = 5,
        dpi = 300,
        bg = "#f8f8f8"
    )
}