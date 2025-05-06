
# Get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
# First argument is the path to the RDS file
rds_file <- args[1]
# Second argument is the output directory for CSV files
output_dir <- args[2]

# Load sports bindings currently embedded in the model
sport_bindings <- readRDS(rds_file)$sport_bindings
# Split by sport and save each as a CSV file
for (sport in unique(sport_bindings$sport)) {
    # Subset the data for the current sport 
    sport_df <- subset(sport_bindings, sport == sport)
    # Write the subset data to a CSV file
    csv_filename <- paste(output_dir, paste0(sport, "_ncaa_espn_bindings.csv"), sep="/")
    write.csv(sport_df, csv_filename, row.names = FALSE)
}