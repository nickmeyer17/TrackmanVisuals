# Load the required libraries
library(data.table)
library(rmarkdown)

# Sample data (replace with your dataset)
date <- <DATE>
file <- paste0(date, "-<FieldName>-Filename")

data <- read.csv(file)

# Sort the data by score in descending order to create a leaderboard
data <- data[order(-ExitSpeed)]

# Create a function to generate the PDF report
generate_leaderboard_pdf <- function(data) {
  rmarkdown::render(
    input = "Postgame_Leaderboard.rmd",  # Create a separate RMarkdown file (leaderboard.Rmd)
    output_format = "pdf_document",
    output_file = "leaderboard.pdf",
    params = list(leaderboard_data = data)
  )
}

# Call the function to generate the PDF
generate_leaderboard_pdf(data)
