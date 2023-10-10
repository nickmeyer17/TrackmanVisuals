library(RCurl)
library(cronR)
library(lubridate)

# FTP configuration
Cluster_LocalDirectory <- Sys.getenv("Cluster_LocalDirectory")
Cluster_FieldName <- Sys.getenv("Cluster_FieldName")
Cluster_FTP_User <- Sys.getenv("Cluster_FTP_User")
Cluster_FTP_Password <- Sys.getenv("Cluster_FTP_Password")
Cluster_UmpireScript <- Sys.getenv("Cluster_UmpireScript")

ftp_server <- "ftp.trackmanbaseball.com"
ftp_user <- Cluster_FTP_User  # Replace with your FTP username
ftp_password <- Cluster_FTP_Password  # Replace with your FTP password

# Get the date for the previous day
previous_day <- Sys.Date() - 1
year <- year(previous_day)
month <- sprintf("%02d", month(previous_day))
day <- sprintf("%02d", day(previous_day))


# Construct the FTP file path
ftp_path <- paste("v3", year, month, day, "CSV/", sep = "/")
# Create the FTP URL
ftp_url <- paste("ftp://", ftp_user, ":", ftp_password, "@", ftp_server, "/", ftp_path, sep = "")

# List files in the FTP directory
file_list <- getURL(ftp_url, dirlistonly = TRUE)

# Split the file list into a vector of filenames
file_names <- unlist(strsplit(file_list, "\n"))

# Filter for CSV files
csv_files <- file_names[grep("\\.csv$", file_names)]

# Download each CSV file
for (csv_file in csv_files) {
  download_url <- paste(ftp_url, csv_file, sep = "")
  download.file(download_url, destfile = csv_file)
}


# Assuming your other R script is named "my_script.R" in the current working directory
source(Cluster_UmpireScript)
2






ulr <- paste0("ftp://", Cluster_FTP_User,":", Cluster_FTP_Password, "@ftp.trackmanbaseball.com/")
ulr
ftp_url <- url
ftp_folder <- "v3/YYYY/MM/DD/CSV"  # Replace YYYY, MM, and DD with the appropriate date

# Local folder to save downloaded files
local_download_folder <- Cluster_LocalDirectory

# Function to download files from FTP
download_files <- function() {
  # Calculate the date for the previous day
  yesterday <- Sys.Date() - 1
  yyyy <- format(yesterday, "%Y")
  mm <- format(yesterday, "%m")
  dd <- format(yesterday, "%d")

  # Construct the FTP URL for the previous day's folder
  ftp_folder <- gsub("YYYY", yyyy, ftp_folder)
  ftp_folder <- gsub("MM", mm, ftp_folder)
  ftp_folder <- gsub("DD", dd, ftp_folder)

  # List files in the FTP folder
  file_list <- getURL(ftp_folder, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  files <- unlist(strsplit(file_list, "\r\n"))

  # Download each file to the local folder
  for (file in files) {
    download_url <- file.path(ftp_folder, file)
    local_file_path <- file.path(local_download_folder, file)
    download.file(download_url, local_file_path, mode = "wb")
  }

  cat("Files downloaded successfully.\n")

  # Run the R script
  r_script_path <- Cluster_UmpireScript
  source(r_script_path)
}

# Uncomment and run the following line if you want to test the download_files function
download_files()
