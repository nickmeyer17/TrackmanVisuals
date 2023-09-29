library(RCurl)
library(cronR)

# FTP configuration
ftp_url <- "ftp://Username:Password@ftp.trackman.com"
ftp_folder <- "v3/YYYY/MM/DD"  # Replace YYYY, MM, and DD with the appropriate date

# Local folder to save downloaded files
local_download_folder <- "local directory"

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
  r_script_path <- "path/to/your/R/script.R"
  source(r_script_path)
}

# Uncomment and run the following line if you want to test the download_files function
# download_files()