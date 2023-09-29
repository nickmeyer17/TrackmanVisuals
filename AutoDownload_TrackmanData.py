!pip install schedule

import os
import ftplib
import schedule
import subprocess
from datetime import datetime, timedelta

# FTP configuration
ftp_server = "ftp.trackman.com"
ftp_user = Cluster_FTP_User
ftp_pass = Cluster_FTP_Password

# Local folder to save downloaded files
local_download_folder = Cluster_LocalDirectory

# Function to download files from FTP
def download_files():
    # Calculate the date for the previous day
    yesterday = datetime.now() - timedelta(days=1)
    yyyy = yesterday.strftime("%Y")
    mm = yesterday.strftime("%m")
    dd = yesterday.strftime("%d")
    
    # FTP folder path for the previous day
    ftp_folder = f"/v3/{yyyy}/{mm}/{dd}"

    try:
        # Connect to FTP server
        with ftplib.FTP(ftp_server) as ftp:
            ftp.login(user=ftp_user, passwd=ftp_pass)
            ftp.cwd(ftp_folder)
            
            # List files in the FTP folder
            files = ftp.nlst()

            if not files:
                print("No new files for processing.")
                return

            # Download each file to the local folder
            for file in files:
                with open(os.path.join(local_download_folder, file), 'wb') as local_file:
                    ftp.retrbinary('RETR ' + file, local_file.write)

        print("Files downloaded successfully.")

        # Run the R script
        r_script_path = "C:/Users/nickm/TrackmanVisuals/UmpireReportScript.r"
        subprocess.run(["Rscript", r_script_path])

    except Exception as e:
        print(f"Error: {str(e)}")

# Schedule the script to run daily at 8 am EST
schedule.every().day.at("08:00").do(download_files)

while True:
    schedule.run_pending()
