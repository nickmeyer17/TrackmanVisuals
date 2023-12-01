library(knitr)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(foreach)
library(gridExtra)
library(dplyr)
library(grid)
library(gridExtra)
library(googlesheets4)
library(googledrive)
library(png)
library(MASS)


Cluster_LocalDirectory <- Sys.getenv("Cluster_LocalDirectory")
Cluster_FieldName <- Sys.getenv("Cluster_FieldName")
Cluster_UmpireDrive_API_Path <- Sys.getenv("Cluster_UmpireDrive_API_Path")
#Set Working Directory to Local, This is where the output .pdf will save to
setwd(Cluster_LocalDirectory)
image <- readPNG("DaytonLogo.png")
# Configure the OAuth client
#drive_auth_configure(client_id = client_id, client_secret = client_secret)

# Authenticate with Google Drive
drive_auth()
2
# Get the current system date
current_date <- Sys.Date()

# Determine the previous day's date
previous_date <- current_date - 1

# Format the previous date as YYYYMMDD

date <- 20231012
#date <- format(previous_date, "%Y%m%d")
field <- paste0("-", Cluster_FieldName,"-Private-2_unverified.csv")
file <- paste0(date, field)
df <- read.csv(file)
str(df)

#MLB Statcast Strike Zone
min_plate_x <- -0.83
max_plate_x <- 0.83
max_plate_z <- 3.92
min_plate_z <- 1.17

df <- filter(df, PitchCall %in% c("BallCalled", "StrikeCalled"))


df <- df %>% mutate(true_strike = ifelse((min_plate_x - 0.25) <= PlateLocSide &
                              (max_plate_x + 0.25) >= PlateLocSide &
                              (min_plate_z - 0.25) <= PlateLocHeight &
                              (max_plate_z + 0.25) >= PlateLocHeight, TRUE, FALSE))



df <- df %>% mutate(correct_strike = ifelse(true_strike == "TRUE" & PitchCall %in% c("StrikeCalled"), TRUE, FALSE))
df <- df %>% mutate(correct_ball = ifelse(true_strike == "FALSE" & PitchCall %in% c("BallCalled"), TRUE, FALSE))

strikes <- filter(df, PitchCall %in% c("StrikeCalled"))
balls <- filter(df, PitchCall %in% c("BallCalled"))

n <- nrow(strikes)
h_x <- 1.06*sd(strikes$PlateLocSide)*n^(-1/5)
h_y <- 1.06*sd(strikes$PlateLocHeight)*n^(-1/5)

# Create a 2D kernel density estimate
kde <- kde2d(strikes$PlateLocSide, strikes$PlateLocHeight, n = 100, lims = c(range(strikes$PlateLocSide), range(strikes$PlateLocHeight)), h = c(h_x, h_y))

# Create a contour plot
#contour(kde, xlab = "X-coordinate", ylab = "Y-coordinate", main = "2D Kernel Density Estimation for 'StrikeCalled' Pitches")

# You can also add a heatmap for better visualization
#image(kde, xlab = "X-coordinate", ylab = "Y-coordinate", main = "2D Kernel Density Estimation for 'StrikeCalled' Pitches", col = terrain.colors(50))



stats <- strikes %>% summarise(
        Correct_Stike_Percent = round(mean(correct_strike, na.rm = TRUE)*100, 2)
    )

ball_call <- balls %>% summarise(
        Correct_Ball_Percent = round(mean(correct_ball, na.rm = TRUE)*100, 2)
)

stats <- merge(stats, ball_call)

plot1 <- ggplot(df, aes(x = -1 *PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = 0.25, yend = 0.25), color = "black", size = 1)+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = 0.25, yend = 0), color = "black", size = 1)+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = 0.25, yend = 0), color = "black", size = 1)+
 geom_segment(aes(x = min_plate_x, xend = 0, y = 0, yend = -0.25), color = "black", size = 1)+
 geom_segment(aes(x = 0, xend = max_plate_x, y = -0.25, yend = 0), color = "black", size = 1)+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = "Game Total Calls, (Umpire View)")+
    xlim(-3, 3)+
    ylim(-1, 6) + xlab(" ")+ ylab(" ")+
    theme_minimal() +  # Start with a minimal theme
  theme(
    panel.background = element_rect(fill = "white"),  # Set the background color to white
    panel.grid.major = element_line(color = "lightgrey"),  # Set major grid lines to light grey
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

plot1 <- plot1 + geom_density_2d(data = subset(df, PitchCall == "StrikeCalled"), aes(x = PlateLocSide, y = PlateLocHeight), h = c(h_x, h_y), fill = "red")+
geom_contour(
  data = subset(baseball_data, PitchCall == "StrikeCalled"),
  aes(x = PlateLocSide, y = PlateLocHeight, z = stat(level)),  # Use stat(level) to specify the contour levels
  breaks = 0.5,  # Set the probability level to 0.5
  color = "red",  # Color for contour lines
  size = 1  # Adjust the size of contour lines
)+ scale_fill_identity()
plot1

output_filename <- paste0(date, "_UmpireReport.pdf")
pdf(output_filename)
grid.table(stats)
print(plot1)

for(team in unique(df$BatterTeam)){
    filtered_data <- df %>% filter(BatterTeam == team)
    plot2 <- ggplot(filtered_data, aes(x = -1*PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = 0.25, yend = 0.25), color = "black", size = 1)+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = 0.25, yend = 0), color = "black", size = 1)+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = 0.25, yend = 0), color = "black", size = 1)+
 geom_segment(aes(x = min_plate_x, xend = 0, y = 0, yend = -0.25), color = "black", size = 1)+
 geom_segment(aes(x = 0, xend = max_plate_x, y = -0.25, yend = 0), color = "black", size = 1)+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = paste("Calls with", team, "at Bat, (Umpire View)"))+
    xlim(-3, 3)+
    ylim(-1, 6)+coord_fixed(ratio = 1) + xlab(" ")+ ylab(" ")+
     theme_minimal() +  # Start with a minimal theme
  theme(
    panel.background = element_rect(fill = "white"),  # Set the background color to white
    panel.grid.major = element_line(color = "lightgrey"),  # Set major grid lines to light grey
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

 print(plot2)

 for(batter_hand in unique(filtered_data$BatterSide)){
    data <- filtered_data %>% filter(BatterSide == batter_hand)
    plot3 <- ggplot(data, aes(x = -1*PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = 0.25, yend = 0.25), color = "black", size = 1)+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = 0.25, yend = 0), color = "black", size = 1)+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = 0.25, yend = 0), color = "black", size = 1)+
 geom_segment(aes(x = min_plate_x, xend = 0, y = 0, yend = -0.25), color = "black", size = 1)+
 geom_segment(aes(x = 0, xend = max_plate_x, y = -0.25, yend = 0), color = "black", size = 1)+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = paste("Calls with", team, "at Bat,", batter_hand, "Handed Hitters, (Umpire View)"))+
    xlim(-3, 3)+
    ylim(-1, 6)+coord_fixed(ratio = 1)+ xlab(" ")+ ylab(" ")+
      theme_minimal() +  # Start with a minimal theme
  theme(
    panel.background = element_rect(fill = "white"),  # Set the background color to white
    panel.grid.major = element_line(color = "lightgrey"),  # Set major grid lines to light grey
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )


 print(plot3)

 }
 for(pitch_type in unique(filtered_data$TaggedPitchType)){
 for(pitcher_hand in unique(filtered_data$PitcherThrows)){
 type <- filtered_data %>% filter(TaggedPitchType == pitch_type, PitcherThrows == pitcher_hand)
 plot4 <- ggplot(type, aes(x = -1*PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = 0.25, yend = 0.25), color = "black", size = 1)+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = 0.25, yend = 0), color = "black", size = 1)+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = 0.25, yend = 0), color = "black", size = 1)+
 geom_segment(aes(x = min_plate_x, xend = 0, y = 0, yend = -0.25), color = "black", size = 1)+
 geom_segment(aes(x = 0, xend = max_plate_x, y = -0.25, yend = 0), color = "black", size = 1)+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = paste("Calls with", team, "at Bat,", pitcher_hand, "Handed Pitchers", pitch_type, "(Umpire View)"))+
    xlim(-3, 3)+
    ylim(-1, 6)+
    coord_fixed(ratio = 1)+ xlab(" ") + ylab(" ")+
     theme_minimal() +  # Start with a minimal theme
  theme(
    panel.background = element_rect(fill = "white"),  # Set the background color to white
    panel.grid.major = element_line(color = "lightgrey"),  # Set major grid lines to light grey
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )


 print(plot4)


 }
 }
}
dev.off()


pdf_path <- paste0(Cluster_LocalDirectory,"/", output_filename)
file_path <- pdf_path

# Create a new folder or specify an existing folder in Google Drive where you want to upload the PDF
#folder_id <- "Umpire Reports"

# Upload the PDF file to Google Drive
#drive_upload(media = file_path, name = output_filename, path = "Trackman_Reports/Umpire_Reports")
