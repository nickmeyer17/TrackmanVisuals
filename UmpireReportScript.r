library(knitr)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(foreach)
library(gridExtra)
library(dplyr)
library(grid)
library(gridExtra)

#Set Working Directory to Local, This is where the output .pdf will save to
setwd(Cluseter_LocalDirectory)


# Get the current system date
current_date <- Sys.Date()

# Determine the previous day's date
previous_date <- current_date - 1

# Format the previous date as YYYYMMDD
date <- format(previous_date, "%Y%m%d")
field <- paste("-", Cluster_FieldName,"-Private-1_unverified.csv")
file <- paste0(date, field)
df <- read.csv(file)
str(df)

#MLB Statcast Strike Zone
min_plate_x <- -0.83
max_plate_x <- 0.83
max_plate_z <- 3.92
min_plate_z <- 1.17

df <- filter(df, PitchCall %in% c("BallCalled", "StrikeCalled"))

plot1 <- ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = "Game Total Calls")+
    xlim(-3, 3)+
    ylim(0, 6)


pdf("UmpireReport.pdf")
print(plot1)

for(team in unique(df$BatterTeam)){
    filtered_data <- df %>% filter(BatterTeam == team)
    plot2 <- ggplot(filtered_data, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = paste("Calls with", team, "at Bat"))+
    xlim(-3, 3)+
    ylim(0, 6)

 print(plot2)

 for(batter_hand in unique(filtered_data$BatterSide)){
    data <- filtered_data %>% filter(BatterSide == batter_hand)
    plot3 <- ggplot(data, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = paste("Calls with", team, "at Bat,", batter_hand, "Handed Hitters"))+
    xlim(-3, 3)+
    ylim(0, 6)

 print(plot3)

 }
 for(pitch_type in unique(filtered_data$TaggedPitchType)){
 for(pitcher_hand in unique(filtered_data$PitcherThrows)){
 type <- filtered_data %>% filter(TaggedPitchType == pitch_type, PitcherThrows == pitcher_hand)
 plot4 <- ggplot(type, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = paste("Calls with", team, "at Bat,", pitcher_hand, "Handed Pitchers", pitch_type))+
    xlim(-3, 3)+
    ylim(0, 6)

 print(plot4)
 }
 }
}

dev.off()
