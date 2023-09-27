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
setwd(<LOCAL DIRECTROY>)

#INPUT DATE BELOW IN YYYYMMDD FORMAT
date <- <DATE>
file <- paste0(date,"-<FieldName>-Private-1_unverified.csv")
sf <- read.csv(file)


#MLB Statcast Strike Zone
min_plate_x <- -0.83
max_plate_x <- 0.83
max_plate_z <- 3.92
min_plate_z <- 1.17

sf <- filter(sf, PitchCall %in% c("BallCalled", "StrikeCalled"))


for(catcher in unique(sf$Catcher)){
    filtered_data <- sf %>% filter(catcher == Catcher)
    
plot1 <- ggplot(filtered_data, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = paste("Game Total Calls for", catcher))+
    xlim(-3, 3)+
    ylim(0, 6)

# Split the names into "First" and "Last" components
name_split <- strsplit(catcher, ", ")

# Combine the "First" and "Last" components in the desired order (FirstLast)
name <- sapply(name_split, function(name) name[1])

  # Create a PDF file for the current PitcherId
  pdf_file <- paste0(date, "_", name, "_catchingreport.pdf")
  pdf(pdf_file, height = 8.5, width = 11)
print(plot1)

for(pitch_type in unique(filtered_data$TaggedPitchType)){
   type <- filtered_data %>% filter(TaggedPitchType == pitch_type)
 for(pitcher_hand in unique(type$PitcherThrows)){
 hand <- filtered_data %>% filter(PitcherThrows == pitcher_hand)
 plot4 <- ggplot(hand, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = paste("Called pitches with", pitcher_hand, "Handed Pitchers", pitch_type, "for", catcher))+
    xlim(-3, 3)+
    ylim(0, 6)

 print(plot4)
 }
  for(batter_hand in unique(filtered_data$BatterSide)){
    data <- filtered_data %>% filter(BatterSide == batter_hand)
    plot3 <- ggplot(data, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 scale_color_manual(values = c("BallCalled" = "blue", "StrikeCalled" = "red"))+
 labs(title = paste("Called pitches with,", batter_hand, "Handed Hitters,", pitch_type, "for", catcher))+
    xlim(-3, 3)+
    ylim(0, 6)

 print(plot3)

 }
}
dev.off()
}


