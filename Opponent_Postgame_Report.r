library(knitr)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(foreach)
library(gridExtra)
library(dplyr)
library(grid)
library(gridExtra)
library(googledrive)

Cluster_LocalDirectory <- Sys.getenv("Cluster_LocalDirectory")
Cluster_FieldName <- Sys.getenv("Cluster_FieldName")
directory <- setwd(Cluster_LocalDirectory)
date <- 20230923
file <- paste0(date,"-", Cluster_FieldName,"-Private-1_unverified.csv")
df <- read.csv(file)
str(df)

#Create a unique AB of Game
df$PAofGame <- paste0(df$Inning, ".", df$PAofInning, ".", df$Top.Bottom)

#MLB Statcast Strike Zone
min_plate_x <- -0.83
max_plate_x <- 0.83
max_plate_z <- 3.92
min_plate_z <- 1.17

#Wall Position SetUp
wall_pos_1 = sin(acos(330/375))*375
wall_pos_2 = 400*cos(pi/4)
radius <- 95
origin_x <- 60.5 * cos(pi/4)
origin_y <- 60.5*cos(pi/4)
phi <- atan(origin_x/radius)

#Infield Arc Setup
n_points <- 200  # Number of points for the curve
theta <- seq(-phi, pi/2 + phi, length.out = n_points)  # Create theta values
x <- origin_x + radius * cos(theta)
y <- origin_x + radius * sin(theta)
curve_data <- data.frame(x = x, y = y)

#Adjusting batted ball data to match orientation where RF line is the x-axis and LF line is the y-axis
#df <- filter(df, BatterTeam %in% c(<TEAM>))
#df <- filter(df, PitchCall %in% c("StrikeSwinging", "Foul", "InPlay"))
df <- df %>% mutate(angle = (Bearing + 45)*(pi/180))
df <- df %>% mutate(ypos = Distance*cos(angle), xpos = Distance*sin(angle))


#Field Plot Setup
field <- ggplot()+
        geom_segment(aes(x = 0, xend = 0, y = 0, yend = 330), color = "black")+
        geom_segment(aes(x = 0, xend = 330, y = 0, yend = 0), color = "black")+
        geom_segment(aes(x = 0, xend = 90, y = 90, yend = 90), color = "black")+
        geom_segment(aes(x = 90, xend = 90, y = 0, yend = 90), color = "black")+    
        geom_segment(aes(x = 0, xend = wall_pos_1, y = 330, yend = 330), color = "black")+
        geom_segment(aes(x = 330, xend = 330, y = 0, yend = wall_pos_1), color = "black")+
        geom_segment(aes(x = wall_pos_1, xend = wall_pos_2, y = 330, yend = wall_pos_2), color = "black")+
        geom_segment(aes(x = wall_pos_2, xend = 330, y = wall_pos_2, yend = wall_pos_1), color = "black")+
        xlim(-45, 380)+
        ylim(-45, 380)+ coord_fixed(ratio = 1) +
        theme_minimal()

#Strike Zone Plot Setup
zone <- ggplot() + 
    geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
    geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
    geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
    geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
    xlim(-3, 3) +
    ylim(-2, 6)+
    coord_fixed(ratio = 1)+
    theme_minimal()

#Firter to desired pitching team
df <- filter(df, PitcherTeam %in% c("DAY_FLY"))

#Report Time
for (batter_id in unique(df$Batter)) {
  bf <- df %>% filter(Batter == batter_id)
  hardhit <- bf %>% filter(ExitSpeed >= 95)

  pitch_loc <- zone + geom_point(data = bf, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) + geom_point(data = hardhit, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType), size = 2)

  spray_loc <- field + geom_point(bf, aes(x = xpos, y = ypos, color = TaggedHitType))

  filename <- paste0(batter_id, "_SeriesReport.pdf")
  pdf(filename, height = 8.5, width = 11)
  print(pitch_loc)
  print(spray_loc)
  dev.off()

}



