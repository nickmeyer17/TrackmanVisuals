setwd(<LOCAL DIRECTORY>)

library(knitr)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(foreach)
library(gridExtra)
library(dplyr)
library(grid)
library(gridExtra)

date <- <DATE>
file <- paste0(date,"-<FieldName>-Private-1_unverified.csv")
df <- read.csv(file)


#Wall Position SetUp
wall_pos_1 = sin(acos(330/375))*375
wall_pos_2 = 400*cos(pi/4)
radius <- 95
origin_x <- 60.5 * cos(pi/4)
origin_y <- 60.5*cos(pi/4)
phi <- atan(origin_x/radius)

n_points <- 200  # Number of points for the curve
theta <- seq(-phi, pi/2 + phi, length.out = n_points)  # Create theta values
x <- origin_x + radius * cos(theta)
y <- origin_x + radius * sin(theta)
curve_data <- data.frame(x = x, y = y)

#Filter data to swings
df <- filter(df, BatterTeam %in% c("DAY_FLY"))
df <- filter(df, PitchCall %in% c("StrikeSwinging", "Foul", "InPlay"))
df <- df %>% mutate(angle = (Bearing + 45)*(pi/180))
df <- df %>% mutate(xpos = Distance*cos(angle), ypos = Distance*sin(angle))


str(df)
for(batter in unique(df$Batter)){
    pf <- df %>% filter(Batter == batter)

    
    avg_stats <- pf %>% summarise(
        AvgExitVelo = round(mean(ExitSpeed, na.rm = TRUE), 2),
        MaxEV = round(max(ExitSpeed, na.rm = TRUE)),
        AvgLaunchAngle = round(mean(Angle, na.rm = TRUE), 2),
        AvgDistance = round(mean(Distance, na.rm = TRUE), 0),
        MaxDistance = round(max(Distance, na.rm = TRUE),2),
        Bearing = round(mean(Bearing, na.rm = TRUE),2)
        
    )

    stats <- pf %>% group_by(TaggedPitchType) %>% summarise(
        ExitVelo = round(mean(ExitSpeed, na.rm = TRUE), 2),
        MaxEV = round(max(ExitSpeed)),
        LaunchAngle = round(mean(Angle, na.rm = TRUE), 2),
        Distance = round(mean(Distance, na.rm = TRUE), 0),
        MaxDistance = round(max(Distance, na.rm = TRUE),2),
        Bearing = round(mean(Bearing, na.rm = TRUE,2))
    )
    spray <- ggplot(pf, aes(x = xpos, y = ypos, color = TaggedHitType)) +
        geom_point()+
        labs(title = paste(batter,"Spray Chart"))
    spray <- spray +
        geom_segment(aes(x = 0, xend = 0, y = 0, yend = 330), color = "black")+
        geom_segment(aes(x = 0, xend = 330, y = 0, yend = 0), color = "black")+
        geom_segment(aes(x = 0, xend = 90, y = 90, yend = 90), color = "black")+
        geom_segment(aes(x = 90, xend = 90, y = 0, yend = 90), color = "black")+    
        geom_segment(aes(x = 0, xend = wall_pos_1, y = 330, yend = 330), color = "black")+
        geom_segment(aes(x = 330, xend = 330, y = 0, yend = wall_pos_1), color = "black")+
        geom_segment(aes(x = wall_pos_1, xend = wall_pos_2, y = 330, yend = wall_pos_2), color = "black")+
        geom_segment(aes(x = wall_pos_2, xend = 330, y = wall_pos_2, yend = wall_pos_1), color = "black")+
        xlim(-45, 380)+
        ylim(-45, 380)+
        scale_color_manual(values = c("FlyBall" = "#FF0000", "GroundBall" = "#0000FF", "Popup" = "#33CC33", "LineDrive"= "#FF6600", "Bunt" = "#330033", "Spliter" = "#00CCFF", "Cutter" = "#CC0099", "Knuckelball" = "#FFFF00", "Other" = "#003300", "Undefined" = "#333333"))
spray <- spray +
  geom_path(data = curve_data, aes(x, y), color = "black")
spray <- spray + coord_fixed(ratio = 1)

plot1 <- ggplot(pf, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType))+geom_point()+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
 geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
 scale_color_manual(values = c("Fastball" = "#FF0000", "Curveball" = "#0000FF", "Slider" = "#33CC33", "ChangeUp"= "#FF6600", "Sinker" = "#330033", "Spliter" = "#00CCFF", "Cutter" = "#CC0099", "Knuckelball" = "#FFFF00", "Other" = "#003300", "Undefined" = "#333333"))+
 labs(title = paste("Swings for", batter))+
    xlim(-3, 3)+
    ylim(0, 6)
plot1 <- plot1 + coord_fixed(ratio = 1)

    # Customize color and size as needed

# Split the names into "First" and "Last" components
name_split <- strsplit(pf$Batter, ", ")

# Combine the "First" and "Last" components in the desired order (FirstLast)
name <- sapply(name_split, function(name) paste(rev(name), collapse = ""))


filename <- paste0(date, "_", name, "_HittingGameReport.pdf")

    pdf(filename, height = 8.5, width = 11)
    grid.newpage()
  # Print the statistics table\
    grid.table(avg_stats)

   grid.newpage()
   grid.table(stats)
    print(spray)
    print(plot1)
    
    dev.off()
}
