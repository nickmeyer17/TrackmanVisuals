directory <- setwd(<LOCAL DIRECTORY>)
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


date <- 20230923
file <- paste0(date,"-<FieldName>-Private-1_unverified.csv")
df <- read.csv(file)
str(df)

#MLB Statcast Strike Zone
min_plate_x <- -0.83
max_plate_x <- 0.83
max_plate_z <- 3.92
min_plate_z <- 1.17




df <- df %>%
  mutate(is_strike = ifelse(PitchCall %in% c("StrikeCalled", "InPlay", "StrikeSwinging", "FoulBall"), TRUE, FALSE),
  whiff = ifelse(PitchCall %in% c("StrikeSwinging"), TRUE, FALSE))



df <- df %>% mutate(
    HB = c(HorzBreak),
    IVB = c(InducedVertBreak)
)

df <- filter(df, PitcherTeam %in% c(<TEAM>))
str(df)

# Group the data by PitchType
#grouped_data <- df %>% group_by(TaggedPitchType)
# Load the required libraries
library(knitr)
library(kableExtra)

# Iterate over each PitcherId
for (pitcher_id in unique(df$Pitcher)) {
    # Filter the data for the current PitcherId
  filtered_data <- df %>% filter(Pitcher == pitcher_id)
    stats <- filtered_data %>% group_by(TaggedPitchType) %>% summarise(
        PitchSpeed = round(mean(RelSpeed, na.rm = TRUE), 2),
        IndVertBreak = round(mean(IVB, na.rm = TRUE), 2),
        HorzBreak = round(mean(HB, na.rm = TRUE), 2),
        SpinRate = round(mean(SpinRate, na.rm = TRUE), 0),
        Extension = round(mean(Extension, na.rm = TRUE),2),
      StirkePercent = round((mean(is_strike)*100),2),
      WhiffRate = round((mean(whiff)*100),2)
    )



  
  # Create the ggplot for PitchType by IVB and HB
  plot1 <- ggplot(filtered_data, aes(x = HB, y = IVB, color = TaggedPitchType)) +
    geom_point(size = 2) +
    labs(title = "Pitch Movement") +
    xlim(-25, 25) +
    ylim(-25, 25) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, color = "black") +
    scale_color_manual(values = c("Fastball" = "#FF0000", "Curveball" = "#0000FF", "Slider" = "#33CC33", "ChangeUp"= "#FF6600", "Sinker" = "#330033", "Spliter" = "#00CCFF", "Cutter" = "#CC0099", "Knuckelball" = "#FFFF00", "Other" = "#003300", "Undefined" = "#333333"))

  # Create the ggplot for ReleasePositionX and ReleasePositionZ
  plot2 <- ggplot(filtered_data, aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
    geom_point(size = 2) +
    labs(title = "Release Point") +
    geom_segment(aes(x = -1, xend = 1, y = 10/12, yend = 10/12), color = "black")+
    xlim(-4, 4) +
    ylim(0, 8)+
    xlaB("Centerfield View")+
    scale_color_manual(values = c("Fastball" = "#FF0000", "Curveball" = "#0000FF", "Slider" = "#33CC33", "ChangeUp"= "#FF6600", "Sinker" = "#330033", "Spliter" = "#00CCFF", "Cutter" = "#CC0099", "Knuckelball" = "#FFFF00", "Other" = "#003300", "Undefined" = "#333333"))

plot3 <- ggplot(filtered_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
    geom_point(size = 5) +
    labs(title = "Pitch Locations, Pitcher's View") +
    geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = max_plate_z, yend = max_plate_z), color = "black")+
    geom_segment(aes(x = min_plate_x, xend = max_plate_x, y = min_plate_z, yend = min_plate_z), color = "black")+
    geom_segment(aes(x = min_plate_x, xend = min_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
    geom_segment(aes(x = max_plate_x, xend = max_plate_x, y = min_plate_z, yend = max_plate_z), color = "black")+
    xlim(-3, 3) +
    ylim(-2, 6)+
    xlab("Pitcher View")+
    scale_color_manual(values = c("Fastball" = "#FF0000", "Curveball" = "#0000FF", "Slider" = "#33CC33", "ChangeUp"= "#FF6600", "Sinker" = "#330033", "Spliter" = "#00CCFF", "Cutter" = "#CC0099", "Knuckelball" = "#FFFF00", "Other" = "#003300", "Undefined" = "#333333"))


# Split the names into "First" and "Last" components
name_split <- strsplit(filtered_data$Pitcher, ", ")

# Combine the "First" and "Last" components in the desired order (FirstLast)
name <- sapply(name_split, function(name) paste(rev(name), collapse = ""))

  # Create a PDF file for the current PitcherId
  pdf_file <- paste0(date, "_", name, "_pitchingreport.pdf")
  pdf(pdf_file, height = 8.5, width = 11)
  
  grid.newpage()
  # Print the statistics table\
  grid.table(stats)

#  grid.newpage()
 # grid.table(adv_stats)
  # Print the plots
  print(plot1)
  print(plot2)
  print(plot3)
  
  # Close the PDF device
  dev.off()

  
}
