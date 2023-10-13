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
date <- 20231008
file <- paste0(date,"-", Cluster_FieldName,"-Private-1_unverified.csv")
df <- read.csv(file)
str(df)

df <- filter(df, PitcherTeam %in% c("DAY_FLY"))
df$PAofGame <- paste0(df$Inning, ".", df$PAofInning, ".", df$Top.Bottom)

df <- df %>% mutate(is_strike = ifelse(PitchCall %in% c("StrikeCalled", "InPlay", "StrikeSwinging", "FoulBall"), 1, 0))
df <- df %>% mutate(strike_count = Strikes + is_strike)
first_pitch <- filter(df, PitchofPA == 1)
#first_pitch <- first_pitch %>% mutate(fp_str = ifelse(PitchCall %in% c("StrikeCalled", "InPlay", "StrikeSwinging", "FoulBall"), TRUE, FALSE))

first_pitch_strike <- paste("First Pitch Strike")
strike_per <- paste("Strike %")

strike_per <- df %>% group_by(Pitcher) %>% summarize(strike_percent = round(mean(is_strike, na.rm = 1)*100, 1))
first_pitch <- first_pitch %>% group_by(Pitcher) %>% summarize(first_pitch_strike = round(mean(is_strike, na.rm = 1)*100, 1))
stats <- merge(strike_per, first_pitch, by = "Pitcher")

df$successful_first_three <- logical(nrow(df))

for(PA in unique(df$PAofGame)){
   #sf <- df %>% filter(PAofGame == PA)
   #sf$successful_first_three <- logical(nrow(sf))
for (i in 1:nrow(df)) {
  if (df$strike_count[i] >= 2 && df$Balls[i] <= 1) {
    df$success[i] <- TRUE
  } else {
    df$success[i] <- FALSE
  }
}
}
sf <- df %>% select(Pitcher, PAofGame, PitchofPA, success, Balls, strike_count)
sf
sf <- sf %>% group_by(PAofGame) %>% filter(PitchofPA == max(PitchofPA[PitchofPA < 3 | PitchofPA == 3]))
sf


f3_success <- sf %>% group_by(Pitcher) %>% summarize(f_2of3_success = round(mean(success, na.rm = TRUE)*100, 1))
stats <- merge(stats, f3_success, by = "Pitcher")

sf <- sf %>% select(Pitcher, PAofGame, success)

df <- df %>% mutate(in_play = ifelse(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Walk", "Strikeout"), TRUE, FALSE))
action_pitch <- filter(df, in_play %in% c("TRUE"))

action <- action_pitch %>% mutate(on_base = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")| KorBB %in% c("Walk")| PitchCall %in% c("HitByPitch"), TRUE, FALSE))
action <- action %>% select(Pitcher, PAofGame, on_base)

str(action)
str(sf)

data <- merge(action, sf, by = c("Pitcher", "PAofGame"))
str(data)
data <- filter(data, success %in% c("TRUE"))

f3_obp <- data %>% group_by(Pitcher) %>% summarize(
    obp_w_succesful_first_three = round(mean(on_base, na.rm = TRUE), 3)
)

stats <- merge(stats, f3_obp, by = "Pitcher")

output_filename <- paste0(date, "_PitchingKPIReport.pdf")


pdf(output_filename, height = 8.5, width = 11)
grid.table(stats)
print(stats)
dev.off()

