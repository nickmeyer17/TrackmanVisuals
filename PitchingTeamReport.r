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

df <- df %>% mutate(PAofGame = cumsum(PAofInning))

df <- filter(df, PitcherTeam %in% c("DAY_FLY"))
df <- df %>% mutate(is_strike = ifelse(PitchCall %in% c("StrikeCalled", "InPlay", "StrikeSwinging", "FoulBall"), 1, 0))
df <- df %>% mutate(strike_count = Strikes + is_strike)
first_pitch <- filter(df, PitchofPA == 1)
#first_pitch <- first_pitch %>% mutate(fp_str = ifelse(PitchCall %in% c("StrikeCalled", "InPlay", "StrikeSwinging", "FoulBall"), TRUE, FALSE))

strike_per <- df %>% group_by(Pitcher) %>% summarize(stike_per = round(mean(is_strike, na.rm = 1)*100, 1))
first_pitch <- first_pitch %>% group_by(Pitcher) %>% summarize(first_pitch_strike = round(mean(is_strike, na.rm = 1)*100, 1))
stats <- merge(strike_per, first_pitch, by = "Pitcher")


first_three <- filter(df, PitchofPA == 3)
first_three <- first_three %>% mutate(success = ifelse(strike_count == 2, TRUE, FALSE))

first_three_success <- first_three %>% group_by(Pitcher) %>% summarize(first_three_success = round(mean(success, na.rm = TRUE)*100, 1))
stats <- merge(stats, first_three_success, by = "Pitcher")
str(stats)
df <- df %>% mutate(in_play = ifelse(PitchCall %in% c("InPlay") | KorBB %in% c("Walk", "Strikeout"), TRUE, FALSE))
action_play <- filter(df, in_play %in% c("TRUE"))

df <- action_play %>% mutate(on_base = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")| KorBB %in% c("Walk")| PitchCall %in% c("HitByPitch"), TRUE, FALSE))
df <- df %>% select(Pitcher, PAofGame, on_base)

success_first_three <- (first_three, success %in% c("TRUE"))

df <- df %>% select(Pitcher, PAofGame, )
f3 <- success_first_three %>% select(Pitcher, PAofGame)

if(df$PAofGame == f3$PAofGame){
    sf <- merge(df, f3, by = "Pitcher", "PAofGame")
}


#f3_obp <- sf %>% group_by(Pitcher) %>% summarize(
    obp_w_succesful_first_three = round(mean(on_base, na.rm = TRUE), 3)
#)

#stats <- merge(stats, f3_obp, by = "Pitcher")

output_filename <- paste0(date, "_PitchingKPIReport.pdf")
pdf(output_filename)
grid.table(stats)

dev.off()