library(dplyr)
library(tidyr)
library(dbr)
library(stringr)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

pred_p <- read.csv('./data/external/yahoo_pred_pitchers.csv', stringsAsFactors = F) %>%
  filter(player != 'Shohei Ohtani (Pitcher)') %>%
  mutate(pos = case_when(
    pos=='P'~'RP',
    T~pos
  ))

pred_b <- read.csv('./data/external/yahoo_pred_batters.csv') %>%
  rename('own_rate'='X..owned')

score_players <- function(df, stat_names){
  stat_scores <- df %>%
    select(player, pos, stat_names) %>%
    gather('stat', 'value', -player, -pos) %>%
    group_by(stat) %>%
    mutate(
      value_01 = ifelse(stat %in% c('ERA', 'WHIP'), 1-range01(value),range01(value)),
      stat_score = rescale(value_01, to=c(50, 100))
    ) %>%
    group_by() %>%
    mutate(stat = paste0('score_', stat)) %>%
    select(-value_01, -value) %>%
    spread(key=stat, value=stat_score) %>%
    mutate(score = sqrt(score_AVG^2 + score_R^2 + score_HR^2 + score_RBI^2 + score_SB^2),
           score = rescale(score, to=c(50, 100)))
    
    
  df %>%
    select(player, pos, stat_names) %>%
    gather('stat', 'value', -player, -pos) %>%
    group_by(stat) %>%
    mutate(
      value_01 = ifelse(stat %in% c('ERA', 'WHIP'), 1-range01(value),range01(value)),
      stat_score = rescale(value_01, 100, 50)
    ) %>%
    group_by(player) %>%
    summarise(
      score = sqrt(sum(value_01^2)),
      pos = pos[1]
    ) %>%
    mutate(
      raw_score = score,
      score = round(rescale(score, c(50, 100)), 1)
    )
}

positions <- c("RP", "SP", "OF", "2B", "SS", "1B", "3B", "C", "Util")
pitching_stats <- c('K', 'W', 'SV','ERA', 'WHIP')
batting_stats <- c('R', 'HR', 'SB', 'RBI', 'AVG')

pitcher_scores <- score_players(pred_p, pitching_stats) %>%
  mutate(pos=ifelse(pos!='P', paste(pos,'P',sep=',')))
batter_scores <- score_players(pred_b,batting_stats) %>%
  mutate(pos=ifelse(pos!='pos', paste(pos,'Util',sep=',')))
player_scores <- rbind(pitcher_scores, batter_scores) %>%
  mutate(score = round(rescale(raw_score, c(50, 100)), 1),
         score_rank = min_rank(-score))
