library(dplyr)
library(tidyr)
library(dbr)
library(stringr)
library(scales)


positions <- c("RP", "SP", "OF", "2B", "SS", "1B", "3B", "C", "Util")
pitching_stats <- c('K', 'W', 'SV','ERA', 'WHIP')
batting_stats <- c('R', 'HR', 'SB', 'RBI', 'AVG')

get_data <- function(){
  pred_p <- read.csv('./data/external/yahoo_pred_pitchers.csv', stringsAsFactors = F, fileEncoding="UTF-8-BOM") %>%
    filter(player != 'Shohei Ohtani (Pitcher)',
           row_number() <= 180) %>%
    dplyr::rename(ownership = 1) %>%
    select(player, pos, pitching_stats) %>%
    gather('stat', 'value', -player, -pos) %>%
    group_by(stat) %>%
    mutate(
      stat_score = rescale(value, to=c(50, 100))
    ) %>%
    ungroup() %>%
    mutate(stat_score = ifelse(stat %in% c('ERA', 'WHIP'), 150 - stat_score, stat_score),
           stat = paste0('score_', stat)) %>%
    select(-value) %>%
    spread(key=stat, value=stat_score) %>%
    mutate(raw_score = sqrt(score_ERA^2 + score_K^2 + score_SV^2 + score_W^2 + score_WHIP^2),
           score = rescale(raw_score, to=c(50, 100)),
           pos=ifelse(pos=='P', pos, paste0(pos, ',P')))
  
  pred_b <- read.csv('./data/external/yahoo_pred_batters.csv', stringsAsFactors = F, fileEncoding="UTF-8-BOM") %>%
    rename(ownership=1) %>%
    filter(row_number() <= 180) %>%
    select(player, pos, batting_stats) %>%
    gather('stat', 'value', -player, -pos) %>%
    group_by(stat) %>%
    mutate(stat_score = rescale(value, to=c(50, 100))) %>%
    ungroup() %>%
    mutate(stat = paste0('score_', stat)) %>%
    select(-value) %>%
    spread(key=stat, value=stat_score) %>%
    mutate(raw_score = sqrt(score_AVG^2 + score_R^2 + score_HR^2 + score_RBI^2 + score_SB^2),
           score = rescale(raw_score, to=c(50, 100)),
           pos=ifelse(grepl('Util', pos), pos, paste0(pos, ',Util')))
  
  players <- bind_rows(pred_b, pred_p) %>%
    mutate(score = rescale(raw_score, to=c(50, 100)))
  players <- players[,c(1, 2, 9, 8, 3:7, 10:14)]
}

players <- get_data()

vorp <- players %>%
  separate_rows(pos, sep=',') %>%
  group_by(pos) %>% 
  arrange(desc(score)) %>%
  mutate(vor = score - score[case_when(
    pos %in% c('OF', 'SP') ~ 3 * 12 + 12,
    pos %in% c('RP')       ~ 2 * 12 + 6,
    T                      ~ 1 * 12 + 3
  )],
    pos_rank = row_number()
  ) %>%
  ungroup() %>%
  arrange(desc(vor)) %>%
  mutate(ovr_rank = row_number()) %>%
  select(player, pos, vor, pos_rank, ovr_rank, score)
