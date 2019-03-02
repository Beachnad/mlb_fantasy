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
    filter(player != 'Shohei Ohtani (Pitcher)' & row_number() <= 180) %>%
    dplyr::rename(ownership = 1) %>%
    select(player, pos, pitching_stats, GP) %>%
    gather('stat', 'value', -player, -pos, -GP) %>%
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
    mutate(pos=ifelse(grepl('Util', pos), pos, paste0(pos, ',Util'))) %>%
    filter(row_number() <= 180 | grepl('C', pos)) %>%
    select(player, pos, GP, batting_stats) %>%
    gather('stat', 'value', -player, -pos, -GP) %>%
    group_by(stat) %>%
    mutate(stat_score = rescale(value, to=c(50, 100))) %>%
    ungroup() %>%
    mutate(stat = paste0('score_', stat)) %>%
    select(-value) %>%
    spread(key=stat, value=stat_score) %>%
    mutate(raw_score = sqrt(score_AVG^2 + score_R^2 + score_HR^2 + score_RBI^2 + score_SB^2),
           score = rescale(raw_score, to=c(50, 100)))
  
  players <- bind_rows(pred_b, pred_p) %>%
    mutate(score = rescale(raw_score, to=c(50, 100)))
  players <- players[,c(1, 2, 3, 10, 9, 4:8, 11:15)]
}

player_data <- get_data()

replacement_player_data <- player_data %>%
  separate_rows(pos, sep=',') %>%
  filter(pos!='SP' | score > 65 | GP <= 30) %>%
  group_by(pos) %>% 
  arrange(desc(score)) %>%
  filter(abs(row_number() - case_when(
    pos %in% c('Util')      ~ 8 * 12 + 24,
    pos %in% c('P')         ~ 7 * 12 + 16,
    pos %in% c('OF', 'SP')  ~ 3 * 12 + 12,
    pos %in% c('RP')        ~ 2 * 12 + 6,
    pos == 'C'              ~ 1 * 12 + 1,
    T                       ~ 1 * 12 + 3
  ))<=4) %>%
  gather('stat','value', -player,-pos) %>%
  group_by(pos, stat) %>%
  summarise(value=mean(value)) %>%
  spread(stat, value) %>%
  ungroup() %>%
  mutate(player = paste0('(', pos, ') Replacement'))

full_rep <- replacement_player_data %>%
  mutate(
    pos_cnt = case_when(
      pos %in% c('SP', 'OF', 'P') ~ 3,
      pos %in% c('RP')            ~ 2,
      T                           ~ 1
    )
  )
full_rep <- 
  full_rep[rep(row.names(full_rep), full_rep$pos_cnt), names(full_rep)] %>%
  group_by(pos) %>%
  mutate(player = paste(player, row_number()))
  


player_data.rep <- bind_rows(
  select(player_data, -raw_score),
  mutate(replacement_player_data, player=paste0('(', pos, ') Replacement'))
 ) 
# %>%
#   bind_rows(
#     player = c('(OF) Replacement 1',
#                '(OF) Replacement 2',
#                '(OF) Replacement 3', 
#                '(SP) Replacement 1',
#                '(SP) Replacement 2',
#                '(SP) Replacement 3',
#                '(RP) Replacement 1',
#                '(RP) Replacement 2',
#                '(P) Replacement 1',
#                '(P) Replacement 2',
#                '(P) Replacement 3'),
#     pos=c()
#   )

vorp <- player_data %>%
  separate_rows(pos, sep=',') %>%
  select(player, pos, score) %>%
  bind_rows(replacement_player_data) %>%
  left_join(select(replacement_player_data, pos, score), by='pos', suffix=c('', '.rep')) %>%
  mutate(vor = score - score.rep) %>%
  arrange(desc(vor)) %>%
  group_by(pos) %>%
  mutate(pos_rank = row_number()) %>%
  ungroup() %>%
  mutate(ovr_rank = row_number()) %>%
  select(player, pos, vor, pos_rank, ovr_rank, score)


vorp.rep <- player_data.rep %>%
  separate_rows(pos, sep=',') %>%
  select(player, pos, score, GP) %>%
  bind_rows(replacement_player_data) %>%
  left_join(select(replacement_player_data, pos, score), by='pos', suffix=c('', '.rep')) %>%
  mutate(vor = score - score.rep) %>%
  arrange(desc(vor)) %>%
  group_by(pos) %>%
  mutate(pos_rank = row_number()) %>%
  ungroup() %>%
  mutate(ovr_rank = row_number()) %>%
  select(player, pos, vor, pos_rank, ovr_rank, score, GP)


