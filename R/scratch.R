library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(dbr)
library(stringr)
library(scales)
library(combinat)

Sys.setenv("plotly_username"="beachnad")
Sys.setenv("plotly_api_key"="Lcndgv6SmYFIJ7xhUalr")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

p_data <- read.csv('./data/external/pitching_data.csv') %>%
  mutate(
    kind = ifelse(IP/G>=2.5,'SP', 'RP')
  )

pred_p <- read.csv('./data/external/yahoo_pred_pitchers.csv', stringsAsFactors = F) %>%
  filter(player != 'Shohei Ohtani (Pitcher)') %>%
  mutate(pos = case_when(
    pos=='P'~'RP',
    T~pos
  ))

pred_b <- read.csv('./data/external/yahoo_pred_batters.csv') %>%
  rename('own_rate'='X..owned')

score_players <- function(df, stat_names){
  df %>%
    select(player, pos, stat_names) %>%
    gather('stat', 'value', -player, -pos) %>%
    group_by(stat) %>%
    mutate(
      value_01 = ifelse(stat %in% c('ERA', 'WHIP'), 1-range01(value),range01(value))
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

positions <- c("RP", "SP", "OF", "2B", "SS", "1B", "3B", "C", "Util")

starting_OF <- 3 * 14
starting_1B <- 1 * 14
starting_2B <- 1 * 14
starting_3B <- 1 * 14
starting_SS <- 1 * 14
starting_C  <- 1 * 14
starting_SP <- 3 * 13
starting_RP <- 2 * 13
starting_BAT <- 9 * 14
starting_PIT <- 8 * 13



replacement_values <- player_scores %>%
  separate_rows(pos, sep=',') %>%
  rbind({filter(.,!grepl('P', pos)) %>%
      mutate(pos='Util')}) %>%
  rbind({filter(.,grepl('P', pos)) %>%
      mutate(pos='P')}) %>%
  group_by(pos) %>%
  arrange(desc(score)) %>%
  filter(row_number()==switch (pos[1],
                               OF=starting_OF,
                               `1B`=starting_1B,
                               `2B`=starting_2B,
                               `3B`=starting_3B,
                               SS=starting_SS,
                               C=starting_C,
                               SP=starting_SP,
                               RP=starting_RP,
                               P=starting_PIT,
                               Util=starting_BAT)
           )

VORP <- player_scores %>%
  separate_rows(pos, sep=',') %>%
  left_join(select(replacement_values, pos, repl_score=score), by='pos') %>%
  mutate(vorp=score-repl_score,
         ovr_vorp_rank=dense_rank(desc(vorp))) %>%
  group_by(pos) %>%
  mutate(pos_vorp_rank=dplyr::dense_rank(desc(vorp))) %>%
  arrange(desc(vorp)) %>%
  ungroup()
  
VORP$player[dense_rank(desc(VORP$vorp))<=5]


best_linup <- function(players){
  lineup <- tibble(
    pos = c('C', '1B', '2B', '3B', 'SS',
            'OF', 'OF', 'OF', 'Util', 
            'SP', 'SP', 'SP', 'RP', 'RP',
            'P', 'P', 'P',
            'BN', 'BN', 'BN', 'BN', 'BN')
  ) %>%
    group_by(pos) %>%
    mutate(pos_num=1:n())
  
  player_data <- filter(player_scores, player %in% players) %>%
    separate_rows(pos, sep=',') %>%
    left_join(select(VORP, player, pos, vorp), by=c('player', 'pos')) %>%
    group_by(pos) %>%
    mutate(rank=dense_rank(desc(vorp))) %>%
    ungroup()
  
  
  
  lineup %>% left_join(select(player_data, player, pos, rank, vorp),
                       by=c('pos'='pos', 'pos_num'='rank'))
  

  
}



my_lineup <- c(
  'Alex Bregman',   # 3B, SS
  'Mike Trout',     # OF
  'Anthony Rizzo',  # 1B
  'Matt Carpenter', # 1B, 2B, 3B
  'Danny Duffy'     # SP
)

roster <- filter(player_scores, player %in% my_lineup) %>%
  separate_rows(pos, sep=',') %>%
  left_join(select(VORP, player, pos, vorp), by=c('player', 'pos')) %>%
  group_by(pos) %>%
  mutate(rank=dense_rank(desc(vorp))) %>%
  ungroup()

lp()

library(arrangements)

arrangements::permutations(c(1, 2, 3))
arrangements::combinations(c(1, 1, 2, 3), k=2)

arrangements::combinations(k = 3, freq = c(2, 3))

tmp <- expand(player_data, nesting(player, pos, vorp), rank)

tibble(
  player = c(1, 1, 2, 3, 3, 4, 4, 5),
  pos    = c(10, 50, 20, 30, 40, 40, 50, 50),
  rank   = c(1, 1, 1, 1, 2, 1, 2, 3)
) %>%
  expand(nesting(player, rank), pos)
  
  group_by(pos) %>%
  expand(pos, rank, player)

paste(permn(c(1, 2, 3)), collapse = '_')

perm <- permn(c('1', '2', '3'))

experiment <- tibble(
  name = rep(c("Alex", "Robert", "Sam"), c(3, 2, 1)),
  trt  = rep(c("a", "b", "a"), c(3, 2, 1)),
  rep = c(1, 2, 3, 1, 2, 1),
  measurment_1 = runif(6),
  measurment_2 = runif(6)
)
# We can figure out the complete set of data with expand()
# Each person only gets one treatment, so we nest name and trt together:
all <- experiment %>% expand(nesting(name, trt), rep)
all

paste(sapply(perm, function(x){paste0(x, collapse =',')}), collapse = '_')

str(perm[[1]])
as.character(perm)

best_linup(my_lineup)

top_picks <- function(vorp_data, picked_players=c()){
  pick_data <- 
  
  vorp_data %>%
    filter(!(player %in% picked_players)) %>%
    select(player, score, pos, vorp) %>%
    mutate(ovr_vorp_rank = dense_rank(desc(vorp))) %>%
    group_by(pos) %>%
    mutate(pos_vorp_rank = dense_rank(desc(vorp))) %>%
    filter(pos_vorp_rank <= 5) %>%
    arrange(ovr_vorp_rank) %>%
    ungroup()
}

top_picks(VORP, c('Mike Trout'))

ggplot(p_data, aes(x=G, y=IP, color=Pitches<500)) +
  geom_point(alpha=0.1) +
  geom_abline(slope=2.5, intercept = 0) +
  theme_bw() %+replace%
  theme(panel.grid = element_blank())

ggplot(p_data, aes(x=WHIP, y=W, color=kind)) +
  geom_point(alpha=0.8) +
  theme_bw() %+replace%
  theme(panel.grid = element_blank())

hist(p_data$Pitches[p_data$kind=='RP'])

ggplot(p_data[p_data$kind=='SP',], aes(x=Pitches)) +
  geom_density()


df <- p_data %>%
  select(Season, WHIP, Name, Age, Pitches) %>%
  filter(Pitches>=1500, Age<=35) %>%
  group_by(Name) %>%
  arrange(Season) %>%
  mutate(next_WHIP = lead(WHIP),
         next_Pitches = lead(Pitches),
         diff_WHIP = WHIP - next_WHIP) %>%
  ungroup()

ggplot(filter(p_data, kind=='SP'), aes(x=GS)) +
  geom_histogram(fill=db_colors('basic')[['blue']], binwidth = 1) +
  db_dark_theme()



p <- plot_ly(data = p_data, x = ~G, y = ~IP, color=~kind,
             text = ~paste("Player: ", Name, '<br>Season:', Season))

chart_link = api_create(p, filename="GIP_Stats")
chart_link

p

# Loking at predicted pitching data =================
pred_p <- read.csv('./data/external/yahoo_pred_pitchers.csv', stringsAsFactors = F) %>%
  filter(player != 'Shohei Ohtani (Pitcher)') %>%
  mutate(pos = case_when(
    pos=='P'~'RP',
    T~pos
  ))

ggplot(filter(pred_p, pos=='RP'), aes(x=GP)) +
  geom_histogram(fill=db_colors('basic')[['blue']], binwidth = 1) +
  db_dark_theme()
30/162
62/162

sp <- pred_p[pred_p$pos=='SP',]
rp <- pred_p[pred_p$pos=='RP',]

mean(sp$K/162) #
mean(rp$K/162)

mean(sp$W/162) #
mean(rp$W/162)

mean(sp$SV/162)
mean(rp$SV/162) #

mean(sp$ERA/162)
mean(rp$ERA/162) #

mean(sp$WHIP/162)
mean(rp$WHIP/162) #

nrow(sp)
nrow(rp)

ggplot(pred_p, aes(x=WHIP, color=pos)) +
  geom_density() +
  scale_color_brewer(name='Position',palette = 'Spectral') +
  scale_y_continuous(expand = expand_scale(mult=c(-0.005, 0.05), 0)) +
  scale_x_continuous(expand=c(-0.005, 0)) +
  labs(title='Predicted WHIP Density',
       subtitle="Predicted Player WHIP Distribution by Position",
       y='Density',
       x='WHIP (Lower is Better)',
       caption=str_wrap("Height of the line represents the relative number of players 
                         who are predicted to acheive the WHIP on the x-axis. The data
                         here suggests that is is very rare to see starting pitchers
                         with a WHIP lower than 1. Meanwhile, there is relatively healthy
                         supply of relief pitchers with a WHIP lower than 1.", width=70)) +
  db_dark_theme() +
  theme(legend.position = 'right',
        panel.border = element_blank(),
        axis.line.x.bottom = element_line('grey50'),
        axis.line.y.left = element_line('grey50'))


ggplot(pred_p, aes(x=K, color=pos)) +
  geom_density() +
  scale_color_brewer(name='Position',palette = 'Spectral') +
  scale_y_continuous(expand = expand_scale(mult=c(-0.005, 0.05), 0)) +
  scale_x_continuous(expand=c(-0.005, 0)) +
  labs(title='Predicted Strike Density',
       subtitle="Predicted Player Strike Distribution by Position",
       y='Density (No Untis)',
       x='Strikes (Higher is Better)') +
  db_dark_theme() +
  theme(legend.position = 'right',
        panel.border = element_blank(),
        axis.line.x.bottom = element_line('grey50'),
        axis.line.y.left = element_line('grey50'),
        axis.text.y = element_blank())

ggplot(pred_p, aes(x=K, color=pos)) +
  geom_density() +
  scale_color_brewer(name='Position',palette = 'Spectral') +
  scale_y_continuous(expand = expand_scale(mult=c(-0.005, 0.05), 0)) +
  scale_x_continuous(expand=c(-0.005, 0)) +
  labs(title='Predicted Strikeout Density',
       subtitle="Predicted Player Strikeout Distribution by Position",
       y='Density',
       x='Strikeouts (Higher is Better)') +
  db_dark_theme() +
  theme(legend.position = 'right',
        panel.border = element_blank(),
        axis.line.x.bottom = element_line('grey50'),
        axis.line.y.left = element_line('grey50'),
        axis.text.y = element_blank())


ggplot(pred_p, aes(x=ERA, color=pos)) +
  geom_density() +
  scale_color_brewer(name='Position',palette = 'Spectral') +
  scale_y_continuous(expand = expand_scale(mult=c(-0.005, 0.05), 0)) +
  scale_x_continuous(expand=c(-0.005, 0)) +
  db_dark_theme() +
  theme(legend.position = 'right',
        panel.border = element_blank(),
        axis.line.x.bottom = element_line('grey50'),
        axis.line.y.left = element_line('grey50'),
        axis.text.y = element_blank())


# looking at predicted batting data =================
pred_b <- read.csv('./data/external/yahoo_pred_batters.csv') %>%
  rename('own_rate'='X..owned')

player_ids <- tibble(
  player=pred_b$player,
  id = 1:nrow(pred_b)
)

pred_b_position_expanded <- function(){
  pred_b %>%
    select(-own_rate, -H.AB, -current_rank, -owner, -preseason_rank, -team) %>%
    separate_rows(pos, sep=',')
}


pred_b_position_expanded() %>%
  ggplot(aes(x=pos, y=stat(count))) +
  geom_bar(fill='purple') +
  db_dark_theme()


pred_b_position_expanded() %>%
  ggplot(aes(x=AVG, y=stat(count), fill=pos)) +
  geom_density(position = 'fill') +
  scale_fill_brewer(name='Position',palette = 'Spectral') +
  scale_y_continuous(expand = expand_scale(mult=c(-0.005, 0.05), 0)) +
  scale_x_continuous(expand=c(-0.005, 0)) +
  db_dark_theme() +
  theme(legend.position = 'right',
        panel.border = element_blank(),
        axis.line.x.bottom = element_line('grey50'),
        axis.line.y.left = element_line('grey50'))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

percentiles <- pred_b_position_expanded() %>%
  gather('stat', 'value', -player, -pos) %>%
  filter(stat!='GP') %>%
  group_by(stat) %>%
  mutate(
    percentile=percent_rank(value),
    value01=range01(value)
  )

player_scores <- pred_b %>%
  select(player, pos, HR, R, RBI, SB, AVG) %>%
  gather('stat', 'value', -player, -pos) %>%
  group_by(stat) %>%
  mutate(
    value_01 = range01(value)
  ) %>%
  group_by(player) %>%
  summarise(
    score = sqrt(sum(value_01^2)),
    pos = pos[1]
  ) %>%
  mutate(
    score = round(rescale(score, c(50, 100)), 1)
  )

player_scores_position <- player_scores %>%
  left_join(select(pred_b, player, pos), by='player') %>%
  separate_rows(pos, sep=',') %>%
  group_by(pos) %>%
  mutate(
    pos_score = range01(score),
    pos_score_100 = round(pos_score * 100, 1),
  )

player_data <- function(player_name){
  pred_b[pred_b$player==player_name,c('AVG', 'HR', 'R', 'RBI', 'SB', 'pos', 'current_rank')]
}

ggplot(player_scores, aes(x=score_100_sq)) +
  geom_histogram(binwidth = 5)

player_data('Trea Turner')
player_data('Mike Trout')
player_data('J.D. Martinez') 
player_data('Mookie Betts')

player_data('Kris Bryant')
player_data('Matt Carpenter')

player_data('J.T. Realmuto')
player_data('Gary Sánchez')
player_data('Freddie Freeman')

ggplot(player_scores_position, aes(x=score_100, color=pos)) +
  geom_histogram(position = position_dodge(), breaks=seq(0,100,25)) +
  scale_color_brewer(palette ='Set3') +
  db_dark_theme() %+replace%
  theme(legend.position = 'right')

rescale(rnorm(10), c(50, 100))

library(lpSolve)
lp()







male = c(177, 1985)
female = c(231, 3567)

M <- as.table(rbind(c(231, 3567), c(177, 1965)))
dimnames(M) <- list(gender = c("F", "M"),
                    opioid_abuse = c("Yes","No"))
M

(Xsq <- chisq.test(M, correct = F))

