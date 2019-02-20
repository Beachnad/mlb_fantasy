library(dplyr)

source('./R/data_prep.R')
source('./R/functions.R')

replacement_pitchers <- c(
  '(SP) Replacement',
  '(RP) Replacement',
  '(P) Replacement'
)

replacement_batters <- c(
  '(1B) Replacement',
  '(2B) Replacement',
  '(3B) Replacement',
  '(SS) Replacement',
  '(OF) Replacement',
  '(C) Replacement',
  '(Util) Replacement'
)

replacement_roster <- c(
  replacement_pitchers,
  replacement_batters
)

replacement_roster.alt <- c(
  '(1B) Replacement',
  '(2B) Replacement',
  '(3B) Replacement',
  '(SS) Replacement',
  '(OF) Replacement 1',
  '(OF) Replacement 2',
  '(OF) Replacement 3',
  '(C) Replacement',
  '(Util) Replacement',
  '(SP) Replacement 1',
  '(SP) Replacement 2',
  '(SP) Replacement 3',
  '(RP) Replacement 1',
  '(RP) Replacement 2',
  '(P) Replacement 1',
  '(P) Replacement 2',
  '(P) Replacement 3'
)


# ROSTER =======
my_roster <- c(
  'Alex Bregman',   # 3B, SS
  'Mike Trout',     # OF
  'Anthony Rizzo',  # 1B
  'Matt Carpenter', # 1B, 2B, 3B
  'Danny Duffy',    # SP
  'Joey Gallo',     # OF, 1B
  'Ryan Zimmerman', # 1B
  "Ryan O'Hearn"    # 1B
)

opt_lineup <- function(players, fill_replacements=T){
  if(fill_replacements){players <- c(players, replacement_roster)}
  
  roster <- filter(player_data.rep, player %in% players) %>%
    separate_rows(pos, sep=',') %>%
    left_join(select(vorp, player, pos, vor), by=c('player', 'pos')) %>%
    group_by(pos) %>%
    mutate(rank=dense_rank(desc(vor))) %>%
    ungroup() %>%
    select(player, vor, pos, score)

  
  df <- roster %>%
    mutate(
      label = paste(player, pos, sep=' - '),
      one=1
    ) %>%
    spread(key=pos, value=one, fill=0) %>%
    add_columns(positions, 0) %>%
    mutate(one=1) %>%
    spread(key=player, value=one, fill=0)
  
  objective.in <- df$score
  
  const.mat <- df %>%
    select(-label, -vor, -score) %>%
    data.matrix() %>% t()
  
  const.rhs <- case_when(
    grepl('Replacement', row.names(const.mat))~99,
    row.names(const.mat)%in%c('1B', '2B', '3B', 'SS', 'C', 'Util')~1,
    row.names(const.mat)=='RP'~2,
    row.names(const.mat)%in%c('OF', 'SP')~3,
    row.names(const.mat)=='P'~3,
    T~1
  )
  
  const.dir <- rep('<=', length(const.rhs))
  
  sol <- lp(direction = "max", 
            objective.in, # maximize objective function
            const.mat, const.dir, const.rhs,   # constraints
            all.int=T)
  
  starters <- tibble(player=df$label[sol$solution>=1],
                     cnt=sol$solution[sol$solution>=1])
  starters <- starters[rep(row.names(starters), starters$cnt), 1] %>%
    separate(player, sep=' - ', into=c('player', 'pos')) %>%
    group_by(pos) %>%
    mutate(pos_num=1:n()) %>%
    ungroup()
  
  lineup <- tibble(
    pos = c('C', '1B', '2B', '3B', 'SS',
            'OF', 'OF', 'OF', 'Util', 
            'SP', 'SP', 'SP', 'RP', 'RP',
            'P', 'P', 'P')
  ) %>%
    group_by(pos) %>%
    mutate(pos_num = 1:n()) %>%
    ungroup() %>%
    left_join(starters, by=c('pos', 'pos_num')) %>%
    arrange(pos)
  
  
  bench <- setdiff(roster$player, lineup$player)
  if(length(bench)>0){
    lineup <- lineup %>%
      rbind(tibble(
        player = bench,
        pos = 'BN',
        pos_num = 1:length(bench)
      ))
  }
  
  lineup %>%
    left_join({select(player_data, player, score) %>%
        bind_rows(transmute(replacement_player_data, score=score, player = paste0('(', pos, ') Replacement')))}, by='player') %>%
    filter(pos!='BN' | !grepl('Replacement', player))
}

opt <- opt_lineup(my_roster, T)
opt_lineup(replacement_roster, F)


#' Simulates a Player's Schedule
#' 
#' Given how many games are s player is expected to
#' play in a mlb season, return an array of 1's and
#' 0's indicating: 1 = playing, 0 = not playing.
sim_schedule <- function(gp=162){
  shift = sample(0:5, 1)
  if (gp <= 60){play <- ceiling(seq(shift, 157 + shift, length.out = gp))}
  else{play = ceiling(seq(1, 162, length.out = gp))}
  
  s <- rep(1, 187)
  s[spaced_sample(1:187, 25, k=4)] <- 0
  x <- rep(0, 187)
  x[which(s==1)[play]] <- 1
  x
}

sim_schedule(gp=60)

sim_weeks <- function(n, gp){
  sch <- sim_schedule(gp)
  wks <- sapply(sample(10:155, n), function(x){sch[x:(x+6)]}) %>%
    t()
  wks
}

sim_weeks(100, 32)


roster <- my_roster
sim_score_weeks <- function(n=100, roster){
  
  players <- player_data.rep$player[player_data.rep$player %in% roster]
  gps <- player_data.rep$GP[player_data.rep$player %in% roster]
  
  
  sim_wks <- lapply(gps, function(x)array(sim_weeks(n, x)))
  
  scores = sapply(1:(7*n), function(dy){
    playing <- sapply(1:length(players), function(i)sim_wks[[i]][dy])
    active_players = players[playing==1]
    opt <- opt_lineup(active_players, fill_replacements = F)
    score <- sum(opt$score[opt$pos!='BN'], na.rm=T)
  })
  
  mean(scores)
}

roster <- my_roster
replace_missing_slots <- function(roster){
  opt <- opt_lineup(roster, F)
  filter(player_data.rep, player %in% players) %>%
    separate_rows(pos, sep=',') %>%
    group_by(pos)
}

sim_score_weeks(n=20, my_roster)

sim_added_value_of <- function(curr_roster, new_player, curr_roster_score=NULL){
  if(!is.null(curr_roster_score)){curr_score <- curr_roster_score}
  else{curr_score <- sim_score_weeks(n=20, curr_roster)}
  
  if(!grepl('P',player_data$pos[player_data$player == new_player])){
    curr_roster <- c(curr_roster, replacement_roster)
  }
  
  
  eval_score <- sim_score_weeks(n=50, c(curr_roster, new_player))
  
  eval_score - curr_score
}

sim_score_weeks(n=100, c(my_roster, replacement_roster))  # 737.9651
sim_score_weeks(n=100, c(my_roster))                      # 399.6822



sim_added_value_of(my_roster, 'Edwin Díaz', 737.9651)
sim_added_value_of(my_roster, 'Jesús Aguilar',  737.9651)
sim_added_value_of(my_roster, 'Charlie Blackmon', curr_roster_score = 1175.215)
sim_added_value_of(my_roster, 'Khris Davis', curr_roster_score = 1175.215)
sim_added_value_of(my_roster, 'Max Scherzer', 399.6822)

sim_score_weeks(n=20, my_roster)
