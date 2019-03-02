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

composite_probabilities <- function(prob_of_1){
  perms <- permutations(2,length(prob_of_1),c(1, -1),repeats.allowed=TRUE) 
  probs <- matrix(rep(prob_of_1, nrow(perms)), ncol=length(prob_of_1), byrow=T)
  x <- probs * perms
  x <- ifelse(x < 0, x + 1, x)
  x_probs <- apply(x, 1, prod)
  perms[perms==-1] <- 0
  mat <- cbind(perms, x_probs)
  colnames(mat) <- c(prob_of_1, 'prob')
  mat
}

opt_lineup_score <- function(players){
  
  roster <- filter(player_data.rep, player %in% players) %>%
    separate_rows(pos, sep=',') %>%
    left_join(select(vorp, player, pos, vor), by=c('player', 'pos')) %>%
    group_by(pos) %>%
    mutate(rank=dense_rank(desc(vor))) %>%
    ungroup() %>%
    select(player, vor, pos)
  
  
  df <- roster %>%
    mutate(
      label = paste(player, pos, sep=' - '),
      one=1
    ) %>%
    spread(key=pos, value=one, fill=0) %>%
    add_columns(positions, 0) %>%
    mutate(one=1) %>%
    spread(key=player, value=one, fill=0)
  
  objective.in <- df$vor
  
  const.mat <- df %>%
    select(-label, -vor) %>%
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
  return(sum(objective.in[sol$solution == 1]))
}

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

get_vor <- function(players, roster_data){
  if (length(players) == 0)return(0)
  play_probs <- roster_data[match(players, roster_data$player),]$GP / 162
  if (length(play_probs) == 1  ){return(max(roster_data$vor[roster_data$player == players[1]]))}
  
  mat <- composite_probabilities(play_probs)
  mat <- mat[sort(-mat[,'prob'], index.return=T)$ix,]
  mat <- mat[1:(min(nrow(mat), 50)),]
  mean_vor <- weighted.mean(apply(mat[,-ncol(mat)], 1, function(x)opt_lineup_score(players[x==1])),w=mat[,'prob'])
  mean_vor
}

roster_vor <- function(list_of_players){
  roster <- vorp.rep[vorp.rep$player %in% list_of_players,]
  
  pitchers <- unique(roster[roster$pos == 'P','player'])$player
  batters <- unique(roster[roster$pos == 'Util','player'])$player
  
  vor_score <- get_vor(batters, roster) + get_vor(pitchers, roster)
  vor_score
}
list_of_players <- my_roster
roster_vor(my_roster)



opt <- opt_lineup(my_roster, T)
opt_lineup(replacement_roster, F)


#' Simulates a Player's Schedule
#' 
#' Given how many games are s player is expected to
#' play in a mlb season, return an array of 1's and
#' 0's indicating: 1 = playing, 0 = not playing.
# sim_schedule <- function(gp=162){
#   shift = sample(0:5, 1)
#   if (gp <= 60){play <- ceiling(seq(shift, 157 + shift, length.out = gp))}
#   else{play = ceiling(seq(1, 162, length.out = gp))}
#   
#   s <- rep(1, 187)
#   s[spaced_sample(1:187, 25, k=4)] <- 0
#   x <- rep(0, 187)
#   x[which(s==1)[play]] <- 1
#   x
# }
# 
# sim_schedule(gp=60)
# 
# sim_weeks <- function(n, gp){
#   sch <- sim_schedule(gp)
#   wks <- sapply(sample(10:155, n), function(x){sch[x:(x+6)]}) %>%
#     t()
#   wks
# }
# 
# sim_weeks(100, 32)
# 
# 
# roster <- my_roster
# sim_score_weeks <- function(n=100, roster){
#   
#   players <- player_data.rep$player[player_data.rep$player %in% roster]
#   gps <- player_data.rep$GP[player_data.rep$player %in% roster]
#   
#   
#   sim_wks <- lapply(gps, function(x)array(sim_weeks(n, x)))
#   
#   scores = sapply(1:(7*n), function(dy){
#     playing <- sapply(1:length(players), function(i)sim_wks[[i]][dy])
#     active_players = players[playing==1]
#     opt <- opt_lineup(active_players, fill_replacements = F)
#     score <- sum(opt$score[opt$pos!='BN'], na.rm=T)
#   })
#   
#   mean(scores)
# }
# 
# roster <- my_roster
# replace_missing_slots <- function(roster){
#   opt <- opt_lineup(roster, F)
#   filter(player_data.rep, player %in% players) %>%
#     separate_rows(pos, sep=',') %>%
#     group_by(pos)
# }

sim_score_weeks(n=20, my_roster)

sim_added_value_of <- function(curr_roster, new_player){
  curr_score <- roster_vor(curr_roster)
  new_score <- roster_vor(c(new_player, curr_roster))
  new_score - curr_score
}

sim_score_weeks(n=100, c(my_roster, replacement_roster))  # 737.9651
sim_score_weeks(n=100, c(my_roster))                      # 399.6822

roster_vor(my_roster)

sim_added_value_of(my_roster, 'Edwin Díaz')
# sim_added_value_of(my_roster, 'Jesús Aguilar',  737.9651)
# sim_added_value_of(my_roster, 'Charlie Blackmon', curr_roster_score = 1175.215)
# sim_added_value_of(my_roster, 'Khris Davis', curr_roster_score = 1175.215)
sim_added_value_of(my_roster, 'Max Scherzer')

sim_score_weeks(n=20, my_roster)
