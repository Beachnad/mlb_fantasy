library(lpSolve)


# HELPER FUNCTIONS =============================================

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#' Adds columns to a data frame
#' 
#' Given a datafram and column names, will add those columns to
#' the dataframe
#' 
#' @param df Dataframe to manipulate
#' @param columns Column names that should be added to the data frame
#' @param fill Value to fill the column with. Defaults to NA
#' @param replace Boolean value, whether or not to replace existing columns.
add_columns <- function(df, columns, fill=NA, replace=FALSE){
  if(replace){df[columns] <- fill}
  else{df[columns[!(columns %in% names(df))]] <- fill}
  df
}


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

opt_lineup <- function(players){
  roster <- filter(player_data, player %in% players) %>%
    separate_rows(pos, sep=',') %>%
    left_join(select(vorp, player, pos, vor), by=c('player', 'pos')) %>%
    group_by(pos) %>%
    mutate(rank=dense_rank(desc(vor))) %>%
    ungroup() %>%
    select(player, score, pos)
  
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
    select(-label, -score) %>%
    data.matrix() %>% t()
  
  const.rhs <- case_when(
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
  
  starters <- tibble(player=df$label[sol$solution==1]) %>%
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
  
  lineup %>%
    rbind(tibble(
      player = bench,
      pos = 'BN',
      pos_num = 1:length(bench)
    )) %>%
    left_join(select(player_data, player, score), by='player')
}

opt_lineup(my_roster)


for(p in sample(player_scores$player, 10)){
  opt <- opt_lineup(c(my_lineup, p))
  print(p)
  print(sum(opt$score, na.rm=T))
}

eval_roster <-function(players){
  opt <- opt_lineup(players)
  print(opt)
  starters <- filter(opt, pos!='BN') %>%
    left_join(select(pred_b, player, AVG, HR, R, RBI, SB), by='player') %>%
    left_join(select(pred_p, player, ERA, WHIP, K, W, SV), by='player')
  
  pred_b %>%
    filter(player %in% starters$player)
  
  bench <- filter(opt, pos=='BN')
  
  
}
eval_roster(my_roster)


#' Simulates a Player's Schedule
#' 
#' Given how many games are s player is expected to
#' play in a mlb season, return an array of 1's and
#' 0's indicating: 1 = playing, 0 = not playing.
sim_schedule <- function(gp=162){
  shift = sample(0:5, 1)
  if (gp <= 60) {play <- ceiling(seq(shift, 157 + shift, length.out = gp))}
  else{play = ceiling(seq(1, 162, length.out = gp))}
  
  s <- rep(1, 187)
  s[spaced_sample(1:187, 25, k=4)] <- 0
  x <- rep(0, 187)
  x[which(s==1)[play]] <- 1
  x
}

sim_schedule(140)





#' Randomly Sample Elements Spaced Apart
#' 
#' Given a list of numbers, the number to sample, and spacing
#' between samples -> returns a sampled list
#' 
#' @param x List of elements to sample from
#' @param n Number of elements to sample
#' @param k Spacing between samples
#' @param reset Boolean, whether or not to reset x when 
#'   If FALSE, will throw an error when x is exhausted and another sample is needed.
#' @return Spaced samples from x
#' @examples
#' spaced_sample(1:100, 5, 5)
#' spaced_sample(1:100, 5, 25)           # will throw error
#' spaced_sample(1:100, 5, 25, reset=T)  # will reset x when exhausted
spaced_sample <- function(x=1:100, n=5, k=2, reset=F){
  arr <- c()
  orig.x <- x
  for(i in 1:n){
    if(length(x) == 0 & reset){x <- orig.x}
    r = sample(x, 1)
    x <- x[abs(x-r)>k]
    arr <- c(r, arr)
  }
  arr
}


spaced_sample(1:100, 10, 25, reset=T)

#' Simulate if a player plays a given game during the week (7 days)
#' 
#' Given the number of simulations to run, the probability of playing
#' in a game, and the likelyhood of the team playing 7 games, a matrix
#' of 1's and 0's of whether or not the player plays
sim_weeks <- function(n_sims = 100, p_game=0.95, p_full_wk=0.0642){
  wks <- ifelse(runif(n=n_sims) > p_full_wk, 1, 0)
  wks <- sapply(wks, function(x){
    if(x==0){f0 <- rep(T, 7)}
    else{f0 <- ifelse(1:7 == sample(1:7, 1), FALSE, TRUE)}
    
    f1 <- runif(7) < p_game
    wk <- ifelse(f0 & f1, 1, 0)
  })
}
