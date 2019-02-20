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
