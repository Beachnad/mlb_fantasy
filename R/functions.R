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

#' Generates a permutation-probability matrix.
#' 
#' Given a list of probabilities that a binary event occurs,
#' will return a matrix of possible permutations, and the
#' probability of that permutaions.
#' 
#' @param prob List of probability values between 0 and 1
#' @return matrix of permutations and associated probability of occuring
#' @examples
#' perm_prob_mat(rep(0.5, 3))
#' 
#' 
prob <- c(0.25, 0.5, 0.75)
perm_prob_mat <- function(prob, min_cases = 0){
  perms <- permutations(2,length(prob),0:1,repeats.allowed=TRUE)
  perms <- perms[which(apply(perms, 1, sum) >= min_cases),]
  probs <- matrix(rep(prob, nrow(perms)), ncol=length(prob), byrow=T)
  x <- ifelse(perms==1,probs,1-probs)
  x_probs <- apply(x, 1, prod)
  mat <- cbind(perms, x_probs)
  colnames(mat) <- c(prob, 'prob')
  mat
}

# seq(1, 0.50, length.out=6)
# x <- perm_prob_mat(seq(1, 0.55, length.out=7))
# x_sorted <- x[sort(x[,'prob'], decreasing=T, index.return=T)$ix,]
# x_filtered <- x_sorted[,1:7]
# colnames(x_filtered) <- NULL
# 
# 
# 
# permutations(2,3,c(1, 0),repeats.allowed=TRUE)
# 
# matrix(3, 1, 1)
# 
# permutations
# 
# event_probs <- c(0.9, 0.65, 0.2)
# 
# most_probable_events <- function(event_probs){
#   rev_probs <- event_probs < 0.5
#   x_probs <- ifelse(rev_probs, 1-event_probs, event_probs)
#   sorted_ind <- sort(x_probs, decreasing = T, index.return=T)
#   x_probs <- x_probs[sorted_ind$ix]
#   
#   perms <- permutations(2,length(x_probs),c(1, 0),repeats.allowed=TRUE)
#   perms <- perms[sort(apply(perm))
# }
# 
# 
# 
# perm_prob_mat(sample(c(0.25, 0.5, 0.75), 23, replace=T), min_cases = 10)
