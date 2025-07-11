library(tidyverse)


make_chain <- function(n, seed = 123) {
  # x1 -> x2 -> y
  
  if (!is.null(seed)) set.seed(seed)
  
  X1 <- rnorm(n, mean = 0, sd = 1)
  
  beta_1 <- rnorm(1, mean = 0, sd = 10)
  
  X2 <- rnorm(n, mean = beta_1* X1, sd = 1)

  beta_2 <- rnorm(1, mean = 0, sd = 10)
  
  Y <- rnorm(n, mean = beta_2 * X2, sd = 1)
  
  df <- tibble(X1 = X1, X2 = X2, Y = Y)
  
  out <- list(
    df = df,
    beta_1 = beta_1,
    beta_2 = beta_2
  )
  
  return(out)
}

make_fork <- function(n, seed = 123) {
  # x1 <- x2 -> y
  
  if (!is.null(seed)) set.seed(seed)
  
  X2 <- rnorm(n, mean = 0, sd = 1)
  
  beta_2_X1 <- rnorm(1, mean = 0, sd = 10)
  
  X1 <- rnorm(n, mean = beta_2_X1 * X2, sd = 1)
  
  beta_2_Y <- rnorm(1, mean = 0, sd = 10)
  
  Y <- rnorm(n, mean = beta_2_Y * X2, sd = 1)
  
  df <- tibble(X1 = X1, X2 = X2, Y = Y)
  
  out <- list(
    df = df,
    beta_2_X1 = beta_2_X1,
    beta_2_Y = beta_2_Y
  )
  
  return(out)
}

make_collider <- function(n, seed = 123) {
  # x1 -> x2 <- y
  
  if (!is.null(seed)) set.seed(seed)
  
  X1 <- rnorm(n, mean = 0, sd = 1)
  
  Y <- rnorm(n, mean = 0, sd = 1)
  
  beta_2_X1 <- rnorm(1, mean = 0, sd = 10)
  beta_2_Y <- rnorm(1, mean = 0, sd = 10)
  
  X2 <- rnorm(n, mean = beta_2_X1 * X1 + beta_2_Y * Y, sd = 1)
  
  df <- tibble(X1 = X1, X2 = X2, Y = Y)
  
  out <- list(
    df = df,
    beta_2_X1 = beta_2_X1,
    beta_2_Y = beta_2_Y
  )
  
  return(out)
}

make_triangle <- function(n, seed = 123) {
  # X2 -> X1 -> Y
  # X2 -> Y
  
  if (!is.null(seed)) set.seed(seed)
  
  X2 <- rnorm(n, mean = 0, sd = 1)
  beta_2_X1 <- rnorm(1, mean = 0, sd = 10)
  X1 <- rnorm(n, mean = beta_2_X1 * X2, sd = 1)
  beta_2_Y <- rnorm(1, mean = 0, sd = 10)
  Y <- rnorm(n, mean = beta_2_Y * X2 + beta_2_Y * X1, sd = 1)
  
  df <- tibble(X1 = X1, X2 = X2, Y = Y)
  out <- list(
    df = df,
    beta_2_X1 = beta_2_X1,
    beta_2_Y = beta_2_Y
  )
  return(out)
}

make_datasets <- function(n = 1000, seed = 123) {
  chain <- make_chain(n, seed)
  fork <- make_fork(n, seed)
  collider <- make_collider(n, seed)
  triangle <- make_triangle(n, seed)
  
  out <- list(
    chain = chain,
    fork = fork,
    collider = collider,
    triangle = triangle
  )
  
  return(out)
}