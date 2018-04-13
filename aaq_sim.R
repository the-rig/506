## Simulate Data From The Acceptance and Action Questionaire

aaq_sim <- function(seed = 1234) {

  set.seed(seed)
  library(dplyr)
  
  # sample size
  N <- 25
  
  # simulate "avoidance", a latent variable with an arbitrary scale
  avoidance = runif(N,2,3)
  
  # simulate latent responses to 9 questions about avoidance for baseline
  b3 = .32
  e3 = rnorm(N,0,1)
  l3 = b3*avoidance+e3
  
  b6 = .49
  e6 = rnorm(N,0,1)
  l6 = b6*avoidance+e6
  
  b8 = .56
  e8 = rnorm(N,0,1)
  l8 = b8*avoidance+e8
  
  b16 = .50
  e16 = rnorm(N,0,1)
  l16 = b16*avoidance+e16
  
  b19 = .59
  e19 = rnorm(N,0,1)
  l19 = b19*avoidance+e19
  
  b20 = .36
  e20 = rnorm(N,0,1)
  l20 = b20*avoidance+e20
  
  b24 = .73
  e24 = rnorm(N,0,1)
  l24 = b24*avoidance+e24
  
  b28 = .48
  e28 = rnorm(N,0,1)
  l28 = b28*avoidance+e28
  
  b31 = .48
  e31 = rnorm(N,0,1)
  l31 = b31*avoidance+e31
  
  dat_prep_A <- data_frame(l31, l28, l20, l19, l16, l8, l6, l3)
  
  # simulate "avoidancease", a latent variable with a normal scale
  avoidance = runif(N,0,1)
  
  # simulate latent responses to 9 questions about avoidance for follow-up
  b3 = .32
  e3 = rnorm(N,0,1)
  l3 = b3*avoidance+e3
  
  b6 = .49
  e6 = rnorm(N,0,1)
  l6 = b6*avoidance+e6
  
  b8 = .56
  e8 = rnorm(N,0,1)
  l8 = b8*avoidance+e8
  
  b16 = .50
  e16 = rnorm(N,0,1)
  l16 = b16*avoidance+e16
  
  b19 = .59
  e19 = rnorm(N,0,1)
  l19 = b19*avoidance+e19
  
  b20 = .36
  e20 = rnorm(N,0,1)
  l20 = b20*avoidance+e20
  
  b24 = .73
  e24 = rnorm(N,0,1)
  l24 = b24*avoidance+e24
  
  b28 = .48
  e28 = rnorm(N,0,1)
  l28 = b28*avoidance+e28
  
  b31 = .48
  e31 = rnorm(N,0,1)
  l31 = b31*avoidance+e31
  
  dat_prep_B <- data_frame(l31, l28, l20, l19, l16, l8, l6, l3) 
  
  dat_raw <- bind_rows(dat_prep_A, dat_prep_B) %>%
    mutate_all(cut, breaks = 7, labels = FALSE) %>%
    mutate(report_date = seq(as.Date("2015/1/1"), by = "week", length.out = 50))
  
  dat_raw
  
}