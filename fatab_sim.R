## Simulate Data From The Favorable Attitudes Toward Antisocial Behavior Scale

fatab_sim <- function(seed = 1234) {
  
  set.seed(seed)
  library(dplyr)
  
  # sample size
  N <- 25
  
  # simulate "antisocial behavior", a latent variable with an arbitrary scale
  attitude = runif(N,0,1)
  
  # simulate latent responses to 5 questions about attitude for baseline
  b_gun = .2
  e_gun = rnorm(N,0,1)
  l_gun = b_gun*attitude+e_gun
  
  b_stl = .3
  e_stl = rnorm(N,0,1)
  l_stl = b_stl*attitude+e_stl
  
  b_att = .15
  e_att = rnorm(N,0,1)
  l_att = b_att*attitude+e_att
  
  b_fgt = .7
  e_fgt = rnorm(N,0,1)
  l_fgt = b_fgt*attitude+e_fgt
  
  b_skip = .25
  e_skip = rnorm(N,0,1)
  l_skip = b_skip*attitude+e_skip
  
  dat_prep_A <- data_frame(l_gun, l_stl, l_att, l_fgt, l_skip)
  
  # simulate "attitude", a latent variable with an arbitrary scale
  attitude = runif(N,2,3)
  # simulate latent responses to 5 questions about attitude for baseline
  b_gun = .2
  e_gun = rnorm(N,0,1)
  l_gun = b_gun*attitude+e_gun
  
  b_stl = .3
  e_stl = rnorm(N,0,1)
  l_stl = b_stl*attitude+e_stl
  
  b_att = .15
  e_att = rnorm(N,0,1)
  l_att = b_att*attitude+e_att
  
  b_fgt = .7
  e_fgt = rnorm(N,0,1)
  l_fgt = b_fgt*attitude+e_fgt
  
  b_skip = .25
  e_skip = rnorm(N,0,1)
  l_skip = b_skip*attitude+e_skip
  
  dat_prep_B <- data_frame(l_gun, l_stl, l_att, l_fgt, l_skip)
  
  dat_raw <- bind_rows(dat_prep_A, dat_prep_B) %>%
    mutate_all(cut, breaks = 4, labels = FALSE) %>%
    mutate(report_date = seq(as.Date("2015/1/1"), by = "week", length.out = 50))
  
  dat_raw
  
}

