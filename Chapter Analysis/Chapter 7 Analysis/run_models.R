## Load dependencies, packages, data, etc ##
# More transformations and data recodes.....
rm(list = ls())
library(brms)
library(modelr)
library(tidybayes)
library(haven)

data_location <- "/Users/Chris/Dropbox/github_repos/Authoritarianism_V2/clean_data/"
##### Data Recodes #####
## Load the cleaned up panel data
setwd(data_location)
load("panel.auth.rda")
library(reshape2)
dat2000 <- tmp_data[[4]] %>% zap_labels()
dat2012 <- tmp_data[[5]] %>% zap_labels()
dat2016 <- tmp_data[[6]] %>% zap_labels()
dat2000$authoritarianism_2 <- dat2000$authoritarianism^2
dat2012$authoritarianism_2 <- dat2012$authoritarianism^2
dat2016$authoritarianism_2 <- dat2016$authoritarianism^2

dat2000$republican <- ifelse(dat2000$pid3.1 == 3, 1, 0)
dat2000$independent <- ifelse(dat2000$pid3.1 == 2, 1, 0)
dat2000$democrat <- ifelse(dat2000$pid3.1 == 1, 1, 0)

dat2012$republican <- ifelse(dat2012$pid3.1 == 3, 1, 0)
dat2012$independent <- ifelse(dat2012$pid3.1 == 2, 1, 0)
dat2012$democrat <- ifelse(dat2012$pid3.1 == 1, 1, 0)

dat2016$republican <- ifelse(dat2016$pid3.1 == 3, 1, 0)
dat2016$independent <- ifelse(dat2016$pid3.1 == 2, 1, 0)
dat2016$democrat <- ifelse(dat2016$pid3.1 == 1, 1, 0)


dat2000$vote1 <- dat2000$vote1 %>% as.numeric()
dat2012$vote1 <- dat2012$vote1 %>% as.numeric()
dat2016$vote1 <- dat2016$vote1 %>% as.numeric()


dat2000 <- dat2000 %>%
     mutate(gen_x = ifelse(18 <= age.2000 & age.2000 <= 35, 1, 0)) %>%
     mutate(boomer = ifelse(36 <= age.2000 & age.2000 <= 54, 1, 0)) %>%
     mutate(silent = ifelse(55 <= age.2000 & age.2000 <= 72, 1, 0)) %>%
     mutate(greatest = ifelse(73 <= age.2000 & age.2000 <= 85, 1, 0))

dat2012 <- dat2012 %>%
     mutate(millenial = ifelse(18 <= age.2012 & age.2012 <= 31, 1, 0)) %>%
     mutate(gen_x = ifelse(32 <= age.2012 & age.2012 <= 47, 1, 0)) %>%
     mutate(boomer = ifelse(48 <= age.2012 & age.2012 <= 66, 1, 0)) %>%
     mutate(silent = ifelse(67 <= age.2012 & age.2012 <= 84, 1, 0)) %>%
     mutate(greatest = ifelse(85 <= age.2012 & age.2012 <= 96, 1, 0))
dat2016 <- dat2016 %>%
     mutate(millenial = ifelse(18 <= age.2016 & age.2016 <= 35, 1, 0)) %>%
     mutate(gen_x = ifelse(36 <= age.2016 & age.2016 <= 51, 1, 0)) %>%
     mutate(boomer = ifelse(52 <= age.2016 & age.2016 <= 71, 1, 0)) %>%
     mutate(silent = ifelse(81 <= age.2016 & age.2016 <= 95, 1, 0))

### Voting, 2000
auth_in_2000 <- brm(authoritarianism ~ gen_x + boomer,
     family = gaussian,
     data = dat2000,
     chains = 3,
     cores = 4,
     seed = 1234,
     iter = 1000,
     control = list(
          adapt_delta = 0.9,
          max_treedepth = 15
     )
)
### auth, 2000


auth_in_2012 <- brm(authoritarianism ~ millenial + gen_x + boomer,
     family = gaussian,
     data = dat2012,
     chains = 3,
     cores = 4,
     seed = 1234,
     iter = 1000,
     control = list(
          adapt_delta = 0.9,
          max_treedepth = 15
     )
)
auth_in_2016 <- brm(authoritarianism ~ millenial + gen_x + boomer,
     family = gaussian,
     data = dat2016,
     chains = 3,
     cores = 4,
     seed = 1234,
     iter = 1000,
     control = list(
          adapt_delta = 0.9,
          max_treedepth = 15
     )
)


dat <- list(auth_in_2000, auth_in_2012, auth_in_2016)
save(dat, file = "/Users/Chris/Dropbox/github_repos/Authoritarianism_V2/Chapter Analysis/Chapter 7 Analysis/chapter7_cohorts.rda")


### Voting, 2000
vote_2000 <- brm(vote2 ~ sex.2000 + age.2000 + college.2000 +
     income.2000 + authoritarianism + authoritarianism_2 +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1,
family = bernoulli(link = "logit"),
data = dat2000,
chains = 3,
cores = 4,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)
### PID, 2000

pid_2000 <- brm(pid3.2 ~ sex.2000 + age.2000 + college.2000 +
     income.2000 + authoritarianism + authoritarianism_2 +
     republican + independent +
     authoritarianism:republican +
     authoritarianism_2:republican +
     authoritarianism:independent +
     authoritarianism_2:independent,
family = categorical(link = "logit"),
data = dat2000,
chains = 3,
cores = 8,
seed = 1234,
iter = 2000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)

### Vote, 2012

vote_2012 <- brm(vote2 ~ sex.2012 + age.2012 + college.2012 +
     income.2012 + authoritarianism + authoritarianism_2 +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1,
family = bernoulli(link = "logit"),
data = dat2012,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)



### PID, 2012

pid__2012 <- brm(pid3.2 ~ sex.2012 + sex.2012 + college.2012 +
     income.2012 + authoritarianism + authoritarianism_2 +
     republican + independent +
     authoritarianism:republican +
     authoritarianism_2:republican +
     authoritarianism:independent +
     authoritarianism_2:independent,
family = categorical(link = "logit"),
data = dat2012,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)


### Vote, 2016

vote_2016 <- brm(vote2 ~ female.2016 + age.2016 + college.2016 +
     income.2016 + authoritarianism + authoritarianism_2 +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1,
family = bernoulli(link = "logit"),
data = dat2016,
chains = 3,
cores = 4,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)

### PID, 2016

pid_2016 <- brm(pid3.2 ~ female.2016 + age.2016 + college.2016 +
     income.2016 + authoritarianism + authoritarianism_2 +
     republican + independent +
     authoritarianism:republican +
     authoritarianism_2:republican +
     authoritarianism:independent +
     authoritarianism_2:independent,
family = categorical(link = "logit"),
data = dat2016,
chains = 3,
cores = 9,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)

### Education Analysis

vote_ed_2000 <- brm(vote2 ~ sex.2000 + age.2000 + college.2000 +
     income.2000 + authoritarianism + authoritarianism_2 +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1 +
     authoritarianism:college.2000 + authoritarianism_2:college.2000 +
     college.2000:vote1 + authoritarianism:college.2000:vote1,
family = bernoulli(link = "logit"),
data = dat2000,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)

vote_ed_2012 <- brm(vote2 ~ sex.2012 + age.2012 + college.2012 +
     income.2012 + authoritarianism + authoritarianism_2 +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1 +
     authoritarianism:college.2012 + authoritarianism_2:college.2012 +
     college.2012:vote1 + authoritarianism:college.2012:vote1,
family = bernoulli(link = "logit"),
data = dat2012,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)


vote_ed_2016 <- brm(vote2 ~ female.2016 + age.2016 + college.2016 +
     income.2016 + authoritarianism + authoritarianism_2 +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1 +
     authoritarianism:college.2016 + authoritarianism_2:college.2016 +
     college.2016:vote1 + authoritarianism:college.2016:vote1,
family = bernoulli(link = "logit"),
data = dat2016,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)

dat <- list(vote_2000, vote_2012, vote_2016, pid_2000, pid__2012, pid_2016)
save(dat, file = "/Users/Chris/Dropbox/github_repos/Authoritarianism_V2/Chapter Analysis/Chapter 7 Analysis/chapter7.rda")



### Education Analysis

fit4a <- brm(vote2 ~ sex.2000 + age.2000 + college.2000 +
     income.2000 + authoritarianism + authoritarianism_2 +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1 +
     authoritarianism:college.2000 + authoritarianism_2:college.2000 +
     college.2000:vote1 + authoritarianism:college.2000:vote1,
family = bernoulli(link = "logit"),
data = dat2000,
chains = 3,
cores = 4,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)

fit4b <- brm(vote2 ~ sex.2012 + age.2012 + college.2012 +
     income.2012 + authoritarianism + authoritarianism_2 +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1 +
     authoritarianism:college.2012 + authoritarianism_2:college.2012 +
     college.2012:vote1 + authoritarianism:college.2012:vote1,
family = bernoulli(link = "logit"),
data = dat2012,
chains = 3,
cores = 4,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)


fit4c <- brm(vote2 ~ female.2016 + age.2016 + college.2016 +
     income.2016 + authoritarianism + authoritarianism_2 +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1 +
     authoritarianism:college.2016 + authoritarianism_2:college.2016 +
     college.2016:vote1 + authoritarianism:college.2016:vote1,
family = bernoulli(link = "logit"),
data = dat2016,
chains = 3,
cores = 4,
seed = 1234,
iter = 1000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)



cohorts1 <- brm(vote2 ~ sex.2000 + age.2000 + college.2000 +
     income.2000 + authoritarianism + authoritarianism_2 +
     boomer + gen_x +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1 +
     vote1:boomer + vote1:boomer +
     authoritarianism:boomer + authoritarianism_2:boomer +
     authoritarianism:gen_x + authoritarianism_2:gen_x,
family = bernoulli(link = "logit"),
data = dat2000,
chains = 3,
cores = 4,
seed = 1234,
iter = 2000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)

cohorts2 <- brm(vote2 ~ sex.2012 + age.2012 + college.2012 +
     income.2012 + authoritarianism + authoritarianism_2 +
     boomer + gen_x +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1 +
     vote1:boomer + vote1:boomer + vote1:gen_x + vote1:millenial +
     authoritarianism:boomer + authoritarianism_2:boomer +
     authoritarianism:gen_x + authoritarianism_2:gen_x +
     authoritarianism:millenial + authoritarianism_2:millenial,
family = bernoulli(link = "logit"),
data = dat2012,
chains = 3,
cores = 4,
seed = 1234,
iter = 2000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)

cohorts3 <- brm(vote2 ~ female.2016 + age.2016 + college.2016 +
     income.2016 + authoritarianism + authoritarianism_2 +
     boomer + gen_x +
     vote1 + authoritarianism:vote1 + authoritarianism_2:vote1 +
     vote1:boomer + vote1:boomer +
     vote1:gen_x +
     authoritarianism:boomer + authoritarianism_2:boomer +
     authoritarianism:gen_x + authoritarianism_2:gen_x +
     authoritarianism:millenial + authoritarianism_2:millenial,
family = bernoulli(link = "logit"),
data = dat2016,
chains = 3,
cores = 4,
seed = 1234,
iter = 2000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)
### Lots of terms.
cohorts_e1 <- brm(vote2 ~ sex.2000 + age.2000 + college.2000 +
     income.2000 + authoritarianism + authoritarianism_2 +
     boomer + gen_x +
     vote1 +
     authoritarianism:vote1 +
     authoritarianism_2:vote1 +
     authoritarianism:college.2000 +
     authoritarianism_2:college.2000 +
     vote1:college.2000 +
     vote1:boomer +
     vote1:boomer +
     authoritarianism:boomer +
     authoritarianism_2:boomer +
     college.2000:boomer +
     college.2000:gen_x +
     college.2000:authoritarianism:boomer +
     college.2000:authoritarianism_2:boomer +
     college.2000:authoritarianism:gen_x +
     college.2000:authoritarianism_2:gen_x,
family = bernoulli(link = "logit"),
data = dat2000,
chains = 3,
cores = 4,
seed = 1234,
iter = 2000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)


cohorts_e2 <- brm(vote2 ~ sex.2012 + age.2012 + college.2012 +
     income.2012 + authoritarianism + authoritarianism_2 +
     boomer + gen_x + millenial +
     vote1 +
     authoritarianism:vote1 +
     authoritarianism_2:vote1 +
     authoritarianism:college.2012 +
     authoritarianism_2:college.2012 +
     authoritarianism:boomer +
     authoritarianism_2:boomer +
     authoritarianism:millenial +
     authoritarianism_2:millenial +
     vote1:millenial +
     vote1:college.2012 +
     college.2012:millenial +
     vote1:boomer +
     vote1:boomer +
     college.2012:boomer +
     college.2012:gen_x +
     college.2012:authoritarianism:boomer +
     college.2012:authoritarianism_2:boomer +
     college.2012:authoritarianism:gen_x +
     college.2012:authoritarianism_2:gen_x +
     college.2012:authoritarianism:millenial +
     college.2012:authoritarianism_2:millenial,
family = bernoulli(link = "logit"),
data = dat2012,
chains = 3,
cores = 4,
seed = 1234,
iter = 2000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)


cohorts_e3 <- brm(vote2 ~ female.2016 + age.2016 + college.2016 +
     income.2016 + authoritarianism + authoritarianism_2 +
     boomer + gen_x + millenial +
     vote1 +
     authoritarianism:vote1 +
     authoritarianism_2:vote1 +
     authoritarianism:college.2016 +
     authoritarianism_2:college.2016 +
     authoritarianism:boomer +
     authoritarianism_2:boomer +
     authoritarianism:millenial +
     authoritarianism_2:millenial +
     vote1:millenial +
     vote1:college.2016 +
     college.2016:millenial +
     vote1:boomer +
     vote1:boomer +
     college.2016:boomer +
     college.2016:gen_x +
     college.2016:authoritarianism:boomer +
     college.2016:authoritarianism_2:boomer +
     college.2016:authoritarianism:gen_x +
     college.2016:authoritarianism_2:gen_x +
     college.2016:authoritarianism:millenial +
     college.2016:authoritarianism_2:millenial,
family = bernoulli(link = "logit"),
data = dat2016,
chains = 3,
cores = 4,
seed = 1234,
iter = 2000,
control = list(
     adapt_delta = 0.9,
     max_treedepth = 15
)
)

dat <- list(
     cohorts1, cohorts2, cohorts3, cohorts_e1,
     cohorts_e2, cohorts_e3
)
save(dat, file = "/Users/Chris/Dropbox/github_repos/Authoritarianism_V2/Chapter Analysis/Chapter 7 Analysis/chapter7_models_cohorts.rda")
