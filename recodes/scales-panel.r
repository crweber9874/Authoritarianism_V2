rm(list = ls())
library(brms)
library(tidyverse)
library(ggplot2)
library(tictoc)
library(modelr)
library(tidybayes)
library(dplyr)
library(cowplot)
source("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/configurations/configurations.r")
source("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/configurations/user_functions.r")
#source("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/analysis/chapter7/user_functions-ch8.r")
setwd("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data")

##### Data Recodes #####
load("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data/panel_data_2000.rda")

load("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data/panel_data_2012.rda")

load("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data/panel_data_2016.rda")


### Construct authoritarianism score in each panel########
library(haven)
head(panel_data_2000$auth1.2000)
dat1 = subset(panel_data_2000, white.2000 ==1) %>% zap_labels() %>%
                mutate(auth1 = recode(as.numeric(auth1.2000), `2` = 1, `3` = 2)) %>% 
                mutate(auth2 = recode(as.numeric(auth2.2000), `2` = 1, `3` = 2)) %>% 
                mutate(auth3 = recode(as.numeric(auth3.2000), `2` = 1, `3` = 2))  %>% 
                mutate(auth4 = recode(as.numeric(auth4.2000), `2` = 1, `3` = 2)) %>% 
                mutate(vote1  = vote.2000) %>% 
                mutate(vote2  = vote.2004) %>% 
                mutate(authoritarianism = rowMeans(cbind(auth1, auth2, auth3, auth4)) %>% zero.one())
dat1$pid3.1<-pid.r(dat1$pid.2000)
dat1$pid3.2<-pid.r(dat1$pid.2004)

dat2 = subset(panel_data_2012, white.2012 ==1) %>% zap_labels() %>%
                mutate(auth1 = recode(as.numeric(auth1.2016), `0` = 1, `1` = 2)) %>% 
                mutate(auth2 = recode(as.numeric(auth2.2016), `0` = 1, `1` = 2)) %>% 
                mutate(auth3 = recode(as.numeric(auth3.2016), `0` = 1, `1` = 2))  %>% 
                mutate(auth4 = recode(as.numeric(auth4.2016), `0` = 1, `1` = 2)) %>% 
                mutate(authoritarianism = rowMeans(cbind(auth1, auth2, auth3, auth4)) %>% zero.one()) %>%
                mutate(vote1  = vote.2012) %>% 
                mutate(vote2  = vote.2016) 
dat2$pid3.1<-pid.r(dat2$pid.2012a)
dat2$pid3.2<-pid.r(dat2$pid.2016)

dat3 = subset(panel_data_2016, white.2016 ==1)%>% zap_labels() %>%
                mutate(auth1 = recode(as.numeric(auth.1.2016), `2` = 1, `3` = 2)) %>% 
                mutate(auth2 = recode(as.numeric(auth.2.2016), `2` = 1, `3` = 2)) %>% 
                mutate(auth3 = recode(as.numeric(auth.3.2016), `2` = 1, `3` = 2))  %>% 
                mutate(auth4 = recode(as.numeric(auth.4.2016), `2` = 1, `3` = 2)) %>% 
                mutate(authoritarianism = rowMeans(cbind(auth1, auth2, auth3, auth4)) %>%zero.one()) %>%
                mutate(vote1  = vote.2016) %>% 
                mutate(vote2  = vote.2020) 
dat3$pid3.1<-pid.r(dat3$pid.2016)
dat3$pid3.2<-pid.r(dat3$pid.2020)

dat1 = dat1 %>% mutate(id = seq(1:nrow(dat1)))
dat2 = dat2 %>% mutate(id = seq(1:nrow(dat2)))
dat3 = dat3 %>% mutate(id = seq(1:nrow(dat3)))


tmp_data = dat1
tmp_data$authoritarianism_2 = tmp_data$authoritarianism^2

dat1_long<-data.frame(id=rep(1:length(tmp_data$id), times=2), 
                 pid=c(tmp_data$pid3.1,  tmp_data$pid3.2)%>% as.numeric(),
                 vote=c(tmp_data$vote1, tmp_data$vote2),
                 authoritarianism=rep(tmp_data$authoritarianism, times=2),
                 authoritarianism_2=rep(tmp_data$authoritarianism_2, times=2),
                 sex=rep(tmp_data$sex.2000, times=2),
                 college=rep(tmp_data$college.2000, times=2),
                 income=rep(tmp_data$income.2000, times=2),
                 age=zero.one(rep(tmp_data$age.2000, times = 2)),
                 time=c(rep(c(1:2), each= length(tmp_data$id))))%>%  
                 arrange(id, time) %>% na.omit()

tmp_data = dat2
tmp_data$authoritarianism_2 = tmp_data$authoritarianism^2

dat2_long<-data.frame(id=rep(1:length(tmp_data$id), times=2), 
                 pid=c(tmp_data$pid3.1,  tmp_data$pid3.2)%>% as.numeric(),
                 vote=c(tmp_data$vote1, tmp_data$vote2),
                 authoritarianism=rep(tmp_data$authoritarianism, times=2),
                 authoritarianism_2=rep(tmp_data$authoritarianism_2, times=2),
                 sex=rep(tmp_data$sex.2012, times=2),
                 college=rep(tmp_data$college.2012, times=2),
                 income=rep(tmp_data$income.2012, times=2),
                 age=zero.one(rep(tmp_data$age.2012, times=2)),
                 time=c(rep(c(1:2), each=length(tmp_data$id) )))%>%  
                 arrange(id, time)%>%na.omit()

### Constuct MSM data frame. This is really just a long representation that can likely be accomplished with reshape2::melt
tmp_data = dat3
tmp_data$authoritarianism_2 = tmp_data$authoritarianism^2
dat3_long<-data.frame(
                 id=rep(1:nrow(tmp_data), times=2),
                 pid=c(tmp_data$pid3.1,  tmp_data$pid3.2)%>% as.numeric(),
                 vote=c(tmp_data$vote1, tmp_data$vote2),
                 authoritarianism=c(tmp_data$authoritarianism, tmp_data$authoritarianism),
                 authoritarianism2=c(tmp_data$authoritarianism_2, tmp_data$authoritarianism2),
                 sex=rep(tmp_data$female.2016, times=2),
                 college=rep(tmp_data$college.2016, times=2),
                 income=rep(tmp_data$income.2016, times=2),
                 age=rep(tmp_data$age.2016, times=2) %>% zero.one(),
                 time=c(rep(c(1:2), each=nrow(tmp_data)))) %>%
                 arrange(id, time) %>%
                 na.omit()

tmp_data = list(dat1_long, dat2_long, dat3_long, dat1, dat2, dat3)
save(tmp_data, file = "panel.auth.rda") # Only white respondents, per collective decision