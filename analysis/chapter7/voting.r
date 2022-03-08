################################################################################################
### 2000 - 2004####
####################################################
##########################
d2$id<-c(1:dim(d2)[1])
DATA            =    d2[,c("auth1.2000", "auth2.2000", "auth3.2000", "auth4.2000",
                            "college.2000", "age.2000", "sex.2000", "income.2000",
                           "catholic.2000", "jewish.2000", "vote.2000", "vote.2004", "other.2000")] %>% na.omit()
FORMULA         =  "latent =~ auth1.2000 + auth2.2000 + auth3.2000 + auth4.2000"
ORDERED         =  c("auth1.2000", "auth2.2000", "auth3.2000", "auth4.2000")
CAT_LABELS      =  c("Vote Democrat", "Vote Republican")

#### Append the data with the latent variable
tmp_data = latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

data_long<-data.frame(id=rep(1:length(tmp_data$id), times=2), state=c(tmp_data$vote.2000, tmp_data$vote.2004) + 1%>% as.numeric(),
                 authoritarianism=rep(tmp_data$latent, times=2),
                 sex=rep(tmp_data$sex.2000, times=2),
                 college=rep(tmp_data$college.2000, times=2),
                 income=rep(tmp_data$income.2000, times=2),
                 age=zero.one(rep(tmp_data$age.2000, times=2)),
                 jewish=rep(tmp_data$jewish.2000, times=2),
                 catholic=rep(tmp_data$catholic.2000, times=2),
                 other=rep(tmp_data$other.2000, times=2),
                 time=c(rep(c(1:2), each=length(tmp_data$id) )))%>%  
                 arrange(id, time)%>%na.omit()


data <- data_long  %>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)

twoway2.q<-rbind(c(0,0.20),
                 c(0.2, 0))

#rownames(twoway3.q)<-colnames(twoway3.q)<-c("Democrat", "Independent", "Republican")
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+other+age, 
           subject=id, data=data,
           qmatrix=twoway2.q, gen.inits=TRUE, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1, fnscale=2000, maxit=20000, reltol=1e-8))


low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))

#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS

MOD1 = as.formula(vote.2000~ latent + sex.2000 + college.2000 + 
                        income.2000 + age.2000 + jewish.2000 + catholic.2000 + other.2000)
MOD2 = as.formula(vote.2004~ latent + sex.2000 + college.2000 + 
                        income.2000 + age.2000 + jewish.2000 + catholic.2000 + other.2000)
### Pass the transition matrices to a plot function 

high_auth <- draw_transition_effects(tran = hi, 
                                   data = tmp_data,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "AUTH",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2000 Election",
                                   y.label = "2004 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10,
                                   vote = 1
                                   )
low_auth <- draw_transition_effects(tran = low, 
                                   data = tmp_data,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "NON",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2000 Election",
                                   y.label = "2004 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10,
                                   vote = 1
                                   )


  plt1 = ggdraw() +
    draw_plot(high_auth[[1]], 0.25, 0.05,   width = 0.45,    height =   0.45) +   
    draw_plot(low_auth[[1]],  0.25, 0.5,    width = 0.45,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.72, y = 0.1, width = 0.25, height = 0.7) +
    draw_plot(low_auth[[2]],  x =  0.72, y = 0.55, width = 0.25, height = 0.7) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0.05, 0.03), y = c(0.9, 0.45))  +
    draw_figure_label("2000-2004 Panel",
                      position = "top", size = 13, fontface = "bold")

save_plot("v2004.pdf", plt1, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)

##############################################################################
##############################################################################
##########  2016  -  Block #####################
##############################################################################

d4$id<-c(1:dim(d4)[1])
d4$pid3.1<-pid.r(d4$pid.2012b)
d4$pid3.2<-pid.r(d4$pid.2016)
d4$pid3.3<-pid.r(d4$pid.2020)
d4 = subset(d4, white.2012 == 1)

##############################################################################
DATA            =    d4[,c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016",
                           "college.2012", "age.2012", "sex.2012", "income.2012",
                           "catholic.2012", "jewish.2012", "vote.2012", "vote.2016","other.2012")] %>% na.omit()
FORMULA         =  "latent =~ auth1.2016 + auth2.2016 + auth3.2016 + auth4.2016"
ORDERED         =  c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016")
CAT_LABELS      =  c("Vote Democrat", "Vote Republican")
#### Append the data with the latent variable
tmp_data = tmp_data1 = latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

tmp_data1 = tmp_data1[,names(tmp_data1) %in% c("latent", "college.2012", "age.2012", "sex.2012", "income.2012",
                           "catholic.2012", "jewish.2012", "vote.2012", "vote.2016", "other.2012")] 

tmp_data1 = tmp_data1 %>%  select(order(colnames(tmp_data1))) 

names(tmp_data1) = c("age", "catholic", "college", "female", "income",
                           "jewish", "latent", "other", "vote1", "vote2")

data_long<-data.frame(id=rep(1:length(tmp_data$id), times=2), state=c(tmp_data$vote.2012, tmp_data$vote.2016) + 1,
                 authoritarianism=rep(tmp_data$latent, times=2),
                 sex=rep(tmp_data$sex.2012, times=2),
                 college=rep(tmp_data$college.2012, times=2),
                 income=rep(tmp_data$income.2012, times=2),
                 age=zero.one(rep(tmp_data$age.2012, times=2)),
                 jewish=rep(tmp_data$jewish.2012, times=2),
                 catholic=rep(tmp_data$catholic.2012, times=2),
                 other=rep(tmp_data$other.2012, times=2),
                 time=c(rep(c(1:2), each=length(tmp_data$id) )))%>%  
                 arrange(id, time)%>%na.omit()


data <- data_long  %>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)

twoway2.q<-rbind(c(0,0.20),
                 c(0.2, 0))

#rownames(twoway3.q)<-colnames(twoway3.q)<-c("Democrat", "Independent", "Republican")
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+other+age, 
           subject=id, data=data,
           qmatrix=twoway2.q, gen.inits=TRUE, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1, fnscale=2000, maxit=20000, reltol=1e-8))


low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))

#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS

MOD1 = as.formula(vote.2012~ latent + sex.2012 + college.2012 + 
                        income.2012 + age.2012 + jewish.2012 + catholic.2012 + other.2012)
MOD2 = as.formula(vote.2012~ latent + sex.2012 + college.2012 + 
                        income.2012 + age.2012 + jewish.2012 + catholic.2012 + other.2012)

### Pass the transition matrices to a plot function 

high_auth <- draw_transition_effects(tran = hi, 
                                   data = tmp_data,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "AUTH",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2012 Election",
                                   y.label = "2016 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10,
                                   vote = 1
                                   )
low_auth <- draw_transition_effects(tran = low, 
                                   data = tmp_data,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "NON",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2012 Election",
                                   y.label = "2016 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10,
                                   vote = 1
                                   )


#dev.new()
  plt2 = ggdraw() +
    draw_plot(high_auth[[1]], 0.25, 0.05,   width = 0.45,    height =   0.45) +   
    draw_plot(low_auth[[1]],  0.25, 0.5,    width = 0.45,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.72, y = 0.1, width = 0.25, height = 0.7) +
    draw_plot(low_auth[[2]],  x =  0.72, y = 0.55, width = 0.25, height = 0.7) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0.05, 0.03), y = c(0.9, 0.45))  +
    draw_figure_label("2012-2016 Panel",
                      position = "top", size = 13, fontface = "bold")

save_plot("v2012.pdf", plt2, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)


########### And now the pooled 2016- 2020
d4$id<-c(1:dim(d4)[1])
d4 = subset(d4, white.2012 == 1)

##############################################################################
DATA            =    d4[,c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016",
                           "college.2012", "age.2012", "sex.2012", "income.2012",
                           "catholic.2012", "jewish.2012", "vote.2016", "vote.2020","other.2012")] %>% na.omit()
FORMULA         =  "latent =~ auth1.2016 + auth2.2016 + auth3.2016 + auth4.2016"
ORDERED         =  c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016")
CAT_LABELS      =  c("Vote Democrat", "Vote Republican")
#### Append the data with the latent variable
tmp_data = tmp_data1 = latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

tmp_data1 = tmp_data1[,names(tmp_data1) %in% c("latent", "college.2012", "age.2012", "sex.2012", "income.2012",
                           "catholic.2012", "jewish.2012", "vote.2016", "vote.2020", "other.2012")] 

tmp_data1 = tmp_data1 %>%  select(order(colnames(tmp_data1))) 

names(tmp_data1) = c("age", "catholic", "college", "income", "jewish",
                           "latent", "other", "female", "vote1", "vote2")

tmp_data1 = tmp_data1 %>%  select(order(colnames(tmp_data1))) 

data_long1<-data.frame(id=rep(1:length(tmp_data$id), times=2), state=c(tmp_data$vote.2016, tmp_data$vote.2020) + 1,
                 authoritarianism=rep(tmp_data$latent, times=2),
                 sex=rep(tmp_data$sex.2012, times=2),
                 college=rep(tmp_data$college.2012, times=2),
                 income=rep(tmp_data$income.2012, times=2),
                 age=zero.one(rep(tmp_data$age.2012, times=2)),
                 jewish=rep(tmp_data$jewish.2012, times=2),
                 catholic=rep(tmp_data$catholic.2012, times=2),
                 other=rep(tmp_data$other.2012, times=2),
                 time=c(rep(c(1:2), each=length(tmp_data$id) )))%>%  
                 arrange(id, time)%>%na.omit()

d7 = subset(d7, white.2016 == 1)
#dim(model.matrix(lm(d7$pid3.1~d7$pid3.2)))
d7$pid3.1 <- pid.r(d7$pid.2016)
d7$pid3.2 <- pid.r(d7$pid.2020)
prop.table(table(d7$pid3.1,  d7$pid3.2), 1)
dim(model.matrix(lm(d7$pid3.1~d7$pid3.2)))
##############################################################################
DATA            =    d7[,c("auth.1.2016", "auth.2.2016", "auth.3.2016", "auth.4.2016",
                            "college.2016", "age.2016", "female.2016", "income.2016",
                           "catholic.2016", "jewish.2016", "vote.2016", "vote.2020", "other.2016")] %>% na.omit()
FORMULA         =  "latent =~ auth.1.2016 + auth.2.2016 + auth.3.2016 + auth.4.2016"
ORDERED         =  c("auth.1.2016", "auth.2.2016", "auth.3.2016", "auth.4.2016")
CAT_LABELS      =  c("Vote Democrat", "Vote Republican")

#### Append the data with the latent variable
tmp_data = tmp_data2 =  latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

tmp_data2 = tmp_data2[,names(tmp_data2) %in% c("latent", "college.2016", "age.2016", "female.2016", "income.2016",
                           "catholic.2016", "jewish.2016", "vote.2016", "vote.2020", "other.2016")] 
                     
tmp_data2 = tmp_data2 %>%  select(order(colnames(tmp_data2))) 

names(tmp_data2) = c("age", "catholic", "college", "female", "income",
                           "jewish", "latent", "other", "vote1", "vote2")

tmp_data2 = tmp_data2 %>%  select(order(colnames(tmp_data2))) 


### Constuct MSM data frame. This is really just a long representation that can likely be accomplished with reshape2::melt
data_long2<-data.frame(
                 id=range(data_long1)[2] + rep(1:nrow(tmp_data), times=2),
                 state=c(tmp_data$vote.2016, tmp_data$vote.2020) + 1,
                 authoritarianism=c(tmp_data$latent, tmp_data$latent),
                 sex=rep(tmp_data$female.2016, times=2),
                 college=rep(tmp_data$college.2016, times=2),
                 income=rep(tmp_data$income.2016, times=2),
                 age=rep(tmp_data$age.2016, times=2) %>% zero.one(),
                 jewish=rep(tmp_data$jewish.2016, times=2),
                 other=rep(tmp_data$other.2016, times=2),
                 catholic=rep(tmp_data$catholic.2016, times=2),
                 time=c(rep(c(1:2), each=nrow(tmp_data)))) %>%
                 arrange(id, time) %>%
                 na.omit()

data_long = rbind(data_long1 %>% select(order(colnames(data_long1))) %>% mutate(survey = 1),
                  data_long2 %>% select(order(colnames(data_long2))) %>% mutate(survey = 2)) %>%
                 arrange(id, time) %>%
                 na.omit()

tmp_data1$survey = 1
tmp_data2$survey = 2 ## 2 is ANES

dat = rbind(tmp_data1, tmp_data2)

###################################


data <- data_long  %>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)

twoway2.q<-rbind(c(0,0.20),
                 c(0.2, 0))

#rownames(twoway3.q)<-colnames(twoway3.q)<-c("Democrat", "Independent", "Republican")
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+other+age, 
           subject=id, data=data,
           qmatrix=twoway2.q, gen.inits=TRUE, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1, fnscale=2000, maxit=20000, reltol=1e-8))


low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))

#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS



MOD1 = as.formula(vote1~ latent + female + college + 
                        income + age + jewish + catholic + other)
MOD2 = as.formula(vote2~ latent + female + college + 
                        income + age + jewish + catholic + other)
### Pass the transition matrices to a plot function 

high_auth <- draw_transition_effects(tran = hi, 
                                   data = dat,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "AUTH",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2016 Election",
                                   y.label = "2020 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10,
                                   vote = 1
                                   )
low_auth <- draw_transition_effects(tran = low, 
                                   data = dat,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "NON",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2016 Election",
                                   y.label = "2020 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10,
                                   vote = 1
                                   )


dev.new()
  plt3 = ggdraw() +
    draw_plot(high_auth[[1]], 0.25, 0.05,   width = 0.45,    height =   0.45) +   
    draw_plot(low_auth[[1]],  0.25, 0.5,    width = 0.45,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.72, y = 0.1, width = 0.25, height = 0.7) +
    draw_plot(low_auth[[2]],  x =  0.72, y = 0.55, width = 0.25, height = 0.7) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0.05, 0.03), y = c(0.9, 0.45))  +
    draw_figure_label("2016-2020 Panel",
                      position = "top", size = 13, fontface = "bold")
save_plot("v2020.pdf", plt3, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)


