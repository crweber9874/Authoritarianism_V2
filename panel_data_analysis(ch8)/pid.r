### Dependencies and other stuff to avoid clutter:
source('/Users/chrisweber/open_projects/panel_models/config.r')

##### Data Recodes #####
load("data/panel2012.RData")
load("data/panel2000.RData")
load("data/panel2012.RData")
load("data/panel2020.RData")
##############################################################################
##############################################################################
##########  2000  - 2004 #####################
##############################################################################
##############################################################################
##############################################################################

d2<-subset(d2, white.2000==1)
names(d2)


d2$id<-c(1:dim(d2)[1])
d2$pid3.1<-pid.r(d2$pid.2000)
d2$pid3.2<-pid.r(d2$pid.2002)
d2$pid3.3<-pid.r(d2$pid.2004)

prop.table(table(d2$pid3.1, d2$pid3.3), 1)
dim(model.matrix(lm(d2$pid3.1~d2$pid3.3)))

##############################################################################
DATA            =    d2[,c("auth1.2000", "auth2.2000", "auth3.2000", "auth4.2000",
                            "college.2000", "age.2000", "sex.2000", "income.2000",
                           "catholic.2000", "jewish.2000", "pid3.1", "pid3.2", "pid3.3", "other.2000")] %>% na.omit()
FORMULA         =  "latent =~ auth1.2000 + auth2.2000 + auth3.2000 + auth4.2000"
ORDERED         =  c("auth1.2000", "auth2.2000", "auth3.2000", "auth4.2000")
CAT_LABELS      =  c("Democrat", "Lean Democrat", "Independent", "Lean Republican",  "Republican")


#### Append the data with the latent variable
tmp_data = latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

## Transition matrix starts ####
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)

data_long<-data.frame(id=rep(1:length(tmp_data$id), times=3), state=c(tmp_data$pid3.1, tmp_data$pid3.2, tmp_data$pid3.3)%>% as.numeric(),
                 authoritarianism=rep(tmp_data$latent, times=3),
                 sex=rep(tmp_data$sex.2000, times=3),
                 college=rep(tmp_data$college.2000, times=3),
                 income=rep(tmp_data$income.2000, times=3),
                 age=zero.one(rep(tmp_data$age.2000, times=3)),
                 jewish=rep(tmp_data$jewish.2000, times=3),
                 catholic=rep(tmp_data$catholic.2000, times=3),
                 other=rep(tmp_data$other.2000, times=3),
                 time=c(rep(c(1:3), each=length(tmp_data$id) )))%>%  
                 arrange(id, time)%>%na.omit()
statetable.msm(state, id, data_long)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                c(0.1,0, 0.10, 0, 0),
                c(0, 0.10, 0, 0.1, 0),
                c(0 ,0, 0.10, 0, 0.1),
                c(0 ,0, 0, 0.1, 0)
)



model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age + other, 
           subject=id, data=data_long,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", 
           control=list(trace=1, 
           REPORT=1,fnscale=10, maxit=10000))
#### Predictions for low level
low <- pmatrix.msm(model, t=2, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))

#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=2, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS
### Fixed parameter values ######

MOD1 = as.formula(pid3.1~ latent + sex.2000 + college.2000 + 
                        income.2000 + age.2000 + jewish.2000 + catholic.2000 + other.2000)
MOD2 = as.formula(pid3.1~ latent + sex.2000 + college.2000 + 
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
                                   axis.label.size = 10
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
                                   axis.label.size = 10
                                   )


#dev.new()
  plt = ggdraw() +
    draw_plot(high_auth[[1]], 0, 0.05,   width = 0.85,   height =   0.45) +   
    draw_plot(low_auth[[1]],  0, 0.5,    width = 0.85,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.82, y = 0.05, width = 0.11, height = 0.40) +
    draw_plot(low_auth[[2]],  x =  0.82, y = 0.49, width = 0.11, height = 0.40) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0, -0.02), y = c(0.96, 0.51))  +
    draw_figure_label("Transition Matrices, By Authoritarianism",
                      position = "top.left", size = 13, fontface = "bold")

save_plot("e2004.pdf", plt, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)

qratio.msm(model, ind1=c(1,2), ind2=c(2,1),
           covariates=list(authoritarianism=0) )

qratio.msm(model, ind1=c(4,5), ind2=c(5,4),
           covariates=list(authoritarianism=1) )

####### 2012 - 2016 #############
d4$id<-c(1:dim(d4)[1])
d4$pid3.1<-pid.r(d4$pid.2012b)
d4$pid3.2<-pid.r(d4$pid.2016)
d4$pid3.3<-pid.r(d4$pid.2020)
d4 = subset(d4, white.2012 == 1)

##############################################################################
DATA            =    d4[,c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016",
                           "college.2012", "age.2012", "sex.2012", "income.2012",
                           "catholic.2012", "jewish.2012", "pid3.1", "pid3.2", "pid3.3", "other.2012")] %>% na.omit()
FORMULA         =  "latent =~ auth1.2016 + auth2.2016 + auth3.2016 + auth4.2016"
ORDERED         =  c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016")
CAT_LABELS      =  c("Democrat", "Lean Democrat", "Independent", "Lean Republican",  "Republican")
#### Append the data with the latent variable
tmp_data = latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

## Transition matrix starts ####
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)

data_long<-data.frame(id=rep(1:length(tmp_data$id), times=2), state=c(tmp_data$pid3.1, tmp_data$pid3.2),
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
statetable.msm(state, id, data_long)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                c(0.1,0, 0.10, 0, 0),
                c(0, 0.10, 0, 0.1, 0),
                c(0 ,0, 0.10, 0, 0.1),
                c(0 ,0, 0, 0.1, 0)
)

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age + other, 
           subject=id, data=data_long,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", 
           control=list(trace=1, 
           REPORT=1,fnscale=10, maxit=10000))
#### Predictions for low level
low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))

#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS

### Fixed parameter values ######
MOD1 = as.formula(pid3.1~ latent + sex.2012 + college.2012 + 
                        income.2012 + age.2012 + jewish.2012 + catholic.2012 + other.2012)
MOD2 = as.formula(pid3.1~ latent + sex.2012 + college.2012 + 
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
                                   axis.label.size = 10
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
                                   axis.label.size = 10
                                   )


library(cowplot)
  plt = ggdraw() +
    draw_plot(high_auth[[1]], 0, 0.05,   width = 0.85,   height =   0.45) +   
    draw_plot(low_auth[[1]],  0, 0.5,    width = 0.85,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.82, y = 0.05, width = 0.11, height = 0.40) +
    draw_plot(low_auth[[2]],  x =  0.82, y = 0.49, width = 0.11, height = 0.40) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0, -0.02), y = c(0.96, 0.51))  +
    draw_figure_label("Transition Matrices, By Authoritarianism",
                      position = "top.left", size = 13, fontface = "bold")

save_plot("e2012.pdf", plt, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)


qratio.msm(model, ind1=c(1,2), ind2=c(2,1),
           covariates=list(authoritarianism=0) )

qratio.msm(model, ind1=c(4,5), ind2=c(5,4),
           covariates=list(authoritarianism=1) )


###########   Read in Chris's Data ###########

setwd("/Users/chrisweber/Desktop/tmp_files/")

#reading in data 
library(haven)
library(dplyr)
data <- read_dta(file="anes1620.dta")
data$pid3_1 = round(data$rpid116*6+1) %>% pid.r()
data$pid3_2 = round(data$rpid120*6+1)  %>% pid.r()

## religious group (reference = protestant) 2020
data$cath16<-ifelse(data$V161247a==2 | data$V161247b==2 , 1, ifelse(data$V161244>0, 0, NA))
data$jew16<-ifelse(data$V161247a==3 | data$V161247b==3, 1, ifelse(data$V161244>0, 0, NA))
data$other16<-ifelse((data$V161247a!=1 & data$V161247b!=1) & data$cath16!=1 & data$jew16!=1, 1, ifelse(data$V161244>0, 0, NA))

## religious group (reference = protestant) 2020
data$cath20<-ifelse(data$V201435==2, 1, ifelse(data$V201435>0, 0, NA))
data$jew20<-ifelse(data$V201435==5, 1, ifelse(data$V201435>0, 0, NA))
data$other20<-ifelse(data$V201435!=1 & data$cath20!=1 & data$jew20!=1, 1, ifelse(data$V201435>0, 0, NA))

## services and spending
data$rserv16<-ifelse(data$V161178>0 & data$V161178<8,((7-data$V161178)/6),NA)
data$rserv20<-ifelse(data$V201246>0 & data$V201246<8,((7-data$V201246)/6),NA)

## authoritarianism scales
psych::alpha(with(data, cbind(rap161, rap162, rap163, rap164)))
data$ra16<-rowMeans(with(data, cbind(rap161, rap162, rap163, rap164)))
psych::alpha(with(data, cbind(rap201, rap202, rap203, rap204)))
data$ra20<-rowMeans(with(data, cbind(rap201, rap202, rap203, rap204)))

## authoritarianism variables, recoded to run 0-4.
data$ra164<-4*data$rauth16
data$ra204<-4*data$ra20

######
data = subset(data, white16==1)

tmp_data            =    data[,c("rauth16", 
                            "educ1601", "ra16", "male16", "rinc01",
                           "cath16", "jew20", "pid3_1", 
                           "pid3_2", "other16")] %>% na.omit()

CAT_LABELS      =  c("Democrat", "Lean Democrat", "Independent", "Lean Republican",  "Republican")


data_long<-data.frame(
                 id=rep(1:nrow(tmp_data), times=2),
                 state=c(tmp_data$pid3_1, tmp_data$pid3_2),
                 authoritarianism=c(tmp_data$rauth16, tmp_data$rauth16),
                 sex=rep(tmp_data$male16, times=2),
                 college=rep(tmp_data$educ1601, times=2),
                 income=rep(tmp_data$rinc01, times=2),
                 age=rep(tmp_data$ra16, times=2) %>% zero.one(),
                 jewish=rep(tmp_data$jew20, times=2),
                 other=rep(tmp_data$other16, times=2),
                 catholic=rep(tmp_data$cath16, times=2),
                 time=c(rep(c(1:2), each=nrow(tmp_data)))) %>%
                 arrange(id, time) %>%
                 na.omit()

statetable.msm(state, id, data_long)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                c(0.1,0, 0.10, 0, 0),
                c(0, 0.10, 0, 0.1, 0),
                c(0 ,0, 0.10, 0, 0.1),
                c(0 ,0, 0, 0.1, 0)
)

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age + other, 
           subject=id, data=data_long,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", 
           control=list(trace=1, 
           REPORT=1,fnscale=100, maxit=10000))
#### Predictions for low level
low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))

#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS






















######################################################
######################################################
###### 2016 - 2020  #####
######################################################
######################################################
d7 = subset(d7, white.2016 == 1)
#dim(model.matrix(lm(d7$pid3.1~d7$pid3.2)))
d7$pid3.1 <- pid.r(d7$pid.2016)
d7$pid3.2 <- pid.r(d7$pid.2020)

DATA            =    d7[,c("auth.1.2016", "auth.2.2016", "auth.3.2016", "auth.4.2016",
                            "college.2016", "age.2016", "female.2016", "income.2016",
                           "catholic.2016", "jewish.2016", "pid3.1", "pid3.2", "other.2016")] %>% na.omit()
FORMULA         =  "latent =~ auth.1.2016 + auth.2.2016 + auth.3.2016 + auth.4.2016"
ORDERED         =  c("auth.1.2016", "auth.2.2016", "auth.3.2016", "auth.4.2016")
CAT_LABELS      =  c("Democrat", "Lean Democrat", "Independent", "Lean Republican",  "Republican")

#### Append the data with the latent variable
tmp_data = tmp_data2 =  latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

tmp_data = tmp_data1 =  tmp_data[,names(tmp_data) %in% c("latent", "college.2016", "age.2016", "female.2016", "income.2016",
                           "catholic.2016", "jewish.2016", "pid3.1", "pid3.2", "other.2016")] 
tmp_data = tmp_data  %>%  select(order(colnames(tmp_data))) 

names(tmp_data1) = c("age", "catholic", "college", "female", "income",
                           "jewish", "latent", "other", "pid1", "pid2")


### Constuct MSM data frame. This is really just a long representation that can likely be accomplished with reshape2::melt
data_long<-data.frame(
                 id=rep(1:nrow(tmp_data), times=2),
                 state=c(tmp_data$pid3.1, tmp_data$pid3.2),
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

statetable.msm(state, id, data_long)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                c(0.1,0, 0.10, 0, 0),
                c(0, 0.10, 0, 0.1, 0),
                c(0 ,0, 0.10, 0, 0.1),
                c(0 ,0, 0, 0.1, 0)
)

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age + other, 
           subject=id, data=data_long,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", 
           control=list(trace=1, 
           REPORT=1,fnscale=100, maxit=10000))
#### Predictions for low level
low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))

#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS

### Fixed parameter values ######
MOD1 = as.formula(pid3.1~ latent + female.2016 + college.2016 + 
                        income.2016 + age.2016 + jewish.2016 + catholic.2016 + other.2016)
MOD2 = as.formula(pid3.1~ latent + female.2016 + college.2016 + 
                        income.2016 + age.2016 + jewish.2016 + catholic.2016 + other.2016)

high_auth <- draw_transition_effects(tran = hi, 
                                   data = tmp_data,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "AUTH",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2016 Election",
                                   y.label = "2020 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10
                                   )
low_auth <- draw_transition_effects(tran = low, 
                                   data = tmp_data,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "NON",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2016 Election",
                                   y.label = "2020 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10
                                   )


  plt = ggdraw() +
    draw_plot(high_auth[[1]], 0, 0.05,   width = 0.85,   height =   0.45) +   
    draw_plot(low_auth[[1]],  0, 0.5,    width = 0.85,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.82, y = 0.05, width = 0.11, height = 0.40) +
    draw_plot(low_auth[[2]],  x =  0.82, y = 0.49, width = 0.11, height = 0.40) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0, -0.02), y = c(0.96, 0.51))  +
    draw_figure_label("Transition Matrices, By Authoritarianism",
                      position = "top.left", size = 13, fontface = "bold")

save_plot("e2020_ANES.pdf", plt, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)



####### 2012 - 2016 #############
d4$id<-c(1:dim(d4)[1])
d4$pid3.1<-pid.r(d4$pid.2012b)
d4$pid3.2<-pid.r(d4$pid.2016)
d4$pid3.3<-pid.r(d4$pid.2020)
d4 = subset(d4, white.2012 == 1)

##############################################################################
DATA            =    d4[,c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016",
                           "college.2012", "age.2012", "sex.2012", "income.2012",
                           "catholic.2012", "jewish.2012", "pid3.1", "pid3.2", "pid3.3", "other.2012")] %>% na.omit()
FORMULA         =  "latent =~ auth1.2016 + auth2.2016 + auth3.2016 + auth4.2016"
ORDERED         =  c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016")
CAT_LABELS      =  c("Democrat", "Lean Democrat", "Independent", "Lean Republican",  "Republican")
#### Append the data with the latent variable
tmp_data = latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

## Transition matrix starts ####
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)

data_long<-data.frame(id=rep(1:length(tmp_data$id), times=2), state=c(tmp_data$pid3.1, tmp_data$pid3.2),
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
statetable.msm(state, id, data_long)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                c(0.1,0, 0.10, 0, 0),
                c(0, 0.10, 0, 0.1, 0),
                c(0 ,0, 0.10, 0, 0.1),
                c(0 ,0, 0, 0.1, 0)
)

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age + other, 
           subject=id, data=data_long,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", 
           control=list(trace=1, 
           REPORT=1,fnscale=10, maxit=10000))
#### Predictions for low level
low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))

#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS

### Fixed parameter values ######
MOD1 = as.formula(pid3.1~ latent + sex.2012 + college.2012 + 
                        income.2012 + age.2012 + jewish.2012 + catholic.2012 + other.2012)
MOD2 = as.formula(pid3.1~ latent + sex.2012 + college.2012 + 
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
                                   axis.label.size = 10
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
                                   axis.label.size = 10
                                   )


library(cowplot)
  plt = ggdraw() +
    draw_plot(high_auth[[1]], 0, 0.05,   width = 0.85,   height =   0.45) +   
    draw_plot(low_auth[[1]],  0, 0.5,    width = 0.85,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.82, y = 0.05, width = 0.11, height = 0.40) +
    draw_plot(low_auth[[2]],  x =  0.82, y = 0.49, width = 0.11, height = 0.40) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0, -0.02), y = c(0.96, 0.51))  +
    draw_figure_label("Transition Matrices, By Authoritarianism",
                      position = "top.left", size = 13, fontface = "bold")

save_plot("e2012.pdf", plt, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)


qratio.msm(model, ind1=c(1,2), ind2=c(2,1),
           covariates=list(authoritarianism=0) )

qratio.msm(model, ind1=c(4,5), ind2=c(5,4),
           covariates=list(authoritarianism=1) )

####### 2012 - 2016 #############
d4$id<-c(1:dim(d4)[1])
d4$pid3.1<-pid.r(d4$pid.2012b)
d4$pid3.2<-pid.r(d4$pid.2016)
d4$pid3.3<-pid.r(d4$pid.2020)
d4 = subset(d4, white.2012 == 1)

##############################################################################
DATA            =    d4[,c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016",
                           "college.2012", "age.2012", "sex.2012", "income.2012",
                           "catholic.2012", "jewish.2012", "pid3.1", "pid3.2", "pid3.3", "other.2012")] %>% na.omit()
FORMULA         =  "latent =~ auth1.2016 + auth2.2016 + auth3.2016 + auth4.2016"
ORDERED         =  c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016")
CAT_LABELS      =  c("Democrat", "Lean Democrat", "Independent", "Lean Republican",  "Republican")
#### Append the data with the latent variable
tmp_data = latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

## Transition matrix starts ####
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)

data_long<-data.frame(id=rep(1:length(tmp_data$id), times=2), state=c(tmp_data$pid3.1, tmp_data$pid3.2),
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
statetable.msm(state, id, data_long)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                c(0.1,0, 0.10, 0, 0),
                c(0, 0.10, 0, 0.1, 0),
                c(0 ,0, 0.10, 0, 0.1),
                c(0 ,0, 0, 0.1, 0)
)

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age + other, 
           subject=id, data=data_long,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", 
           control=list(trace=1, 
           REPORT=1,fnscale=10, maxit=10000))
#### Predictions for low level
low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))

#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS

### Fixed parameter values ######
MOD1 = as.formula(pid3.1~ latent + sex.2012 + college.2012 + 
                        income.2012 + age.2012 + jewish.2012 + catholic.2012 + other.2012)
MOD2 = as.formula(pid3.1~ latent + sex.2012 + college.2012 + 
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
                                   axis.label.size = 10
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
                                   axis.label.size = 10
                                   )


library(cowplot)
  plt = ggdraw() +
    draw_plot(high_auth[[1]], 0, 0.05,   width = 0.85,   height =   0.45) +   
    draw_plot(low_auth[[1]],  0, 0.5,    width = 0.85,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.82, y = 0.05, width = 0.11, height = 0.40) +
    draw_plot(low_auth[[2]],  x =  0.82, y = 0.49, width = 0.11, height = 0.40) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0, -0.02), y = c(0.96, 0.51))  +
    draw_figure_label("Transition Matrices, By Authoritarianism",
                      position = "top.left", size = 13, fontface = "bold")

save_plot("e2012.pdf", plt, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)


qratio.msm(model, ind1=c(1,2), ind2=c(2,1),
           covariates=list(authoritarianism=0) )

qratio.msm(model, ind1=c(4,5), ind2=c(5,4),
           covariates=list(authoritarianism=1) )



##############################################################################
##############################################################################
##########  2016  - 2020 ANES (d7) #####################
##############################################################################
##########    2016- 2020 ################
prop.table(table(d7$pid3.1, d7$pid3.2), 1)
d7 = subset(d7, white.2016 == 1)
#dim(model.matrix(lm(d7$pid3.1~d7$pid3.2)))
d7$auth.w1 = zero.one(with(d7, rowMeans(cbind(auth.1.2016, auth.2.2016, auth.3.2016, auth.4.2016), na.rm = T)))
d7$auth.w2 = zero.one(with(d7, rowMeans(cbind(auth.1.2020, auth.2.2020, auth.3.2020, auth.4.2020), na.rm = T)))
d7$pid3.1 <- pid.r(d7$pid.2016)
d7$pid3.2 <- pid.r(d7$pid.2020)
prop.table(table(d7$pid3.1,  d7$pid3.2), 1)
dim(model.matrix(lm(d7$pid3.1~d7$pid3.2)))
d7$auth.w1 = zero.one(with(d7, rowMeans(cbind(auth.1.2016, auth.2.2016, auth.3.2016, auth.4.2016), na.rm = T)))
d7$auth.w2 = zero.one(with(d7, rowMeans(cbind(auth.1.2020, auth.2.2020, auth.3.2020, auth.4.2020), na.rm = T)))
d7$authoritarianism = zero.one(with(d7, rowMeans(cbind(auth.1.2016, auth.2.2016, auth.3.2016, auth.4.2016,
                                                       auth.1.2020, auth.2.2020, auth.3.2020, auth.4.2020), na.rm = T)))
##############################################################################
DATA            =    d7[,c("auth.1.2016", "auth.2.2016", "auth.3.2016", "auth.4.2016",
                           "authoritarianism", "college.2016", "age.2016", "female.2016", "income.2016",
                           "catholic.2016", "jewish.2016", "pid3.1", "pid3.2", "other.2016")] %>% na.omit()
FORMULA         =  "latent =~ auth.1.2016 + auth.2.2016 + auth.3.2016 + auth.4.2016"
ORDERED         =  c("auth.1.2016", "auth.2.2016", "auth.3.2016", "auth.4.2016")
CAT_LABELS      =  c("Democrat", "Lean Democrat", "Independent", "Lean Republican",  "Republican")

#### Append the data with the latent variable
tmp_data = latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

### Constuct MSM data frame. This is really just a long representation that can likely be accomplished with reshape2::melt
data_long<-data.frame(
                 id=rep(1:nrow(tmp_data), times=2),
                 state=c(tmp_data$pid3.1, tmp_data$pid3.2),
                 authoritarianism=c(tmp_data$latent, tmp_data$latent),
                 sex=rep(tmp_data$female.2016, times=2),
                 college=rep(tmp_data$college.2016, times=2),
                 income=rep(tmp_data$income.2016, times=2),
                 age=rep(tmp_data$age.2016, times=2) %>% zero.one(),
                 jewish=rep(tmp_data$jewish.2016, times=2),
                 other=rep(tmp_data$other.2016, times=2),
                 catholic=rep(tmp_data$catholic.2016, times=2),
                 time=c(rep(c(1:2), each=nrow(tmp_data)))) %>%
                 arrange(id, time)%>%
                 na.omit()
## Transition matrix starts ####
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+other+age , 
           subject=id, data=data_long,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", 
           control=list(trace=1, 
           REPORT=1,fnscale=10, maxit=10000))
#### Predictions or low level
low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS


### Post estimation stuff:
# #Ratio of transition intensitites.

# Authoritarians move to the Right more than non-Authoritarians move to the left #
qratio.msm(model, ind1=c(2,1), ind2=c(1,2),
           covariates=list(authoritarianism=0) )

qratio.msm(model, ind1=c(4,5), ind2=c(5,4),
           covariates=list(authoritarianism=1) )



qratio.msm(model, ind1=c(3,2), ind2=c(2,3),
           covariates=list(authoritarianism=0) )
qratio.msm(model, ind1=c(3,4), ind2=c(4,3),
           covariates=list(authoritarianism=1) )
qratio.msm(model, ind1=c(4,5), ind2=c(5,4),
           covariates=list(authoritarianism=1) )


### Fixed parameter values ######
MOD1 = as.formula(pid3.1~ latent + female.2016 + college.2016 + 
                           income.2016 + age.2016 + jewish.2016 + catholic.2016 + other.2016)
MOD2 = as.formula(pid3.2~ latent + female.2016 + college.2016 + 
                           income.2016 + age.2016 + jewish.2016 + catholic.2016 + other.2016)

### Pass the transition matrices to a plot function 

high_auth <- draw_transition_effects(tran = hi, 
                                   data = tmp_data,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "AUTH",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2016 Election",
                                   y.label = "2020 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10
                                   )
low_auth <- draw_transition_effects(tran = low, 
                                   data = tmp_data,
                                   mod1 = MOD1,
                                   mod2 = MOD2, 
                                   auth_score = "NON",
                                   CAT_LABELS = CAT_LABELS,
                                   x.label = "2016 Election",
                                   y.label = "2020 Election",
                                   alpha = 0.25,
                                   axis.text.size = 10,
                                   axis.label.size = 10
                                   )


#dev.new()
  plt = ggdraw() +
    draw_plot(high_auth[[1]], 0, 0.05,   width = 0.85,   height =   0.45) +   
    draw_plot(low_auth[[1]],  0, 0.5,    width = 0.85,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.82, y = 0.05, width = 0.11, height = 0.40) +
    draw_plot(low_auth[[2]],  x =  0.82, y = 0.49, width = 0.11, height = 0.40) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0, -0.02), y = c(0.96, 0.51))  +
    draw_figure_label("Transition Matrices, By Authoritarianism",
                      position = "top.left", size = 13, fontface = "bold")

save_plot("e2020.pdf", plt, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)

##### Merged data analysis ###

###### Pooling Data ##########
####### 2012 - 2016 #############
d4$id<-c(1:dim(d4)[1])
d4$pid3.1<-pid.r(d4$pid.2012b)
d4$pid3.2<-pid.r(d4$pid.2016)
d4$pid3.3<-pid.r(d4$pid.2020)
d4 = subset(d4, white.2012 == 1)

##############################################################################
DATA            =    d4[,c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016",
                           "college.2012", "age.2012", "sex.2012", "income.2012",
                           "catholic.2012", "jewish.2012", "pid3.1", "pid3.2", "pid3.3", "other.2012")] %>% na.omit()
FORMULA         =  "latent =~ auth1.2016 + auth2.2016 + auth3.2016 + auth4.2016"
ORDERED         =  c("auth1.2016", "auth2.2016", "auth3.2016", "auth4.2016")
CAT_LABELS      =  c("Democrat", "Lean Democrat", "Independent", "Lean Republican",  "Republican")
#### Append the data with the latent variable
tmp_data = tmp_data1 = latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

tmp_data1 = tmp_data1[,names(tmp_data1) %in% c("latent", "college.2012", "age.2012", "sex.2012", "income.2012",
                           "catholic.2012", "jewish.2012", "pid3.1", "pid3.2", "other.2012")] 
tmp_data1 = tmp_data1 %>%  select(order(colnames(tmp_data1))) 
names(tmp_data1) = c("age", "catholic", "college", "income",
                           "jewish", "latent", "other", "pid1", "pid2", "female")

data_long1<-data.frame(id=rep(1:length(tmp_data$id), times=2), state=c(tmp_data$pid3.1, tmp_data$pid3.2),
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
                           "authoritarianism", "college.2016", "age.2016", "female.2016", "income.2016",
                           "catholic.2016", "jewish.2016", "pid3.1", "pid3.2", "other.2016")] %>% na.omit()
FORMULA         =  "latent =~ auth.1.2016 + auth.2.2016 + auth.3.2016 + auth.4.2016"
ORDERED         =  c("auth.1.2016", "auth.2.2016", "auth.3.2016", "auth.4.2016")
CAT_LABELS      =  c("Democrat", "Lean Democrat", "Independent", "Lean Republican",  "Republican")

#### Append the data with the latent variable
tmp_data = tmp_data2 =  latent_model(formula = FORMULA,  
                        ordered = ORDERED,
                        data = DATA)  #### Return the original data frame with latent authoritarianism scores, 0 - 1 coded

tmp_data2 = tmp_data2[,names(tmp_data2) %in% c("latent", "college.2016", "age.2016", "female.2016", "income.2016",
                           "catholic.2016", "jewish.2016", "pid3.1", "pid3.2", "other.2016")] 

tmp_data2 = tmp_data2 %>%  select(order(colnames(tmp_data2))) 

names(tmp_data2) = c("age", "catholic", "college", "female", "income",
                           "jewish", "latent", "other", "pid1", "pid2")

### Constuct MSM data frame. This is really just a long representation that can likely be accomplished with reshape2::melt
data_long2<-data.frame(
                 id=range(data_long1)[2] + rep(1:nrow(tmp_data), times=2),
                 state=c(tmp_data$pid3.1, tmp_data$pid3.2),
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

## Transition matrix starts ####
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+other+age , 
           subject=id, data=data_long,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", 
           control=list(trace=1, 
           REPORT=1,fnscale=10, maxit=10000))


low <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
#### Predictions for high levels of authoritarianism ####
hi  <- pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1)) %>% c() %>% matrix(ncol = length(CAT_LABELS))
rownames(low) <- rownames(hi) <- colnames(low) <- colnames(hi) <- CAT_LABELS
### Fixed parameter values ######
MOD1 = as.formula(pid1~ latent + female + college + 
                        income + age + jewish + catholic + other)
MOD2 = as.formula(pid1 ~ latent + female + college + 
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
                                   axis.label.size = 10
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
                                   axis.label.size = 10
                                   )


  plt = ggdraw() +
    draw_plot(high_auth[[1]], 0, 0.05,   width = 0.85,   height =   0.45) +   
    draw_plot(low_auth[[1]],  0, 0.5,    width = 0.85,    height = 0.45)  +
    draw_plot(high_auth[[2]], x =  0.82, y = 0.05, width = 0.11, height = 0.40) +
    draw_plot(low_auth[[2]],  x =  0.82, y = 0.49, width = 0.11, height = 0.40) +
    draw_plot_label(label = c("A. Non-Authoritarian", 
                              "    B. Authoritarian"), size = 12, 
                    x = c(0, -0.02), y = c(0.96, 0.51))  +
    draw_figure_label("Transition Matrices, By Authoritarianism",
                      position = "top.left", size = 13, fontface = "bold")

save_plot("e2020_merged.pdf", plt, ncol = 2, base_asp = 1.68, base_height = 5, base_width = 5)


### Create function to simulate stuff with the MSM ###

