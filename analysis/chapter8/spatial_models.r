	
rm(list = ls())
# install.packages("brms")
# install.packages("tidyverse")
# install.packages("modelr")
# install.packages("tidybayes")

library(brms)
library(modelr)
library(tidybayes)
#library(cowplot)
source("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/configurations/configurations.r")
source("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/configurations/user_functions.r")
data_location = "/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data/"
setwd("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/Chapters/Chapters/Chapter8")
# for all chapters
##### Data Recodes #####
load("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data/pooled.auth.rda")  ### Just work from this data; everything should be here, recoded.
data$authoritarianism<-(rowMeans(cbind(data$auth.1.x, data$auth.2.x,data$auth.3.x, data$auth.4.x), na.rm=T)-1)/1
data$party3<-car::recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
data$republican<-car::recode(data$pid*6+1, "1:2=0; 3:5=0; 6:7=1" )
data$democrat<-car::recode(data$pid*6+1, "1:2=1; 3:5=0; 6:7=0" )
data$independent<-car::recode(data$pid*6+1, "1:2=0; 3:5=1; 6:7=0" )
cor.test(data$know.interview.pre, data$know.interview.post)
data$knowledge<-(rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm=T)-1)/4
  #### Difference Measure ####
  tapply(data$ideologyR, data$year, mean, na.rm=T)
  tapply(data$ideologyCR, data$year, mean, na.rm=T)
  tapply(data$ideologyCD, data$year, mean, na.rm=T)
  tapply(data$ideologyCR, data$year, mean, na.rm=T)
  tapply(data$jobsD, data$year, mean, na.rm=T)
  tapply(data$jobsR, data$year, mean, na.rm=T)
  tapply(data$jobs, data$year, mean, na.rm=T)
  tapply(data$jobsCR, data$year, mean, na.rm=T)
  tapply(data$jobsCD, data$year, mean, na.rm=T)
  
  tapply(data$gov.servicesCR, data$year, mean, na.rm=T)
  tapply(data$gov.servicesCD, data$year, mean, na.rm=T)
  tapply(data$gov.services, data$year, mean, na.rm=T)

  #### Basic measurement properties
  psych::alpha(cbind(data$ideologyR, data$ideologyCR,
                     data$jobsR, data$jobsCR,
                     data$gov.servicesCR ,data$gov.servicesR)) 
  psych::alpha(cbind(data$ideologyD, data$ideologyCD,
                     data$jobsD, data$jobsCD,
                     data$gov.servicesCD ,data$gov.servicesD))
  psych::alpha(cbind(data$ideology,
                     data$jobs,
                     data$gov.services))

   # # data$ideologyR<-(rowMeans(cbind(data$ideologyR, data$ideologyCR,
   #                                 data$jobsR, data$jobsCR,
   #                                 data$gov.servicesCR ,data$gov.servicesR), na.rm=T)-1)/6
   # 
   # # data$ideologyD<-(rowMeans(cbind(data$ideologyD, data$ideologyCD,
   #                                  data$jobsD, data$jobsCD,
   #                                  data$gov.servicesCD , data$gov.servicesD), na.rm=T)-1)/6
   # # data$ideology<-(rowMeans(cbind(data$ideology,
   #                                  data$jobs,
   #                                  data$gov.services), na.rm=T)-1)/6
  # data$ideologyR<-(data$ideologyR-1)/6
  # data$ideologyD<-(data$ideologyD-1)/6
  # data$ideology<-(data$ideology-1)/6
  # 
  #
  data$ideologyR<-(rowMeans(cbind(data$ideologyR, data$ideologyCR), na.rm=T)-1)/6
  
  data$ideologyD<-(rowMeans(cbind(data$ideologyD, data$ideologyCD) 
                            , na.rm=T)-1)/6
  data$ideology<-(data$ideology-1)/6
  
  data$difference<-data$ideologyR-data$ideologyD
  data$differenceR1<-data$ideology-data$ideologyR
  data$differenceD1<-data$ideology-data$ideologyD
  data$proximity<-(data$ideology-data$ideologyD)^2-(data$ideology-data$ideologyR)^2
  data$differenceR2<-(data$ideology-data$ideologyR)^2
  data$differenceD2<-(data$ideology-data$ideologyD)^2  
  data$mode<-as.character(data$mode)
  data<-subset(data, mode=="FTF"|mode=="FTC/CASI" ) ## Drop 1990
  data$diffR<-data$ideology-data$ideologyR
  data$diffD<-data$ideology-data$ideologyD


### I create smaller, more tractable versions of the data rather than operating on the full data frame, data.
tmp_dat = data[,c("diffR", "diffD", "authoritarianism", 
                 "female", "age", "college", "income",
                 "jewish", "catholic", "other", "year")] %>% na.omit() %>% 
                 mutate(authoritarianism_2 = authoritarianism*authoritarianism) 

library(brms)
fit1 <- brm(diffR~ female + age + college + income + jewish + 
                 catholic + other + authoritarianism + authoritarianism_2 + 
                 (1+authoritarianism + authoritarianism_2|year), 
                 family = "gaussian",
                 data = tmp_dat,
                  chains = 2, 
                  cores = 6, 
                  seed = 1234, 
                  iter = 4000)

fit2 <- brm(diffD~ female + age + college + income + jewish + 
                 catholic + other + authoritarianism + authoritarianism_2 + 
                 (1+authoritarianism + authoritarianism_2|year), 
                 family = "gaussian",
                 data = tmp_dat,
                  chains = 2, 
                  cores = 6, 
                  seed = 1234, 
                  iter = 4000)

fit3 <- brm(diffR~ female + age + college + income + jewish + 
                 catholic + other + authoritarianism + authoritarianism_2 + 
                 authoritarianism:college + authoritarianism_2:college+
                 (1+authoritarianism + authoritarianism_2|year), 
                 family = "gaussian",
                 data = tmp_dat,
                  chains = 2, 
                  cores = 6, 
                  seed = 1234, 
                  iter = 4000)

fit4 <- brm(diffD~ female + age + college + income + jewish + 
                 catholic + other + authoritarianism + authoritarianism_2 + 
                 authoritarianism:college + authoritarianism_2:college+
                 (1+authoritarianism + authoritarianism_2|year), 
                 family = "gaussian",
                 data = tmp_dat,
                  chains = 2, 
                  cores = 6, 
                  seed = 1234, 
                  iter = 4000)








a<- as.formula(diffD~auth+
    female+age+college+income+
    jewish+catholic+other)
m1a<-lm(a, subset(y, year==1992))
m2a<-lm(a, subset(y, year==2000))
m3a<-lm(a, subset(y, year==2004))
m4a<-lm(a, subset(y, year==2008))
m5a<-lm(a, subset(y, year==2012))
m6a<-lm(a, subset(y, year==2016))
a<- as.formula(diffR~auth+
                 female+age+college+
                 income+
                 jewish+catholic+
                 other)
m1b<-lm(a, subset(y, year==1992))
m2b<-lm(a, subset(y, year==2000))
m3b<-lm(a, subset(y, year==2004))
m4b<-lm(a, subset(y, year==2008))
m5b<-lm(a, subset(y, year==2012))
m6b<-lm(a, subset(y, year==2016))


data1<-rbind(pred.ols(m1a, 1000)[[1]],pred.ols(m2a, 1000)[[1]],
      pred.ols(m3a, 1000)[[1]],pred.ols(m4a, 1000)[[1]],
      pred.ols(m5a, 1000)[[1]],pred.ols(m6a, 1000)[[1]],
      pred.ols(m1a, 1000)[[2]],pred.ols(m2a, 1000)[[2]],
      pred.ols(m3a, 1000)[[2]],pred.ols(m4a, 1000)[[2]],
      pred.ols(m5a, 1000)[[2]],pred.ols(m6a, 1000)[[2]]
      )
names(data1)<-c("min1", "min2", "max2", "max1", "mean")
data1$year<-rep(c(1992,2000,2004, 2008, 2012, 2016), times=2)   
data1$authoritarianism<-rep(c("High", "Low"), each=6)      

data2<-rbind(pred.ols(m1b, 1000)[[1]],pred.ols(m2b, 1000)[[1]],
             pred.ols(m3b, 1000)[[1]],pred.ols(m4b, 1000)[[1]],
             pred.ols(m5b, 1000)[[1]],pred.ols(m6b, 1000)[[1]],
             pred.ols(m1b, 1000)[[2]],pred.ols(m2b, 1000)[[2]],
             pred.ols(m3b, 1000)[[2]],pred.ols(m4b, 1000)[[2]],
             pred.ols(m5b, 1000)[[2]],pred.ols(m6b, 1000)[[2]]
)
names(data2)<-c("min1", "min2", "max2", "max1", "mean")
data2$year<-rep(c(1992,2000,2004, 2008, 2012, 2016), times=2)   
data2$authoritarianism<-rep(c("High", "Low"), each=6)      

      

plot1<-ggplot(data = data1,
               aes(x = factor(year), 
                   y = mean, ymin=min1, ymax=max1, 
                   linetype=authoritarianism, colour=authoritarianism))+
  geom_point(position = position_dodge(width = 0.01)) +
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.01) +
  geom_errorbar(aes(x = as.factor(year), 
                    y = mean, ymin=min2, 
                    ymax=max2, linetype=authoritarianism),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  coord_flip()+
  scale_linetype_manual(name="Authoritarianism", values=c("solid", "solid"))+
  scale_colour_manual(name="Authoritarianism", values=c("black", "grey79"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Ideological Distance from Democratic Party") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=8,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=8, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=8,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=8,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Distance", limits=c(0,1))+
  scale_x_discrete("Year")+
  geom_hline(yintercept=0)

plot2<-ggplot(data = data2,
              aes(x = factor(year), 
                  y = mean, ymin=min1, ymax=max1, 
                  linetype=authoritarianism, colour=authoritarianism))+
  geom_point(position = position_dodge(width = 0.01)) +
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.01) +
  geom_errorbar(aes(x = as.factor(year), 
                    y = mean, ymin=min2, 
                    ymax=max2, linetype=authoritarianism),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  coord_flip()+
  scale_linetype_manual(name="Authoritarianism", values=c("solid", "solid"))+
  scale_colour_manual(name="Authoritarianism", values=c("black", "grey79"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Ideological Distance from Republican Party") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=8,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=8, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=8,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=8,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Distance", limits=c(-0.6,0.2))+
  scale_x_discrete("Year")+
  geom_hline(yintercept=0)

