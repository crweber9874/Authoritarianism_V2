rm(list = ls())
###  Data Description ##
# More transformations and data recodes.....
source("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/configurations/configurations.r")
### Set working directory 
### These are user functions
### 1990 ANES ####
data_location = "/Users/chrisweber/Dropbox/Data/cross_sectional"
setwd("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data")
detach("package:dplyr")
load("pooled.auth_ns.Rdata")

data$group = ifelse(data$white    == 1, 1, 0)
data$group = ifelse(data$black    == 1, 2, data$group)
data$group = ifelse(data$hispanic == 1, 3, data$group)

#### Subset to whites
data = subset(data, group ==1)
#### Drop mids
data<-subset(data, year!=1990) ## Drop 1990
data<-subset(data, year!=1994) ## Drop 1990
data$mode<-as.character(data$mode)
data<-subset(data, year ==2020 | (mode=="FTF"|mode=="FTC/CASI" ) )## Drop 1990
data$ideology<-zero.one(data$ideology)
psych::alpha(cbind(data$rr1, data$rr2, data$rr3, data$rr4))  #.86
data$racial.resentment<-zero.one(rowMeans(cbind(data$rr1, data$rr2, data$rr3, data$rr4), na.rm=T)) 


##### Create a latent authoritarianism score #####
data$authoritarianism <- zero.one(rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm=T))


data$split.ticket<-recode(as.character(data$split.house), "'DP-DC'=0; 'DP-RC'=1; 'RP-DC'=1; 'RP-RC'=0; else=NA")

### Overall Efficacy

psych::alpha(cbind(data$efficacy1, data$efficacy2, data$efficacy3, data$efficacy5, data$efficacy6, data$efficacy7))
cor.test(data$efficacy5, data$efficacy6)

data$efficacy<-rowMeans(cbind(data$efficacy1, data$efficacy2, data$efficacy3, data$efficacy5, data$efficacy6, data$efficacy7), na.rm=T)
data$knowledge<-((rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm=T)-1)/4)
data$age<-(data$age-17)/80

data$republican<-recode(data$pid*6+1, "1:2=0; 3:5=0; 6:7=1" )
data$democrat<-recode(data$pid*6+1, "1:2=1; 3:5=0; 6:7=0" )
data$independent<-recode(data$pid*6+1, "1:2=0; 3:5=1; 6:7=0" )
#data$party3<-recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
#data$republican<-recode(data$pid*6+1, "1:2=0; 4=0; 5:7=1" )
#data$democrat<-recode(data$pid*6+1, "1:3=1; 4=0; 5:7=0" )
#data$independent<-recode(data$pid*6+1, "1:3=0; 4=1; 4:7=0" )
data$party3<-recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )


with(data, psych::alpha(cbind(dem.decent, dem.know,
                              dem.leader, dem.cares)))

data$dem_traits = abs(zero.one(with(data, rowMeans(cbind(dem.know, dem.decent,
                                                         dem.leader, dem.cares), na.rm = T)))-1)


with(data, psych::alpha(cbind(rep.decent, rep.know,
                              rep.leader, rep.cares)))

data$rep_traits = abs(zero.one(with(data, rowMeans(cbind(rep.know, rep.decent,
                                                         rep.leader, rep.cares), na.rm = T)))-1)


data$dem_anxiety = zero.one(with(data, rowMeans(cbind(dem.angry, dem.afraid), na.rm = T)))
data$rep_anxiety = zero.one(with(data, rowMeans(cbind(rep.angry, rep.afraid), na.rm = T)))


### Create a directional Measures ####

data$out_animosity = ifelse(data$party3 == "Democrat", data$rep_anxiety, 
                            ifelse(data$party3 == "Republican", data$dem_anxiety, NA))

data$out_tanimosity = ifelse(data$party3 == "Democrat", data$rep_traits, 
                             ifelse(data$party3 == "Republican", data$dem_traits, NA))


data$participation<-rowSums(cbind(data$p1, data$p2, data$p3, data$p4, data$p5), na.rm=T)
data$AuthXRep<-data$authoritarianism*data$republican
data$AuthXInd<-data$authoritarianism*data$independent
psych::alpha(with(data, cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year==1992,], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year==2000,], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year==2004,], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year==2008,], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year==2012,], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year==2016,], cbind(egal1, egal2, egal3, egal4)))

psych::alpha(with(data, cbind(moral1, moral2, moral3, moral4)))

data$egalitarianism<-zero.one(rowMeans(with(data, cbind(egal1, egal2, egal3, egal4)),
                                       na.rm=T))
data$moral.traditionalism<-zero.one(rowMeans(with(data, cbind(moral1, moral2, moral3, moral4)),
                                             na.rm=T))
data$services = zero.one(data$gov.services)

### Construct Emotion Measures ###
cor.test(data$dem.angry, data$dem.afraid)
cor.test(data$rep.angry, data$rep.afraid)



##### Effects, Figures 1 -3, Vote, Candidate Affect, Partisan Affect

# Anti-Egalitarianism

# 
data = subset(data, white==1)
save(data, file = "pooled.auth.rda") # Only white respondents, per collective decision