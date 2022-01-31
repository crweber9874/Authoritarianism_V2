##### Chapter 5, V2:  Authoritarianism and its Relationship to PID among Whites #####
# Relies on pooled data.
# Source file generates effects

#tinytex::install_tinytex()
#install.packages("ggjoy")
detach("package:dplyr")
library(readstata13)
library(foreign)
library(psych)
library(ggjoy)
library(ltm)
library(dplyr)
library(lavaan)

setwd("/Users/chrisweber/authoritarianism_book_v2/")
load("pooled.auth.Rdata")
source("/Users/chrisweber/authoritarianism_book_v2/analysis/BookFunctions.r")
data$group = ifelse(data$white    == 1, 1, 0)
data$group = ifelse(data$black    == 1, 2, data$group)
data$group = ifelse(data$hispanic == 1, 3, data$group)
data = subset(data, group !=0)

data<-subset(data, year!=1990) ## Drop 1990
data<-subset(data, year!=1994) ## Drop 1990
data$mode<-as.character(data$mode)
################ UPDATE  ###########
data<-subset(data, year ==2020 | (mode=="FTF"|mode=="FTC/CASI" ) )## Drop 1990

data$ideology<-(data$ideology-1)/6
data$racial.resentment<-(rowMeans(cbind(data$rr1, data$rr2, data$rr3, data$rr4), na.rm=T)-1)/4
data$auth.observed<-(rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm=T)-1)/4
data$identifier = seq(1:nrow(data))
#### Estimate a latent variable model #### 
#### Just vanilla one factor model ####
#### with varying slopes (i don't know about this)


dat_fscores<-data.frame(identifier = data$identifier,
                        auth.1 = data$auth.1, 
                        auth.2 = data$auth.2, 
                        auth.3 = data$auth.3, 
                        auth.4 = data$auth.4,
                        group = data$group) %>%  na.omit()

mod <-  "latent =~ auth.1 + auth.2 + auth.3 + auth.4"
model1 = cfa(mod, ordered=names(dat_fscores)[2:5], 
             data=dat_fscores, group = "group")  
a= lavPredict(model1, append.data = TRUE,  assemble = TRUE) %>% as.data.frame()
## Test if order is wrong
test = data.frame(dat_fscores, a)
table(test$auth.1.1 == test$auth.1)
table(test$auth.2.1 == test$auth.2)
table(test$auth.3.1 == test$auth.3)
table(test$auth.4.1 == test$auth.4)
# All good
library(dplyr)
dat_fscores$latent = a$latent %>% zero.one()
data =  merge(data, dat_fscores, "identifier", all.x = T)
data$authoritarianism = data$latent
#### Sanity Check:
corr.test(data$authoritarianism, data$auth.observed) # Of course it makes no difference.
detach("package:dplyr")
library(car)
#data$knowledge<-rowMeans(cbind(data$knowledge.defensec, data$knowledge.govc, data$knowledge.governmentc), na.rm=T)
data$split.ticket<-recode(as.character(data$split.house), "'DP-DC'=0; 'DP-RC'=1; 'RP-DC'=1; 'RP-RC'=0; else=NA")
data$efficacy<-rowMeans(cbind(data$efficacy1, data$efficacy2, data$efficacy3, data$efficacy5, data$efficacy6, data$efficacy7), na.rm=T)
data$knowledge<-((rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm=T)-1)/4)
data$age<-(data$age-17)/80
data$interaction<-data$authoritarianism*data$pid
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

white.data<-subset(data, white==1)
black.data<-subset(data, black==1)
hispanic.data<-subset(data, hispanic==1)
nonwhite.data<-subset(data, nonwhite==1)

##### Effects, Figures 1 -3, Vote, Candidate Affect, Partisan Affect

setwd("/Users/chrisweber/authoritarianism_book_v2/figs")

detach("package:dplyr")
tt<-as.formula(vote~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other  )
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))
h<-glm(tt, data=subset(white.data, year==2020), family=binomial("logit"))


### Predictions for Vote ###########################

spaghetti.plot<-rbind(spaghetti(a),
                      spaghetti(b),
                      spaghetti(c),
                      spaghetti(d),
                      spaghetti(e),
                      spaghetti(f),
                      spaghetti(h)
)
spaghetti.plot$Authoritarianism<-rep(seq(0,100)/100, times=7)
spaghetti.plot$Year<-rep(c(1992,2000,2004,2008,2012,2016, 2020), each=101)
melted<-reshape2::melt(spaghetti.plot, id=952:953)

tapply(melted$value[melted$Authoritarianism==1], melted$Year[melted$Authoritarianism==1],
       quantile, 0.5)

tapply(melted$value[melted$Authoritarianism==0], melted$Year[melted$Authoritarianism==0],
       quantile, 0.5)

library(ggplot2)
#melted$Year<-rep(c(1992,2000,2004,2008,2012,2016), each=dim(melted)[1]/6)

#melted<-subset(melted, Year==2016)
plot1<- ggplot(data =melted,
               aes(x = Authoritarianism, 
                   y = value, group=variable))+
  geom_line(colour="lightgray", alpha=0.05)+guides(colour=FALSE) +
  geom_smooth(aes(group=1), se=FALSE, colour="black", size =0.5)+
  facet_wrap(~ Year)+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Authoritarianism and Presidential Vote. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=0,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=9,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=9, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability of Republican Vote", limits=c(0,1))+
  scale_x_continuous("Authoritarianism") 
plot1

dev.copy(png,'ch5_1.jpg',
         width = 750, height = 500,)
dev.off()

##### Figure 2 and Probs ###
detach("package:dplyr")
tt<-as.formula(vote~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other    )
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))
h<-glm(tt, data=subset(white.data, year==2020), family=binomial("logit"))


### Predictions for Vote ###########################
cov<-c(0,1)
vote.margin<-function(a){
  temp<-model.prediction(a, 
                         design.matrix.marginal(a, "authoritarianism"), "Marginal",
                         "Binary.Logit",1)
  return(temp)
}

plot.data<-rbind(vote.margin(a), vote.margin(b),
                 vote.margin(c), vote.margin(d),
                 vote.margin(e), vote.margin(f), vote.margin(h))  
plot.data$Year <- c(1992, 2000, 2004, 2008, 2012, 2016, 2020)

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992],
                                        plot.data$mean.score[plot.data$Year==2000])))
plot.data$Year[nrow(plot.data)]<-1996

plot1<-ggplot(data = plot.data,
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5))+
  geom_point(size=1.5) +
  geom_line(data = plot.data,
            aes(x = as.factor(Year), 
                y = mean.score, group=1),
            colour="#535353")+
  geom_errorbar(width = 0.10, alpha=0.5) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75),
                width = 0.01, size=0.5, alpha=0.7)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Republican Presidential Vote. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-.5,0.75))+
  scale_x_discrete("Year") + 
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") 


plot1

dev.copy(png,'ch5_2.jpg',
         width = 750, height = 500)
dev.off()




### Feelings towards the Political Candidates #####
#### Point estimate towards teh Parties
# Republicans
detach("package:dplyr")
tt<-as.formula(I(feeling.rep/100)~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_1<-lm(tt, data=subset(white.data, year==1992))
b_1<-lm(tt, data=subset(white.data, year==2000))
c_1<-lm(tt, data=subset(white.data, year==2004))
d_1<-lm(tt, data=subset(white.data, year==2008))
e_1<-lm(tt, data=subset(white.data, year==2012))
f_1<-lm(tt, data=subset(white.data, year==2016))
g_1<-lm(tt, data=subset(white.data, year==2020))

#summary(margins(a_1,  vce="simulation", iterations = 100L))
pred.feeling.noint(a_1)

pred.feeling(a_1,1)
pred.feeling(f_1,1)
# Republican Candidate
tt<-as.formula(I(feeling.repc/100)~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_2<-lm(tt, data=subset(white.data, year==1992))
b_2<-lm(tt, data=subset(white.data, year==2000))
c_2<-lm(tt, data=subset(white.data, year==2004))
d_2<-lm(tt, data=subset(white.data, year==2008))
e_2<-lm(tt, data=subset(white.data, year==2012))
f_2<-lm(tt, data=subset(white.data, year==2016))
g_2<-lm(tt, data=subset(white.data, year==2020))

pred.feeling(a_2,1)
pred.feeling(f_2,1)

# Democrats
tt<-as.formula(I(feeling.dem/100)~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_3<-lm(tt, data=subset(white.data, year==1992))
b_3<-lm(tt, data=subset(white.data, year==2000))
c_3<-lm(tt, data=subset(white.data, year==2004))
d_3<-lm(tt, data=subset(white.data, year==2008))
e_3<-lm(tt, data=subset(white.data, year==2012))
f_3<-lm(tt, data=subset(white.data, year==2016))
g_3<-lm(tt, data=subset(white.data, year==2020))

pred.feeling(a_3,1)
pred.feeling(f_3,1)
pred.feeling(g_3,1)


# Democratic Candidate
tt<-as.formula(I(feeling.demc/100)~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other  )
a_4<-lm(tt, data=subset(white.data, year==1992))
b_4<-lm(tt, data=subset(white.data, year==2000))
c_4<-lm(tt, data=subset(white.data, year==2004))
d_4<-lm(tt, data=subset(white.data, year==2008))
e_4<-lm(tt, data=subset(white.data, year==2012))
f_4<-lm(tt, data=subset(white.data, year==2016))
g_4<-lm(tt, data=subset(white.data, year==2020))

pred.feeling(a_4,1)
pred.feeling(f_4,1)
pred.feeling(g_4,1)

auth.feelingP<-rbind(
  pred.feeling(a_1,1),
  pred.feeling(b_1, 1),
  pred.feeling(c_1, 1),
  pred.feeling(d_1, 1),
  pred.feeling(e_1, 1),
  pred.feeling(f_1, 1),
  pred.feeling(g_1, 1),
  pred.feeling(a_3,1),
  pred.feeling(b_3, 1),
  pred.feeling(c_3, 1),
  pred.feeling(d_3, 1),
  pred.feeling(e_3, 1),
  pred.feeling(f_3, 1),
  pred.feeling(g_3, 1)
  
)
Nauth.feelingP<-rbind(
  pred.feeling(a_1,0),
  pred.feeling(b_1, 0),
  pred.feeling(c_1, 0),
  pred.feeling(d_1, 0),
  pred.feeling(e_1, 0),
  pred.feeling(f_1, 0),
  pred.feeling(g_1, 0),
  pred.feeling(a_3, 0),
  pred.feeling(b_3, 0),
  pred.feeling(c_3, 0),
  pred.feeling(d_3, 0),
  pred.feeling(e_3, 0),
  pred.feeling(f_3, 0),
  pred.feeling(g_3, 0)
  
)

auth.feelingC<-rbind(
  pred.feeling(a_2,1),
  pred.feeling(b_2, 1),
  pred.feeling(c_2, 1),
  pred.feeling(d_2, 1),
  pred.feeling(e_2, 1),
  pred.feeling(f_2, 1),
  pred.feeling(g_2, 1),
  pred.feeling(a_4,1),
  pred.feeling(b_4, 1),
  pred.feeling(c_4, 1),
  pred.feeling(d_4, 1),
  pred.feeling(e_4, 1),
  pred.feeling(f_4, 1),
  pred.feeling(g_4, 1)
  
)
Nauth.feelingC<-rbind(
  pred.feeling(a_2,0),
  pred.feeling(b_2, 0),
  pred.feeling(c_2, 0),
  pred.feeling(d_2, 0),
  pred.feeling(e_2, 0),
  pred.feeling(f_2, 0),
  pred.feeling(g_2, 0),
  pred.feeling(a_2, 0),
  pred.feeling(b_4, 0),
  pred.feeling(c_4, 0),
  pred.feeling(d_4, 0),
  pred.feeling(e_4, 0),
  pred.feeling(f_4, 0),
  pred.feeling(g_4, 0)
)
plot.label<-function(plot.feeling){
  names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean")
  plot.feeling$year<-rep(c("1992","2000", "2004", "2008", "2012", "2016", "2020"), times=2)
  plot.feeling$Party<-rep(c("Republican", "Democrat"), each=7)
  return(plot.feeling)
}
feelingP<-rbind(plot.label(auth.feelingP),
                plot.label(Nauth.feelingP))
feelingP$Group<-rep(c("Authoritarian", "Non-Authoritarian"), each=14)


feelingC<-rbind(plot.label(auth.feelingC),
                plot.label(Nauth.feelingC))
feelingC$Group<-rep(c("Authoritarian", "Non-Authoritarian"), each=14)

plot1<-ggplot(data=feelingP) + 
  geom_bar(aes(x=year, y=mean, fill=Party, group=Party), stat="identity", position=position_dodge())+
  scale_fill_manual(name="Party", values=c("gray", "lightgray"))+
  geom_errorbar(aes(x=year, ymin=min1, ymax=max1, group=Party),  width=.2,                    # Width of the error bars
                position=position_dodge(0.9), alpha = 0.6) +
  facet_wrap(~Group)+
  ggtitle("Feelings towards the parties") +
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5))+
  scale_y_continuous("Predicted Score", limits=c(-0.1,1))

plot1
dev.copy(png,'ch5_3.jpg',
         width = 750, height = 500)
dev.off()


plot1<-ggplot(data=feelingC) + 
  geom_bar(aes(x=year, y=mean, fill=Party, group=Party), stat="identity", position=position_dodge())+
  scale_fill_manual(name="Party", values=c("gray", "lightgray"))+
  geom_errorbar(aes(x=year, ymin=min1, ymax=max1, group=Party),  width=.2,                    # Width of the error bars
                position=position_dodge(0.9), alpha = 0.5) +
  facet_wrap(~Group)+
  ggtitle("Feelings towards the candidates") +
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5))+
  scale_y_continuous("Predicted Score", limits=c(-0.1,1))
plot1
dev.copy(png,'ch5_4.jpg',
         width = 750, height = 500)
dev.off()



plot.feeling<-rbind(
  pred.feeling.noint(a_1),
  mean(c(as.numeric(pred.feeling.noint(a_1)[5]), as.numeric(pred.feeling.noint(b_1)[5]))),
  pred.feeling.noint(b_1),
  pred.feeling.noint(c_1),
  pred.feeling.noint(d_1),
  pred.feeling.noint(e_1),
  pred.feeling.noint(f_1),
  pred.feeling.noint(g_1)
)
plot.feeling$year<-rep(c("1992","1996", "2000", "2004", "2008", "2012", "2016", "2020"))
names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean", "Year")
temp1<-plot.feeling
temp1$Question<-"Feelings about the Republican Party"

plot.feeling<-rbind(
  pred.feeling.noint(a_2),
  mean(c(as.numeric(pred.feeling.noint(a_2)[5]), as.numeric(pred.feeling.noint(b_2)[5]))),
  pred.feeling.noint(b_2),
  pred.feeling.noint(c_2),
  pred.feeling.noint(d_2),
  pred.feeling.noint(e_2),
  pred.feeling.noint(f_2),
  pred.feeling.noint(g_2)
  
)
plot.feeling$year<-rep(c("1992","1996", "2000", "2004", "2008", "2012", "2016", "2020"))
names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean", "Year")
temp2<-plot.feeling
temp2$Question<-"Feelings about the Republican Candidate"

plot.feeling<-rbind(
  pred.feeling.noint(a_3),
  mean(c(as.numeric(pred.feeling.noint(a_3)[5]), as.numeric(pred.feeling.noint(b_3)[5]))),
  pred.feeling.noint(b_3),
  pred.feeling.noint(c_3),
  pred.feeling.noint(d_3),
  pred.feeling.noint(e_3),
  pred.feeling.noint(f_3),
  pred.feeling.noint(g_3)
)
plot.feeling$year<-rep(c("1992","1996", "2000", "2004", "2008", "2012", "2016", "2020"))
names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean", "Year")
temp3<-plot.feeling
temp3$Question<-"Feelings about the Democratic Party"

plot.feeling<-rbind(
  pred.feeling.noint(a_4),
  mean(c(as.numeric(pred.feeling.noint(a_4)[5]), as.numeric(pred.feeling.noint(b_4)[5]))),
  pred.feeling.noint(b_4),
  pred.feeling.noint(c_4),
  pred.feeling.noint(d_4),
  pred.feeling.noint(e_4),
  pred.feeling.noint(f_4),
  pred.feeling.noint(g_4)
)
plot.feeling$year<-rep(c("1992","1996", "2000", "2004", "2008", "2012", "2016", "2020"))
names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean", "Year")
temp4<-plot.feeling
temp4$Question<-"Feelings about the Democratic candidate"

plot.data<-rbind(temp1, temp2, temp3, temp4)

plot1<-ggplot(data = plot.data,
              aes(x = factor(Year), 
                  y = mean, ymin=min1, 
                  ymax=max1))+
  facet_wrap(~Question)+
  geom_point(position = position_dodge(width = 0.2), size =0.7) +
  geom_line(data = plot.data,
            aes(x = Year, 
                y = mean, group=1))+
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
  geom_errorbar(aes(ymin=min2, 
                    ymax=max2),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Authoritarianism and Party Affect. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-1,1))+
  scale_x_discrete("Year")+    
  geom_hline(yintercept=0, linetype="dashed" )
plot1

dev.copy(png,'ch5_5.jpg',
         width = 750, height = 500)
dev.off()



### Show how authoritarianism maps onto PID
require(nnet)
### (2) Alignment with authoritarianism and PID
#a. Multinomial Logit. Present Figure
tt<-as.formula(party3~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
require(nnet)
a<-multinom(tt, data=subset(white.data, year==1992))
b<-multinom(tt, data=subset(white.data, year==2000))
c<-multinom(tt, data=subset(white.data, year==2004))
d<-multinom(tt, data=subset(white.data, year==2008))
e<-multinom(tt, data=subset(white.data, year==2012))
f<-multinom(tt, data=subset(white.data, year==2016))
g<-multinom(tt, data=subset(white.data, year==2020))

###
plot.pid<-rbind(
  cbind(pred.pid.noint(a), year=1992),
  cbind(pred.pid.noint(b), year=2000),
  cbind(pred.pid.noint(c), year=2004),
  cbind(pred.pid.noint(d), year=2008),
  cbind(pred.pid.noint(e), year=2012),
  cbind(pred.pid.noint(f), year=2016),
  cbind(pred.pid.noint(g), year=2020)
  
)

detach("package:car")
detach("package:foreign")
library(tidyverse)
library(readr)
library(ggridges)
library(ggplot2)


rep.plot<-data.frame(Probability=c(plot.pid$rep.h, plot.pid$rep.l), Year=as.integer(c(plot.pid$year, plot.pid$year)), 
                     Authoritarianism=as.character(rep(c("High", "Low"), each=length(plot.pid$rep.h))))

rep.plot$Year<-factor(rep.plot$Year, levels=c("2020", "2016", "2012", "2008", "2004", "2000",  "1992"))

dem.plot<-data.frame(Probability=c(plot.pid$dem.h, plot.pid$dem.l), Year=as.integer(c(plot.pid$year, plot.pid$year)), 
                     Authoritarianism=as.character(rep(c("High", "Low"), each=length(plot.pid$rep.h))))

dem.plot$Year<-factor(dem.plot$Year, levels=c("2020", "2016", "2012", "2008", "2004", "2000",  "1992"))

ind.plot<-data.frame(Probability=c(plot.pid$ind.h, plot.pid$ind.l), Year=as.integer(c(plot.pid$year, plot.pid$year)), 
                     Authoritarianism=as.character(rep(c("High", "Low"), each=length(plot.pid$rep.h))))

ind.plot$Year<-factor(dem.plot$Year, levels=c("2020", "2016", "2012", "2008", "2004", "2000",  "1992"))

total.plot<-rbind(
  cbind(dem.plot, Group="Democrat"),
  cbind(ind.plot, Group="Independent"),
  cbind(rep.plot, Group="Republican")
)

librar(dplyr)
total.plot %>% group_by(Group, Authoritarianism) %>%
  subset(Year == 1992) %>% summarize(mean(Probability))
total.plot %>% group_by(Authoritarianism) %>%
  subset(Year == 2020) %>% summarize(mean(Probability))

plot1<-ggplot(total.plot, aes(x=Probability,
                              y=as.factor(Year),
                              fill=Authoritarianism))+
  facet_wrap(~Group, ncol=3)+
  stat_density_ridges(geom="density_ridges", alpha=0.65, 
                      quantile_lines=TRUE, quantiles = 2)+
  xlab('Value') +
  theme_joy() +
  theme(axis.title.y = element_blank())+
  scale_fill_manual(name = "Authoritarianism", 
                    values = c("#D3D3D3", "#686868"))+
  ylab("Year") +
  xlab("Simulated Distribution")+
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.65, hjust=0.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5, hjust=0.5))+
  ggtitle("Party Identification and Authoritarianism. White Respondents") 
plot1     

dev.copy(png,'ch5_6.jpg',
         width = 750, height = 500)
dev.off()


### Effect types ###
library(dplyr)

white.data$repXauth = white.data$republican  * white.data$authoritarianism
white.data$indXauth = white.data$independent * white.data$authoritarianism


temp_dat = white.data
temp_dat$republican = ifelse(temp_dat$pid > 0.68, 1, 0)
temp_dat$democrat = ifelse(temp_dat$pid < 0.33, 1, 0)
temp_dat$independent = ifelse(temp_dat$pid <0.68 & temp_dat$pid > 0.33, 1, 0)

temp_dat$pid3 = ifelse(temp_dat$republican ==  1, 3, NA)
temp_dat$pid3 = ifelse(temp_dat$democrat   ==  1, 1, temp_dat$pid3)
temp_dat$pid3 = ifelse(temp_dat$independent == 1, 2, temp_dat$pid3)


mediator.probability = function(output, treatment = 0 ){
  design = data.frame(model.matrix(output))
  if(treatment == 0){
    design$authoritarianism = 0
  }
  
  if(treatment == 1){
    design$authoritarianism = 1
  }
  
  k<-max(as.numeric(as.factor(output$lev)))-1
  j<-length(coef(output))/2
  
  
  beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
  
  c2<-as.matrix(design)%*%t(beta.sim[,1:j])
  
  c3<-as.matrix(design)%*%t(beta.sim[,(j+1):(j*2)])
  
  p.c1<-1/(1+exp(c2)+exp(c3))
  p.c2<-exp(c2)/(1+exp(c2)+exp(c3))
  p.c3<-exp(c3)/(1+exp(c2)+exp(c3))
  
  g1<-apply(p.c1, 2, mean) #De
  g2<-apply(p.c2, 2, mean) #Ind
  g3<-apply(p.c3, 2, mean) #Rep
  return(c(g1, g2, g3)) 
}

outcome.probability = function(output, treatment = 0, pid = 1 ){
  design = data.frame(model.matrix(output))
  if(treatment == 0 & pid == 3){
    design$authoritarianism = 0
    design$repXauth = 0
    design$republican = 1
    design$indXauth = 0
    design$independent      = 0
  }
  if(treatment == 1 & pid == 3){
    design$authoritarianism = 1
    design$repXauth = 1
    design$republican = 1
    design$indXauth = 0
    design$independent      = 0
  }
  
  if(treatment == 0 & pid == 2){
    design$authoritarianism = 0
    design$repXauth = 0
    design$republican = 0
    design$indXauth = 0
    design$independent      = 1
  }
  if(treatment == 1 & pid == 2){
    design$authoritarianism = 1
    design$repXauth = 0
    design$republican = 0
    design$indXauth = 1
    design$independent      = 1
  }
  
  if(treatment == 0 & pid == 1){
    design$authoritarianism = 0
    design$repXauth = 0
    design$republican = 0
    design$indXauth = 0
    design$independent      = 0
  }
  if(treatment == 1 & pid == 1){
    design$authoritarianism = 1
    design$repXauth = 0
    design$republican = 0
    design$indXauth = 0
    design$independent      = 0
  }
  
  
  beta.sim<-mvrnorm(1000, coef(output), vcov(output))
  c1<-plogis(as.matrix(design)%*%t(beta.sim))
  g1<-apply(c1, 2, mean) #De
  return(g1) 
}

### This draws on these:
effects_calculator = function(dat ){
  tt<-as.formula(pid3~
                   authoritarianism+
                   female+age+college+income+
                   jewish+catholic+other  
  )
  output.m = nnet::multinom(formula = tt, data = dat)
  
  med_data = data.frame(
    prob = c(mediator.probability(output.m, treatment = 0),
             mediator.probability(output.m, treatment = 1)),
    party = rep(c("Democrat", "Independent", "Republican"), 
                each = 1000),
    authoritarianism = rep(c(0,1), 
                           each = 3000)
  )
  tt<-as.formula(vote~
                   authoritarianism + republican + independent + 
                   repXauth + indXauth + 
                   female+age+college+income+
                   jewish+catholic+other  
  )
  output = glm(tt, data = dat, family = binomial("logit"))
  outcome_data = 
    rbind(  
      data.frame(
        prob = c(outcome.probability(output, treatment = 0, pid = 1),
                 outcome.probability(output, treatment = 1, pid = 1)),
        party =  "Democrat",
        authoritarianism = rep(c(0,1), 
                               each = 1000)),
      data.frame(
        prob = c(outcome.probability(output, treatment = 0, pid = 2),
                 outcome.probability(output, treatment = 1, pid = 2)),
        party =  "Independendent",
        authoritarianism = rep(c(0,1), 
                               each = 1000)),
      data.frame(
        prob = c(outcome.probability(output, treatment = 0, pid = 3),
                 outcome.probability(output, treatment = 1, pid = 3)),
        party =  "Republican",
        authoritarianism = rep(c(0,1), 
                               each = 1000))
    )
  return(data.frame(nid_rep = outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                  outcome_data$party =="Republican" ]* 
                      (med_data$prob[med_data$authoritarianism ==1 & 
                                       med_data$party =="Republican" ]  - 
                         med_data$prob[med_data$authoritarianism ==0 & 
                                         med_data$party =="Republican" ] ),
                    nid_ind = outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                  outcome_data$party =="Independendent" ]*
                      abs(med_data$prob[med_data$authoritarianism ==1 & med_data$party =="Independent" ] -
                            med_data$prob[med_data$authoritarianism ==0 & med_data$party =="Independent" ]),
                    nid_dem = outcome_data$prob[outcome_data$authoritarianism ==1 & outcome_data$party =="Democrat" ]* 
                      abs(med_data$prob[med_data$authoritarianism ==1 &  
                                          med_data$party =="Democrat" ] -med_data$prob[med_data$authoritarianism ==0]),
                    
                    nde_rep = (outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                   outcome_data$party =="Republican" ] - 
                                 outcome_data$prob[outcome_data$authoritarianism ==0 & 
                                                     outcome_data$party =="Republican" ])*
                      med_data$prob[med_data$authoritarianism ==0 &  
                                      med_data$party =="Republican" ],
                    nde_ind = (outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                   outcome_data$party =="Independendent" ] - 
                                 outcome_data$prob[outcome_data$authoritarianism ==0 & 
                                                     outcome_data$party =="Independendent" ])*
                      med_data$prob[med_data$authoritarianism ==0 &  
                                      med_data$party =="Independent" ],
                    nde_dem = (outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                   outcome_data$party =="Democrat" ] - 
                                 outcome_data$prob[outcome_data$authoritarianism ==0 & 
                                                     outcome_data$party =="Democrat" ])*
                      med_data$prob[med_data$authoritarianism ==0 &  
                                      med_data$party =="Democrat" ]
                    
  ))
}

dat = effects_calculator(subset(temp_dat, year ==1992))

for(i in c(2000, 2004, 2008, 2012, 2016, 2020)){
  dat = rbind(dat, effects_calculator(subset(temp_dat, year ==i)))
}
dat$year = rep(c(1992, 2000, 2004, 2008, 2012, 2016, 2020), each = 3000)

head(dat)
### This is pretty cumbersome. All the "effects" are stored by year in a 
### data frame called "dat." THe stuff below just pulls out the quantiles to
### calculate mean, min, max for the figure.  
plot_dat = dat %>% group_by(year) %>%
  mutate(mind = quantile(nid_dem, 0.025))%>%
  mutate(meand = quantile(nid_dem, 0.5))%>%
  mutate(maxd = quantile(nid_dem, 0.975))%>%
  mutate(mini = quantile(nid_ind, 0.025))%>%
  mutate(meani = quantile(nid_ind, 0.5))%>%
  mutate(maxi = quantile(nid_ind, 0.975))%>%
  mutate(minr = quantile(nid_rep, 0.025))%>%
  mutate(meanr = quantile(nid_rep, 0.5))%>%
  mutate(maxr = quantile(nid_rep, 0.975))  %>%
  mutate(minde = quantile(nde_dem, 0.025))%>%
  mutate(meande = quantile(nde_dem, 0.5))%>%
  mutate(maxde = quantile(nde_dem, 0.975))%>%
  mutate(minie = quantile(nde_ind, 0.025))%>%
  mutate(meanie = quantile(nde_ind, 0.5))%>%
  mutate(maxie = quantile(nde_ind, 0.975))%>%
  mutate(minre = quantile(nde_rep, 0.025))%>%
  mutate(meanre = quantile(nde_rep, 0.5))%>%
  mutate(maxre = quantile(nde_rep, 0.975)) 

head(plot_dat)

plot_dat = plot_dat[,4:ncol(plot_dat)] # only keep relevant stuff

plot_dat = data.frame(min = 
                        c(plot_dat$minr, plot_dat$mini, plot_dat$mind,
                          plot_dat$minre, plot_dat$minie, plot_dat$minde),
                      mean = 
                        c(plot_dat$meanr, plot_dat$meani, plot_dat$meand,
                          plot_dat$meanre, plot_dat$meanie, plot_dat$meande),
                      max = 
                        c(plot_dat$maxr, plot_dat$maxi, plot_dat$maxd,
                          plot_dat$maxre, plot_dat$maxie, plot_dat$maxde),
                      
                      year = c(plot_dat$year, plot_dat$year, plot_dat$year,
                               plot_dat$year, plot_dat$year, plot_dat$year),
                      
                      party = rep(c("Republican", "Independent", "Democrat"), each = nrow(plot_dat)),
                      
                      type = rep(c("Indirect Effect", "Direct Effect"), each = length(plot_dat$mini)*3)
                      
                      
)

plot_dat = plot_dat %>% subset(party!="Independent") %>% 
  group_by(year, type, party) %>% 
  summarize(mean(mean), mean(min), mean(max)) 
names(plot_dat) = c("year", "type", "party", "mean", "min", "max")


plot1<-ggplot(data = plot_dat,
              aes(x = factor(year), 
                  y = mean, ymin=min, 
                  ymax=max, colour=party))+
  facet_wrap(~type, scales = "fixed")+
  geom_point(size=0.8, position = position_dodge(width = 0.3)) +
  geom_errorbar(width = 0.20, alpha=0.5, position = position_dodge(width = 0.3)) +
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  scale_colour_manual(name="Party", values=c("grey", "black"))+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Direct and Indirect Effects, by Party (Voting). White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Indirect, Direct Effect of Authoritarianism")+
  scale_x_discrete("Year") + 
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") 
plot1

dev.copy(png, "ch5_7.png",
         width = 750, height = 500)
dev.off()

###### Estimate effects for candidate feelings ######

### This draws on these:
effects_calculator = function(dat ){
  tt<-as.formula(pid3~
                   authoritarianism+
                   female+age+college+income+
                   jewish+catholic+other  
  )
  output.m = nnet::multinom(formula = tt, data = dat)
  
  med_data = data.frame(
    prob = c(mediator.probability(output.m, treatment = 0),
             mediator.probability(output.m, treatment = 1)),
    party = rep(c("Democrat", "Independent", "Republican"), 
                each = 1000),
    authoritarianism = rep(c(0,1), 
                           each = 3000)
  )
  tt<-as.formula(I((feeling.repc - feeling.demc)+100)/200~
                   authoritarianism + republican + independent + 
                   repXauth + indXauth + 
                   female+age+college+income+
                   jewish+catholic+other  
  )
  output = lm(tt, data = dat)
  outcome_data = 
    rbind(  
      data.frame(
        prob = c(outcome.probability(output, treatment = 0, pid = 1),
                 outcome.probability(output, treatment = 1, pid = 1)),
        party =  "Democrat",
        authoritarianism = rep(c(0,1), 
                               each = 1000)),
      data.frame(
        prob = c(outcome.probability(output, treatment = 0, pid = 2),
                 outcome.probability(output, treatment = 1, pid = 2)),
        party =  "Independendent",
        authoritarianism = rep(c(0,1), 
                               each = 1000)),
      data.frame(
        prob = c(outcome.probability(output, treatment = 0, pid = 3),
                 outcome.probability(output, treatment = 1, pid = 3)),
        party =  "Republican",
        authoritarianism = rep(c(0,1), 
                               each = 1000))
    )
  return(data.frame(nid_rep = outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                  outcome_data$party =="Republican" ]* 
                      (med_data$prob[med_data$authoritarianism ==1 & 
                                       med_data$party =="Republican" ]  - 
                         med_data$prob[med_data$authoritarianism ==0 & 
                                         med_data$party =="Republican" ] ),
                    nid_ind = outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                  outcome_data$party =="Independendent" ]*
                      abs(med_data$prob[med_data$authoritarianism ==1 & med_data$party =="Independent" ] -
                            med_data$prob[med_data$authoritarianism ==0 & med_data$party =="Independent" ]),
                    nid_dem = outcome_data$prob[outcome_data$authoritarianism ==1 & outcome_data$party =="Democrat" ]* 
                      abs(med_data$prob[med_data$authoritarianism ==1 &  
                                          med_data$party =="Democrat" ] -med_data$prob[med_data$authoritarianism ==0]),
                    
                    nde_rep = (outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                   outcome_data$party =="Republican" ] - 
                                 outcome_data$prob[outcome_data$authoritarianism ==0 & 
                                                     outcome_data$party =="Republican" ])*
                      med_data$prob[med_data$authoritarianism ==0 &  
                                      med_data$party =="Republican" ],
                    nde_ind = (outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                   outcome_data$party =="Independendent" ] - 
                                 outcome_data$prob[outcome_data$authoritarianism ==0 & 
                                                     outcome_data$party =="Independendent" ])*
                      med_data$prob[med_data$authoritarianism ==0 &  
                                      med_data$party =="Independent" ],
                    nde_dem = (outcome_data$prob[outcome_data$authoritarianism ==1 & 
                                                   outcome_data$party =="Democrat" ] - 
                                 outcome_data$prob[outcome_data$authoritarianism ==0 & 
                                                     outcome_data$party =="Democrat" ])*
                      med_data$prob[med_data$authoritarianism ==0 &  
                                      med_data$party =="Democrat" ]
                    
  ))
}

dat = effects_calculator(subset(temp_dat, year ==1992))

for(i in c(2000, 2004, 2008, 2012, 2016, 2020)){
  dat = rbind(dat, effects_calculator(subset(temp_dat, year ==i)))
}
dat$year = rep(c(1992, 2000, 2004, 2008, 2012, 2016, 2020), each = 3000)

head(dat)
### This is pretty cumbersome. All the "effects" are stored by year in a 
### data frame called "dat." THe stuff below just pulls out the quantiles to
### calculate mean, min, max for the figure.  
plot_dat = dat %>% group_by(year) %>%
  mutate(mind = quantile(nid_dem, 0.025))%>%
  mutate(meand = quantile(nid_dem, 0.5))%>%
  mutate(maxd = quantile(nid_dem, 0.975))%>%
  mutate(mini = quantile(nid_ind, 0.025))%>%
  mutate(meani = quantile(nid_ind, 0.5))%>%
  mutate(maxi = quantile(nid_ind, 0.975))%>%
  mutate(minr = quantile(nid_rep, 0.025))%>%
  mutate(meanr = quantile(nid_rep, 0.5))%>%
  mutate(maxr = quantile(nid_rep, 0.975))  %>%
  mutate(minde = quantile(nde_dem, 0.025))%>%
  mutate(meande = quantile(nde_dem, 0.5))%>%
  mutate(maxde = quantile(nde_dem, 0.975))%>%
  mutate(minie = quantile(nde_ind, 0.025))%>%
  mutate(meanie = quantile(nde_ind, 0.5))%>%
  mutate(maxie = quantile(nde_ind, 0.975))%>%
  mutate(minre = quantile(nde_rep, 0.025))%>%
  mutate(meanre = quantile(nde_rep, 0.5))%>%
  mutate(maxre = quantile(nde_rep, 0.975)) 

head(plot_dat)

plot_dat = plot_dat[,4:ncol(plot_dat)] # only keep relevant stuff

plot_dat = data.frame(min = 
                        c(plot_dat$minr, plot_dat$mini, plot_dat$mind,
                          plot_dat$minre, plot_dat$minie, plot_dat$minde),
                      mean = 
                        c(plot_dat$meanr, plot_dat$meani, plot_dat$meand,
                          plot_dat$meanre, plot_dat$meanie, plot_dat$meande),
                      max = 
                        c(plot_dat$maxr, plot_dat$maxi, plot_dat$maxd,
                          plot_dat$maxre, plot_dat$maxie, plot_dat$maxde),
                      
                      year = c(plot_dat$year, plot_dat$year, plot_dat$year,
                               plot_dat$year, plot_dat$year, plot_dat$year),
                      
                      party = rep(c("Republican", "Independent", "Democrat"), each = nrow(plot_dat)),
                      
                      type = rep(c("Indirect Effect", "Direct Effect"), each = length(plot_dat$mini)*3)
                      
                      
)

plot_dat = plot_dat %>% subset(party!="Independent") %>% 
  group_by(year, type, party) %>% 
  summarize(mean(mean), mean(min), mean(max)) 
names(plot_dat) = c("year", "type", "party", "mean", "min", "max")


plot1<-ggplot(data = plot_dat,
              aes(x = factor(year), 
                  y = mean, ymin=min, 
                  ymax=max, colour=party))+
  facet_wrap(~type, scales = "fixed")+
  geom_point(size=0.8, position = position_dodge(width = 0.3)) +
  geom_errorbar(width = 0.20, alpha=0.5, position = position_dodge(width = 0.3)) +
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  scale_colour_manual(name="Party", values=c("grey", "black"))+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Direct and Indirect Effects, by Party (Candidate Affect). White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Indirect, Direct Effect of Authoritarianism")+
  scale_x_discrete("Year") + 
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") 
plot1

dev.copy(png, "ch5_8.png",
         width = 750, height = 500)
dev.off()


#### Robustness check here, a ala mediation #####

#### Part III: Education Analysis 
###### Vote, Moderated by Education ######
white.data$collegeXauthoritarianism<-white.data$college*white.data$authoritarianism
tt<-as.formula(vote~
                 authoritarianism+college+
                 collegeXauthoritarianism+
                 female+age+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))
g<-glm(tt, data=subset(white.data, year==2020), family=binomial("logit"))

marginal.ed<-function(a){
  temp<-rbind(
    model.prediction(a, 
                     design.matrix.int.marginal(a, 
                                                iv.name="authoritarianism", mod.name="college", int.name="collegeXauthoritarianism", 
                                                value.moderator=1, FALSE), 
                     "Marginal", "Binary.Logit"),
    model.prediction(a, 
                     design.matrix.int.marginal(a, 
                                                iv.name="authoritarianism", mod.name="college", int.name="collegeXauthoritarianism", 
                                                value.moderator=0, FALSE), 
                     "Marginal", "Binary.Logit")
  )
  return(temp)
}

plot.data<-rbind(marginal.ed(a), marginal.ed(b), marginal.ed(c),
                 marginal.ed(d), marginal.ed(e), marginal.ed(f),  marginal.ed(g))
plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016, 
                      2020), each=2)
plot.data$Education<-rep(c("College Degree or Greater", "Less than College Degree"))

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="Less than College Degree"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="Less than College Degree"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"Less than College Degree"

plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="College Degree or Greater"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="College Degree or Greater"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"College Degree or Greater"


plot1<-ggplot(data = plot.data,
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5))+
  facet_wrap(~Education)+
  geom_point(position = position_dodge(width = .01)) +
  geom_line(group = 1)+
  geom_errorbar(width = 0.10, alpha=0.5) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75),
                width = 0.01, size=0.5, alpha=0.7)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  scale_colour_manual(name="Positive Feeling towards", values=c("darkgrey", "black"))+
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Authoritarianism, Education, and Voting") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("M.E. Authoritarianism", limits=c(-.1,1))+
  scale_x_discrete("Year")+
  geom_hline(aes(yintercept=0), linetype="dashed")

p1 = plot1 

### FEELINGS, C()
tt<-as.formula(I(feeling.dem/100)~
                 authoritarianism+college+
                 collegeXauthoritarianism+
                 female+age+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992))
b<-glm(tt, data=subset(white.data, year==2000))
c<-glm(tt, data=subset(white.data, year==2004))
d<-glm(tt, data=subset(white.data, year==2008))
e<-glm(tt, data=subset(white.data, year==2012))
f<-glm(tt, data=subset(white.data, year==2016))
g<-glm(tt, data=subset(white.data, year==2020))

marginal.ed<-function(a){
  temp<-rbind(
    model.prediction(a, 
                     design.matrix.int.marginal(a, 
                                                iv.name="authoritarianism", mod.name="college", int.name="collegeXauthoritarianism", 
                                                value.moderator=1, FALSE), 
                     "Marginal", "OLS"),
    model.prediction(a, 
                     design.matrix.int.marginal(a, 
                                                iv.name="authoritarianism", mod.name="college", int.name="collegeXauthoritarianism", 
                                                value.moderator=0, FALSE), 
                     "Marginal", "OLS")
  )
  return(temp)
}

plot.data<-rbind(marginal.ed(a), marginal.ed(b), marginal.ed(c),
                 marginal.ed(d), marginal.ed(e), marginal.ed(f),  marginal.ed(g))
plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016, 
                      2020), each=2)
plot.data$Education<-rep(c("College Degree or Greater", "Less than College Degree"))

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="Less than College Degree"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="Less than College Degree"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"Less than College Degree"

plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="College Degree or Greater"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="College Degree or Greater"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"College Degree or Greater"

p2 = plot.data

tt<-as.formula(I(feeling.rep/100)~
                 authoritarianism+college+
                 collegeXauthoritarianism+
                 female+age+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992))
b<-glm(tt, data=subset(white.data, year==2000))
c<-glm(tt, data=subset(white.data, year==2004))
d<-glm(tt, data=subset(white.data, year==2008))
e<-glm(tt, data=subset(white.data, year==2012))
f<-glm(tt, data=subset(white.data, year==2016))
g<-glm(tt, data=subset(white.data, year==2020))

plot.data<-rbind(marginal.ed(a), marginal.ed(b), marginal.ed(c),
                 marginal.ed(d), marginal.ed(e), marginal.ed(f),  marginal.ed(g))
plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016, 
                      2020), each=2)
plot.data$Education<-rep(c("College Degree or Greater", "Less than College Degree"))

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="Less than College Degree"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="Less than College Degree"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"Less than College Degree"

plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="College Degree or Greater"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="College Degree or Greater"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"College Degree or Greater"

p3 = plot.data

combined_trait_plot = data.frame(rbind(p2, p3))
combined_trait_plot$affect = rep(c("Democats", "Republicans"), each = nrow(p2))


plot1<-ggplot(data = combined_trait_plot,
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5, group = affect,
                  colour = affect))+
  facet_wrap(~Education)+
  geom_point(position = position_dodge(width = .01)) +
  geom_line()+
  geom_errorbar(width = 0.10, alpha=0.5) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75, colour=affect),
                width = 0.01, size=0.5, alpha=0.7)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  scale_colour_manual(name="Positive Feeling towards", values=c("darkgrey", "black"))+
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Authoritarianism, Education, and Party Affect") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("M.E. Authoritarianism", limits=c(-1,1))+
  scale_x_discrete("Year")+
  geom_hline(aes(yintercept=0), linetype="dashed")

p2 = plot1


### Feelings candidates


tt<-as.formula(I(feeling.demc/100)~
                 authoritarianism+college+
                 collegeXauthoritarianism+
                 female+age+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992))
b<-glm(tt, data=subset(white.data, year==2000))
c<-glm(tt, data=subset(white.data, year==2004))
d<-glm(tt, data=subset(white.data, year==2008))
e<-glm(tt, data=subset(white.data, year==2012))
f<-glm(tt, data=subset(white.data, year==2016))
g<-glm(tt, data=subset(white.data, year==2020))

marginal.ed<-function(a){
  temp<-rbind(
    model.prediction(a, 
                     design.matrix.int.marginal(a, 
                                                iv.name="authoritarianism", mod.name="college", int.name="collegeXauthoritarianism", 
                                                value.moderator=1, FALSE), 
                     "Marginal", "OLS"),
    model.prediction(a, 
                     design.matrix.int.marginal(a, 
                                                iv.name="authoritarianism", mod.name="college", int.name="collegeXauthoritarianism", 
                                                value.moderator=0, FALSE), 
                     "Marginal", "OLS")
  )
  return(temp)
}

plot.data<-rbind(marginal.ed(a), marginal.ed(b), marginal.ed(c),
                 marginal.ed(d), marginal.ed(e), marginal.ed(f),  marginal.ed(g))
plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016, 
                      2020), each=2)
plot.data$Education<-rep(c("College Degree or Greater", "Less than College Degree"))

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="Less than College Degree"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="Less than College Degree"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"Less than College Degree"

plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="College Degree or Greater"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="College Degree or Greater"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"College Degree or Greater"

tmp1 = plot.data

tt<-as.formula(I(feeling.repc/100)~
                 authoritarianism+college+
                 collegeXauthoritarianism+
                 female+age+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992))
b<-glm(tt, data=subset(white.data, year==2000))
c<-glm(tt, data=subset(white.data, year==2004))
d<-glm(tt, data=subset(white.data, year==2008))
e<-glm(tt, data=subset(white.data, year==2012))
f<-glm(tt, data=subset(white.data, year==2016))
g<-glm(tt, data=subset(white.data, year==2020))

plot.data<-rbind(marginal.ed(a), marginal.ed(b), marginal.ed(c),
                 marginal.ed(d), marginal.ed(e), marginal.ed(f),  marginal.ed(g))
plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016, 
                      2020), each=2)
plot.data$Education<-rep(c("College Degree or Greater", "Less than College Degree"))

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="Less than College Degree"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="Less than College Degree"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"Less than College Degree"

plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="College Degree or Greater"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="College Degree or Greater"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"College Degree or Greater"

tmp2 = plot.data

combined_trait_plot = data.frame(rbind(tmp1, tmp2))
combined_trait_plot$affect = rep(c("Democats", "Republicans"), each = nrow(tmp2))


plot1<-ggplot(data = combined_trait_plot,
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5, group = affect,
                  colour = affect))+
  facet_wrap(~Education)+
  geom_point(position = position_dodge(width = .01)) +
  geom_line()+
  geom_errorbar(width = 0.10, alpha=0.5) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75, colour=affect),
                width = 0.01, size=0.5, alpha=0.7)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  scale_colour_manual(name="Positive Feeling towards", values=c("darkgrey", "black"))+
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Authoritarianism, Education, and Candidate Affect") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("M.E. Authoritarianism", limits=c(-1,1))+
  scale_x_discrete("Year")+
  geom_hline(aes(yintercept=0), linetype="dashed")

p3 = plot1

p1
dev.copy(png, "ch5_9.png",
         width = 750, height = 500)
dev.off()

p2
dev.copy(png, "ch5_10.png",
         width = 750, height = 500)
dev.off()

p3

dev.copy(png, "ch5_11.png",
         width = 750, height = 500)
dev.off()

### PID, education effects #######

tt<-as.formula(party3~
                 authoritarianism+ college + 
                 collegeXauthoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)


require(nnet)
a<-multinom(tt, data=subset(white.data, year==1992))
b<-multinom(tt, data=subset(white.data, year==2000))
c<-multinom(tt, data=subset(white.data, year==2004))
d<-multinom(tt, data=subset(white.data, year==2008))
e<-multinom(tt, data=subset(white.data, year==2012))
f<-multinom(tt, data=subset(white.data, year==2016))
g<-multinom(tt, data=subset(white.data, year==2020))

##### Moderation Effects ####

college_pid_interaction<-function(output = g, college = 1){
        tmp_dat1 = data.frame(model.matrix(output))
        tmp_dat2 = data.frame(model.matrix(output))

        tmp_dat1$authoritarianism = 1
        tmp_dat1$college = college
        tmp_dat1$collegeXauthoritarianism = college* 1
          set.seed(27)
          k<-max(as.numeric(as.factor(output$lev)))-1
          j<-length(coef(output))/2
          beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
          c2a<-as.matrix(tmp_dat1)%*%t(beta.sim[,1:j])
          c3a<-as.matrix(tmp_dat1)%*%t(beta.sim[,(j+1):(j*2)])
          ######
          tmp_dat1$authoritarianism = 0
          tmp_dat1$college = college
          tmp_dat1$collegeXauthoritarianism = college* 0
          c2b<-as.matrix(tmp_dat2)%*%t(beta.sim[,1:j])
          c3b<-as.matrix(tmp_dat2)%*%t(beta.sim[,(j+1):(j*2)])
          
          p.c1a<-1/(1+exp(c2a)+exp(c3a))
          p.c2a<-exp(c2a)/(1+exp(c2a)+exp(c3a))
          p.c3a<-exp(c3a)/(1+exp(c2a)+exp(c3a))
          
          p.c1b<-1/(1+exp(c2b)+exp(c3b))
          p.c2b<-exp(c2b)/(1+exp(c2b)+exp(c3b))
          p.c3b<-exp(c3b)/(1+exp(c2b)+exp(c3b))
          
         democrat =     apply(p.c1a, 2, mean) - apply(p.c1b, 2, mean) # Marginal dem
         independent =  apply(p.c2a, 2, mean) - apply(p.c2b, 2, mean) # Marginal independent
         republican  =  apply(p.c3a, 2, mean) - apply(p.c3b, 2, mean) # Marginal republican
         return(cbind(democrat, independent, republican))
         
}

plot_dat = data.frame(rbind(
  college_pid_interaction(a, college = 0),
  college_pid_interaction(b, college = 0),
  college_pid_interaction(c, college = 0),
  college_pid_interaction(d, college = 0),
  college_pid_interaction(e, college = 0),
  college_pid_interaction(f, college = 0),
  college_pid_interaction(g, college = 0)
) )
#plot_dat$college = rep(c(0,1), each = nrow(college_pid_interaction(a, college = 0)))
plot_dat$year = rep(c(1992, 2000, 2004, 2008, 2012, 2016, 2020),
                    each = 1* nrow(college_pid_interaction(a, college = 0)))
plot_dat$id = seq(1:nrow(plot_dat))
plot_dat = plot_dat %>% reshape2::melt(id.vars = c("id", "year"),
                            measure.vars = c("democrat", "independent", "republican"),
                            variable.name = "party",
                            value.name = "probability") 

pid_plot1 = data.frame(
  reshape2::melt(t(tapply(plot_dat$probability, list(plot_dat$party, plot_dat$year), quantile, 0.5))),
  reshape2::melt(t(tapply(plot_dat$probability, list(plot_dat$party, plot_dat$year), quantile, 0.975)))[,3],
  reshape2::melt(t(tapply(plot_dat$probability, list(plot_dat$party, plot_dat$year), quantile, 0.025)))[,3]
)
names(pid_plot1) = c("year", "party", "mean", "max", "min")


plot_dat = data.frame(rbind(
  college_pid_interaction(a, college = 1),
  college_pid_interaction(b, college = 1),
  college_pid_interaction(c, college = 1),
  college_pid_interaction(d, college = 1),
  college_pid_interaction(e, college = 1),
  college_pid_interaction(f, college = 1),
  college_pid_interaction(g, college = 1)
) )
#plot_dat$college = rep(c(0,1), each = nrow(college_pid_interaction(a, college = 0)))
plot_dat$year = rep(c(1992, 2000, 2004, 2008, 2012, 2016, 2020),
                    each = 1* nrow(college_pid_interaction(a, college = 1)))
plot_dat$id = seq(1:nrow(plot_dat))
plot_dat = plot_dat %>% reshape2::melt(id.vars = c("id", "year"),
                                       measure.vars = c("democrat", "independent", "republican"),
                                       variable.name = "party",
                                       value.name = "probability") 

pid_plot2 = data.frame(
  reshape2::melt(t(tapply(plot_dat$probability, list(plot_dat$party, plot_dat$year), quantile, 0.5))),
  reshape2::melt(t(tapply(plot_dat$probability, list(plot_dat$party, plot_dat$year), quantile, 0.975)))[,3],
  reshape2::melt(t(tapply(plot_dat$probability, list(plot_dat$party, plot_dat$year), quantile, 0.025)))[,3]
)
names(pid_plot2) = c("year", "party", "mean", "max", "min")

pid_plot = data.frame(rbind(pid_plot1, pid_plot2), college = rep(c("No College Degree", "College Degree"), each = nrow(pid_plot1)))

plot1<-ggplot(data = pid_plot,
              aes(x = factor(year), 
                  y = mean, ymin=min, 
                  ymax=max, group = party, colour = party))+
  facet_wrap(~college, nrow = 3, scales = "free")+
  geom_point(position = position_dodge(width = .01)) +
  geom_line()+
  geom_errorbar(width = 0.10, 
                alpha=0.3) +
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  scale_colour_manual(name="Party", values=c("black", "darkgrey", "lightgrey"))+
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Authoritarianism, Education, and Party Identification") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism")+
  scale_x_discrete("Year")+
  geom_hline(aes(yintercept=0), linetype="dashed")

p4 = plot1

p4
dev.copy(png, "ch5_10.png",
         width = 750, height = 500)
dev.off()

### What drives sorting ####

########### Affective Polarization Analysis ##########
white.data$polarize = zero.one(abs(white.data$dem_traits - white.data$rep_traits ))
white.data$authXpolarize = white.data$authoritarianism  * white.data$polarize
white.data$pid2 = car::recode(white.data$party3, "'Republican'=1; 'Democrat'=0; else = NA")
tt<-as.formula(pid2~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other + polarize + 
                 authXpolarize)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
#e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))
g<-glm(tt, data=subset(white.data, year==2020), family=binomial("logit"))

#g<-glm(tt, data=subset(white.data, year==2020), family=binomial("logit"))

### Create marginal effect estimates of authoritarianism across levels of polarization

marginalXpolarize2 = function(output, polarization = 1){
  design = data.frame(model.matrix(output))
  design$authoritarianism = 0
  design$polarize = polarization
  design$authXpolarize = polarization * 0
  beta.sim<-MASS::mvrnorm(1000, coef(output), vcov(output))
  c1<-plogis(as.matrix(design)%*%t(beta.sim))
  
  design$authoritarianism = 1
  design$polarize = polarization
  design$authXpolarize = polarization * 1
  beta.sim<-MASS::mvrnorm(1000, coef(output), vcov(output))
  c2<-plogis(as.matrix(design)%*%t(beta.sim))
  treatment = c2-c1
  g1<-apply(treatment, 2, mean) #De
  return(g1) 
}
marginal_stack2 = function(model = a){
  library(dplyr)
  library(ggplot2)
  output = c()
  something = seq(0, 1, by = .1)
  for(i in 1:length(something)){
    output = rbind(output, marginalXpolarize2(model, 
                                              polarization = something[i]))}
  output = data.frame(value = c(output))
  output$polarize     =  something # Number of random drawas from beta.sim
  output = output %>% group_by(polarize) %>%
    summarise(min = quantile(value, probs = 0.025),  
              max = quantile(value, probs = 0.975),
              mean = quantile(value, probs = 0.5), .groups = 'drop')  
  return(output)
}




tmp_stack<- 
  data.frame(
    rbind(
      marginal_stack2(model = a),
      marginal_stack2(model = b),
      marginal_stack2(model = c),
      marginal_stack2(model = d),
      marginal_stack2(model = f),
      marginal_stack2(model = g)))

tmp_stack$year <- rep(c(1992, 2000, 2004, 2008, 2016, 2020), each = nrow(tmp_stack)/6)        
ggplot(tmp_stack,
       aes(y = mean, ymin=min, 
           ymax=max, x = polarize)) +
  facet_wrap(~year) + 
  geom_line()+
  geom_ribbon(fill="grey79", alpha=0.5)+
  theme(text=element_text(size=16), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Republican (versus Democratic) Identification") +
  theme(plot.title=element_text(face="bold", hjust=-.08,vjust=2,colour="#3C3C3C",size=16)) +
  theme(axis.text.x=element_text(size=11,colour="#535353", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-1,1))+
  scale_x_continuous("Comparative Trait Negativity (abs[Republican negativity - Democratic negativity])", 
                     limits=c(0,1))+
  geom_hline(aes(yintercept=.0), linetype="dashed", colour="grey") 

dev.copy(png,'ch5_11.jpg',
         width = 750, height = 500)
dev.off()



white.data$polarize = zero.one(abs(white.data$dem.angry - white.data$rep.angry))
white.data$authXpolarize = white.data$authoritarianism  * white.data$polarize
white.data$pid2 = car::recode(white.data$party3, "'Republican'=1; 'Democrat'=0; else = NA")
tt<-as.formula(pid2~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other + polarize + 
                 authXpolarize)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))
#g<-glm(tt, data=subset(white.data, year==2020), family=binomial("logit"))

tmp_stack<- 
  data.frame(
    rbind(
      marginal_stack2(model = a),
      marginal_stack2(model = b),
      marginal_stack2(model = c),
      marginal_stack2(model = d),
      marginal_stack2(model = e),
      marginal_stack2(model = f)))

tmp_stack$year <- rep(c(1992, 2000, 2004, 2008, 2012, 2016), each = nrow(tmp_stack)/6)        
ggplot(tmp_stack,
       aes(y = mean, ymin=min, 
           ymax=max, x = polarize)) +
  facet_wrap(~year) + 
  geom_line()+
  geom_ribbon(fill="grey79", alpha=0.5)+
  theme(text=element_text(size=16), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Republican (versus Democratic) Identification") +
  theme(plot.title=element_text(face="bold", hjust=-.08,vjust=2,colour="#3C3C3C",size=16)) +
  theme(axis.text.x=element_text(size=11,colour="#535353", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-1,1))+
  scale_x_continuous("Comparative Anger (abs[Republican anger - Democratic anger])") +  
  geom_hline(aes(yintercept=.0), linetype="dashed", colour="grey") 

dev.copy(png,'ch5_12.jpg',
         width = 750, height = 500)
dev.off()




white.data$ft.dem = abs(white.data$feeling.dem - 100)
white.data$ft.rep = abs(white.data$feeling.rep - 100)
white.data$polarize = zero.one(abs(white.data$ft.dem - white.data$ft.rep))
white.data$authXpolarize = white.data$authoritarianism  * white.data$polarize
white.data$pid2 = car::recode(white.data$party3, "'Republican'=1; 'Democrat'=0; else = NA")
tt<-as.formula(pid2~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other + polarize + 
                 authXpolarize)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))
g<-glm(tt, data=subset(white.data, year==2020), family=binomial("logit"))

tmp_stack<- 
  data.frame(
    rbind(
      marginal_stack2(model = a),
      marginal_stack2(model = b),
      marginal_stack2(model = c),
      marginal_stack2(model = d),
      marginal_stack2(model = e),
      marginal_stack2(model = f),
      marginal_stack2(model = g)))

tmp_stack$year <- rep(c(1992, 2000, 2004, 2008, 2012, 2016, 2020), each = nrow(tmp_stack)/7)        
ggplot(tmp_stack,
       aes(y = mean, ymin=min, 
           ymax=max, x = polarize)) +
  facet_wrap(~year) + 
  geom_line()+
  geom_ribbon(fill="grey79", alpha=0.5)+
  theme(text=element_text(size=16), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Republican (versus Democratic) Identification") +
  theme(plot.title=element_text(face="bold", hjust=-.08,vjust=2,colour="#3C3C3C",size=16)) +
  theme(axis.text.x=element_text(size=11,colour="#535353", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-1,1))+
  scale_x_continuous("Comparative Affect (abs[Republican Negative - Democratic Negative])") +  
  geom_hline(aes(yintercept=.0), linetype="dashed", colour="grey") 

dev.copy(png,'ch5_13.jpg',
         width = 750, height = 500)
dev.off()






