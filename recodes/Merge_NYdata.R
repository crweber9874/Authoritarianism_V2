#### Recode of New York Data #####
rm(list=ls())
require(foreign)
require(car)
require(mirt)
require(ggplot2)
require(car)
data<-read.dta("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/New York Data/issues_merged_final_2.dta", convert.factors=FALSE)
### Dependent Variables #####
sc.data<-as.data.frame(data[,1])
sc.data$immD[!is.na(data$q05a)]<-1  ## Immigration Culture Threat
sc.data$immD[!is.na(data$q05b)]<-0 
sc.data$immigration<-ifelse(sc.data$immD==1,data$q05a, data$q05b )
sc.data$immigration<-recode(sc.data$immigration, "2=0; 8=NA; 9=NA") ## Conservative Response

sc.data$KKKD[!is.na(data$q06a)]<-1  ## KKK Culture Threat
sc.data$KKKD[!is.na(data$q06b)]<-0 
sc.data$KKK<-ifelse(sc.data$KKKD==1,data$q06a, data$q06b )
sc.data$KKK<-recode(sc.data$KKK, "2=0; 8=NA; 9=NA") ## Conservative Response

sc.data$commD[!is.na(data$q06c)]<-1  ## Communism Culture Threat
sc.data$commD[!is.na(data$q06d)]<-0 
sc.data$comm<-ifelse(sc.data$commD==1,data$q06c, data$q06d )
sc.data$comm<-recode(sc.data$comm, "2=0; 8=NA; 9=NA") ## Conservative Response

sc.data$drugD[!is.na(data$q07a)]<-1  ## Drug Culture Threat
sc.data$drugD[!is.na(data$q07b)]<-0 
sc.data$drug<-ifelse(sc.data$drugD==1,data$q07a, data$q07b )
sc.data$drug<-recode(sc.data$drug, "2=0; 8=NA; 9=NA") ## Conservative Response

sc.data$pornD[!is.na(data$q08a)]<-1  ## Porn Culture Threat
sc.data$pornD[!is.na(data$q08b)]<-0 
sc.data$porn<-ifelse(sc.data$pornD==1,data$q08a, data$q08b )
sc.data$porn<-recode(sc.data$porn, "1=1; 2=2; 3=3; 8=NA; 9=NA") ## Conservative Response

sc.data$prostD[!is.na(data$q09a)]<-1  ## Prostitution Culture Threat
sc.data$prostD[!is.na(data$q09b)]<-0 
sc.data$prost<-ifelse(sc.data$prostD==1,data$q08a, data$q08b )
sc.data$prost<-recode(sc.data$prost, "1=1; 2=2; 3=3; 8=NA; 9=NA") ## Conservative Response

sc.data$prayer<-recode(data$q10, "1=1; 2=2; 3=3; else=NA") #Common Prayer
sc.data$sex.tv<-recode(data$q11, "1=1; 2=2; 3=3; else=NA") #TV censorship

sc.data$gayD[!is.na(data$q12a)]<-1  ## Gay threat; regulation frame
sc.data$gayD[!is.na(data$q12b)]<-0 
sc.data$gay<-ifelse(sc.data$gayD==1,data$q12a, data$q12b )
sc.data$gay<-recode(sc.data$gay, "1=0; 2=1;  8=NA; 9=NA") ## Conservative Response

sc.data$gay2D[!is.na(data$q12c)]<-1  ## Gay threat; regulation frame
sc.data$gay2D[!is.na(data$q12d)]<-0 
sc.data$gay2<-ifelse(sc.data$gay2D==1,data$q12c, data$q12d )
sc.data$gay2<-recode(sc.data$gay2, "1=0; 2=1; 8=NA; 9=NA") ## Conservative Response

#### Feeling Thermometers #####
sc.data$welfare<-recode(data$welfare, "98=NA; 99=NA") 
sc.data$asianam<-recode(data$asianam, "20=NA; 98=NA; 99=NA") 
sc.data$feminia<-recode(data$feminia, "98=NA; 99=NA") 
sc.data$hispano<-recode(data$hispano, "98=NA; 99=NA") 
sc.data$kukluxk<-recode(data$kukluxk, "98=NA; 99=NA") 
sc.data$african<-recode(data$african, "98=NA; 99=NA; 55=NA; 11=NA") 
sc.data$commies<-recode(data$commies, "98=NA; 99=NA; 30=NA") 

#### Moral Questions #####
sc.data$sex.marriage<-recode(data$moralch, "1=4; 2=3; 3=2; 4=1; else=NA") 
sc.data$same.sex<-recode(data$samesex, "1=4; 2=3; 3=2; 4=1; else=NA") 
sc.data$cohabitate<-recode(data$married, "1=4; 2=3; 3=2; 4=1; else=NA") 
sc.data$read.porno<-recode(data$pornogr, "1=4; 2=3; 3=2; 4=1; else=NA") 

#### Social Conformity, Threat, etc. #####
f1<-recode(data$foundat, "1=1; 2=2; 3=3; 4=4; else=NA")
f2<-recode(data$founda2, "1=1; 2=2; 3=3; 4=4; else=NA")
lc1<-recode(data$lcommon, "1=4; 2=3; 3=2; 4=1; else=NA")
lc2<-recode(data$lcommo2, "1=4; 2=3; 3=2; 4=1; else=NA")
c1<-recode(data$conflic, "1=4; 2=3; 3=2; 4=1; else=NA")
c2<-recode(data$conflic2, "1=4; 2=3; 3=2; 4=1; else=NA")
b1<-recode(data$basicva, "1=4; 2=3; 3=2; 4=1; else=NA")
b2<-recode(data$basicva2, "1=4; 2=3; 3=2; 4=1; else=NA")
d1<-recode(data$diffopi, "1=4; 2=3; 3=2; 4=1; else=NA")
d2<-recode(data$diffop2, "1=4; 2=3; 3=2; 4=1; else=NA")
sc.data$foundation<-ifelse(!is.na(f1), f1, f2)
sc.data$common<-ifelse(!is.na(lc1), lc1, lc2)
sc.data$conflict<-ifelse(!is.na(c1), c1, c2)
sc.data$values<-ifelse(!is.na(b1), b1, b2)
sc.data$opinion<-ifelse(!is.na(d1), d1, d2)
with(sc.data, psych::alpha(cbind(foundation, common, conflict, values, opinion)))
with(sc.data,cor(cbind(foundation, common, conflict, values, opinion), use="complete.obs"))

#### Party Identification #####
sc.data$PID<-rep(NA, length(sc.data[,1]))
sc.data$PID[data$republi==1]<-7
sc.data$PID[data$democra==1]<-1
sc.data$PID[data$democra==2]<-2
sc.data$PID[data$indepen==2|data$democra==9]<-3
sc.data$PID[data$indepen==3|data$indepen==9|data$genspeak==9]<-4
sc.data$PID[data$indepen==3|data$republi==9]<-5
sc.data$PID[data$republi==2]<-6
#### Ideology ####
sc.data$Ideology<-rep(NA, length(sc.data[,1]))
sc.data$Ideology[data$moderat==1| data$liberal==8|data$liberal==9]<-3
sc.data$Ideology[data$moderat==3| data$moderat==4| data$moderat>7| data$ideolog>7]<-4
sc.data$Ideology[data$moderat==2| data$conserv==8|data$conserv==9]<-5
sc.data$Ideology[data$conserv==1]<-7
sc.data$Ideology[data$conserv==2]<-6
sc.data$Ideology[data$liberal==1]<-1
sc.data$Ideology[data$liberal==2]<-2
####### Social Conformity #######
sc.data$accepts<-recode(data$accepts, "1=4; 2=3; 3=2; 4=1; else=NA")
sc.data$student<-recode(data$student, "1=1; 2=2; 3=3; 4=4; else=NA")
sc.data$fitinin<-recode(data$fitinin, "1=4; 2=3; 3=2; 4=1; else=NA")
sc.data$breakdo<-recode(data$breakdo, "1=4; 2=3; 3=2; 4=1; else=NA")
sc.data$lawless<-recode(data$lawless, "1=4; 2=3; 3=2; 4=1; else=NA")
sc.data$toofree<-recode(data$toofree, "1=4; 2=3; 3=2; 4=1; else=NA")
sc.data$obedien<-recode(data$obedien, "1=4; 2=3; 3=2; 4=1; else=NA")
with(sc.data, psych::alpha(cbind(accepts, student, fitinin, breakdo, lawless, toofree, obedien)))
with(sc.data, cor(cbind(accepts, fitinin, student, breakdo, lawless, toofree, obedien), use="complete.obs"))
#### Demographic Variables #####
sc.data$college<-recode(data$educate, "1:10=0; 11:14=1; else=NA")
sc.data$age<-2001-recode(data$yearbrn, "9998=NA; 9999=NA")
sc.data$jewish<-recode(data$religio, "3=1; 1:2=0; 4:8=0; else=NA")
sc.data$catholic<-recode(data$religio, "2=1; 1=0; 3:8=0; else=NA")
sc.data$protestant<-recode(data$religio, "1=1; 2:8=0; else=NA")
sc.data$other<-recode(data$religio, "1:3=0; 4:8=1; else=NA")
sc.data$white<-recode(data$race, "1=1; 2:8=0; else=NA")
sc.data$black<-recode(data$race, "2=1; 1=0; 3:8=0; else=NA")
sc.data$hispanic<-recode(data$race, "3=1; 1:2=0; 4:8=0; else=NA")
sc.data$other.race<-recode(data$race, "1:3=0; 4:8=1; else=NA")
sc.data$female<-recode(data$gender, "1=1; 2=0; else=NA")
### Income ###
sc.data$income<-(recode(data$income, "99=NA; 98=NA")-1)/12
save(sc.data, file="/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/New York Data/ny.Rdata")
