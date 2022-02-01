####-------------------------------------------------------------------------------------####
# This File Cleans 1990, 1992 2000 ANES 2008 ANES and YouGov, with equivalence across 
# operationalization of variables.
## Add vote in house elections, where possible
####-------------------------------------------------------------------------------------####
rm(list=ls())
detach("package:dplyr")
require(car)
source("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/configurations/configurations.r")
### Set working directory 
setwd("/Users/chrisweber/Dropbox/Data/Panel_Data_Files")
### These are user functions
### 1990 ANES ####
data.1990<-haven::read_dta("anes1990_1992.dta")
names(data.1990)
panel.1990<-data.frame(id=data.1990$V900004)
panel.1990$sex.1990<-recode(data.1990$V900547, "1=0; 2=1") #female
panel.1990$income.1990=recode(data.1990$V900663, "1:19=0; 20:23=1; else=NA") # 68th percentile in 1999
panel.1990$white.1990<-ifelse(data.1990$V900549==1 & data.1990$V900676!=1, 1, 0)
panel.1990$college.1990<-recode(data.1990$V900554, "0:15=0; 16:17=1; else=NA")
panel.1990$age.1990<-data.1990$V900548
panel.1990$protestant.1990<-recode(data.1990$V900528, "1=1; 2:4=0; else=NA")
panel.1990$catholic.1990<-recode(data.1990$V900528, "2=1; 1=0; 3:4=0; else=NA")
panel.1990$jewish.1990<-recode(data.1990$V900528, "3=1; 1:2=0; 4=0; else=NA")
panel.1990$other.1990<-recode(data.1990$V900528, "4=1; 1:3=0; else=NA")
## Authoritarianism
panel.1990$auth1.1992<-recode(data.1990$V926020, "1=3;3=2; 5=1; else=NA") #Obey Authority
panel.1990$auth2.1992<-recode(data.1990$V926019, "1=1;3=2; 5=3; else=NA") #Respect for elders
panel.1990$auth3.1992<-recode(data.1990$V926021, "1=1;3=2; 5=3; else=NA") #Good manners
panel.1990$auth4.1992<-recode(data.1990$V926022, "1=1;3=2; 5=3; else=NA") #Well behaved

### Presidential Vote
panel.1990$vote.1990<-NA
panel.1990$vote.1992<-NA


## Party Identification
panel.1990$pid.1990<-recode(data.1990$V900320, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican
panel.1990$pid.1991<-recode(data.1990$V912333, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican
panel.1990$pid.1992<-recode(data.1990$V923634, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican

## Check
for (i in 1:dim(panel.1990)[2]){
  print(table(panel.1990[,i]))
}


### 1992 Panel ####
### 1990 ANES ####
data.1992<-haven::read_dta("anes1992_1997.dta")
names(data.1992)
panel.1992<-data.frame(id=data.1992$VCASEID)
panel.1992$sex.1992<-recode(data.1992$V924201, "1=0; 2=1") #female
panel.1992$income.1992=recode(data.1992$V924104, "1:19=0; 20:24=1; else=NA") # 68th percentile in 1991
panel.1992$white.1992<-ifelse(data.1992$V924202==1 & data.1992$V924122!=1, 1, 0) # Non hispanic white
panel.1992$college.1992<-recode(data.1992$V923905, "0:15=0; 16:17=1; else=NA") #College
panel.1992$age.1992<-data.1992$V923903
panel.1992$protestant.1992<-recode(data.1992$V923830, "1=1; 2:4=0; else=NA")
panel.1992$catholic.1992<-recode(data.1992$V923830, "2=1; 1=0; 3:4=0; else=NA")
panel.1992$jewish.1992<-recode(data.1992$V923830, "3=1; 1:2=0; 4=0; else=NA")
panel.1992$other.1992<-recode(data.1992$V923830, "4=1; 1:3=0; else=NA")

table(panel.1992$sex.1992)
table(panel.1992$college.1992)
table(panel.1992$protestant.1992)
table(panel.1992$income.1992)


## Authoritarianism
panel.1992$auth1.1992<-recode(data.1992$V926020, "1=3;3=2; 5=1; else=NA") #Obey Authority
panel.1992$auth2.1992<-recode(data.1992$V926019, "1=1;3=2; 5=3; else=NA") #Respect for elders
panel.1992$auth3.1992<-recode(data.1992$V926021, "1=1;3=2; 5=3; else=NA") #Good manners
panel.1992$auth4.1992<-recode(data.1992$V926022, "1=1;3=2; 5=3; else=NA") #Well behaved

### Presidential Vote
panel.1992$vote.1992<-recode(data.1992$V925609, "1=1;2=0; else=NA") # Exclude perot
panel.1992$vote.1996<-recode(data.1992$V961082, "1=0;2=1;else=NA") 

## House Vote
panel.1992$vote.1992H<-recode(data.1992$V925622, "31=0; 32=1; 33=0; 34=1; 35=0; 36=1; 81=0; 82=1; 83=0; 84=1; 85=0;
                              86=1; 91=0; 92=1; else=NA") # 1992 Republican House Vote
panel.1992$vote.1994H<-recode(data.1992$V940614, "1=0;2=1; else=NA") # 1994 Republican House Vote
panel.1992$vote.1996H<-recode(data.1992$V961089, "1=0;2=1; else=NA") # 1996 Republican House Vote

## Party Identification
panel.1992$pid.1992<-recode(data.1992$V923634, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican
panel.1992$pid.1993<-recode(data.1992$V937370, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican
panel.1992$pid.1994<-recode(data.1992$V940655, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican
panel.1992$pid.1995<-recode(data.1992$V952263a, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican
panel.1992$pid.1996<-recode(data.1992$V960420 , "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican
panel.1992$pid.1997<-recode(data.1992$V970106 , "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican


for (i in 1:dim(panel.1992)[2]){
  print(table(panel.1992[,i]))
}

### 2000 ANES ####
data.2000<-haven::read_dta("anes2000_2004.dta")
names(data.2000)
panel.2000<-data.frame(id=data.2000$ID)

panel.2000$sex.2000<-recode(data.2000$M001029, "1=0; 2=1; else=NA")
panel.2000$income.2000=recode(data.2000$M000997, "1:8=0; 9:22=1") # 68th percentile in 1999, consistent with the cumulative
panel.2000$white.2000<-ifelse(data.2000$M001006a==50 | data.2000$M001013==7, 1, 0)
panel.2000$college.2000<-recode(data.2000$M000913, "1:5=0; 6:7=1; else=NA")
panel.2000$age.2000<-data.2000$M000908
# Religion
protestant.2000.1<-recode(data.2000$M000882, "1=1; 2:7=0; else=NA")
catholic.2000.1<-recode(data.2000$M000882, "2=1; 1=0; 3:7=0; else=NA")
jewish.2000.1<-recode(data.2000$M000882, "3=1; 1:2=0; 4:7=0; else=NA")
other.2000.1<-recode(data.2000$M000882, "1:6=0; 7=1; else=NA")
protestant.2000.2<-recode(data.2000$M000883, "1=1; 2:7=0; else=NA")
catholic.2000.2<-recode(data.2000$M000883, "2=1; 1=0; 3:7=0; else=NA")
jewish.2000.2<-recode(data.2000$M000883, "3=1; 1:2=0; 4:7=0; else=NA")
other.2000.2<-recode(data.2000$M000883, "1:6=0; 7=1; else=NA")
protestant.2000<-protestant.2000.1
protestant.2000[protestant.2000.2==1]<-1
catholic.2000<-catholic.2000.1
catholic.2000[catholic.2000.2==1]<-1
jewish.2000<-jewish.2000.1
jewish.2000[jewish.2000.2==1]<-1
other.2000<-other.2000.1
other.2000[other.2000.2==1]<-1


panel.2000$protestant.2000<-protestant.2000
panel.2000$jewish.2000<-jewish.2000
panel.2000$other.2000<-other.2000
panel.2000$catholic.2000<-catholic.2000

table(panel.2000$sex.2000)
table(panel.2000$college.2000)
table(panel.2000$protestant.2000)
table(panel.2000$income.2000)

## Authoritarianism
panel.2000$auth1.2000<-recode(data.2000$M001587, "1=3;3=2; 5=1; else=NA") #Obey Authority
panel.2000$auth2.2000<-recode(data.2000$M001586, "1=1;3=2; 5=3; else=NA") #Respect for elders
panel.2000$auth3.2000<-recode(data.2000$M001588, "1=1;3=2; 5=3; else=NA") #Good manners
panel.2000$auth4.2000<-recode(data.2000$M001589, "1=1;3=2; 5=3; else=NA") #Well behaved
### Presidential Vote
panel.2000$vote.2000<-recode(data.2000$M001249, "1=0; 3=1; else=NA") #Republican
panel.2000$vote.2004<-recode(data.2000$M045049a, "1=0; 3=1; else=NA") #Republican

## House Vote
panel.2000$vote.2000H<-recode(data.2000$M001263, "1=0; 2=1; else=NA") # 2000 Republican House Vote
panel.2000$vote.2002H<-recode(data.2000$M025028B, "1=0; 2=1; else=NA") # 2002 Republican House Vote
panel.2000$vote.2004H<-recode(data.2000$M045053, "1=0; 5=1; else=NA") # 2002 Republican House Vote

## Party Identification
panel.2000$pid.2000<-recode(data.2000$M000523, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican
panel.2000$pid.2002<-recode(data.2000$M023038X, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican
panel.2000$pid.2004<-recode(data.2000$M045058x, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA") #Republican

### Gay Policy Items ###
panel.2000$gay1.2000<-recode(data.2000$M001481, "1=1; 2=2; 4=3; 5=4; else=NA") #Oppose law protecting gays from discrimination
panel.2000$gay1.2004<-recode(data.2000$M045112x, "1=1; 2=2; 4=3; 5=4; else=NA") #Oppose law protecting gays from discrimination
# Homosexual Feeling Thermometer
panel.2000$gay2.2000<-data.2000$M001321
panel.2000$gay2.2002<-data.2000$M025067
panel.2000$gay2.2004<-data.2000$M045035
# Fundamental Feeling Thermometer
panel.2000$fund.2000<-data.2000$M001317
panel.2000$fund.2002<-data.2000$M025064
panel.2000$fund.2004<-data.2000$M045032
# Feminists Feeling Thermometer
panel.2000$fem.2000<-data.2000$M001326
panel.2000$fem.2002<-data.2000$M025071
panel.2000$fem.2004<-data.2000$M045039

## self placement ##
panel.2000$ideo.2000<-recode(data.2000$M001368, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2000$ideo.2002<-recode(data.2000$M023022, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")

## Policy placement ##
panel.2000$ideo.2000<-recode(data.2000$M001368, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2000$ideo.2002<-recode(data.2000$M023022, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2000$jobs.2000<-recode(data.2000$M000620, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
panel.2000$insurance.2000<-recode(data.2000$M000609, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2000$services.2000<-recode(data.2000$M001385, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")
panel.2000$abortion.2000<-recode(data.2000$M001403, "1=4; 2=3; 3=2; 4=1; else=NA")
panel.2000$abortion.2004<-recode(data.2000$M045110, "1=4; 2=3; 3=2; 4=1; else=NA")
##Adoption not in 02 04 waves ##

### Additional Items
panel.2000$big.business<-data.2000$M001313 
panel.2000$unions<-data.2000$M001312
panel.2000$womens.role<-recode(data.2000$M000760,  "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
panel.2000$partial.birth<-recode(data.2000$M001405, "1=4; 2=3; 4=2; 5=1; else=NA")
panel.2000$parental.consent<-recode(data.2000$M000702, "1=4; 2=3; 4=2; 5=1; else=NA")
panel.2000$gay.adopt<-recode(data.2000$M000748, "1=0; 5=1; else=NA")
panel.2000$gay.military<-recode(data.2000$M000727, "1=1; 2=2; 4=3; 5=1; else=NA")
panel.2000$guns<-recode(data.2000$M000731, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
panel.2000$protect.ss<-recode(data.2000$M000731, "1=1; 2=2; 4=3; 5=4; else=NA")
panel.2000$tax.cuts<-recode(data.2000$M000731, "1=4; 2=3; 4=2; 5=1; else=NA")
panel.2000$english<-recode(data.2000$M000745, "1=3; 3=2; 5=1; else=NA")
panel.2000$vouchers<-recode(data.2000$M000744, "1=4; 2=3; 4=2; 5=1; else=NA")

for (i in 1:dim(panel.2000)[2]){
  print(table(panel.2000[,i]))
}

###2008 ANES###
data.2008<-haven::read_dta("anes2008_2009.dta")
data.off<-foreign::read.spss("offwave.sav",
                     use.value.labels=FALSE, to.data.frame=TRUE)
data.2008<-merge(data.2008, data.off, by=c("caseid"))
names(data.2008)
panel.2008<-data.frame(caseid=data.2008$caseid)
panel.2008$college.2008<-recode(data.2008$der05, "4:5=1; 3:3=0; else=NA")
panel.2008$white.2008<-recode(data.2008$der04, "1=1; 3:4=0; else=NA")
panel.2008$income.2008=recode(data.2008$der06, "1:12=0; 13:19=1;else=NA") 
panel.2008$age.2008<-data.2008$der02
panel.2008$sex.2008<-recode(data.2008$der01, "1=0; 2=1; else=NA")
panel.2008$protestant.2008<-recode(data.2008$der22, "1=1;  2:5=0;  else=NA")
panel.2008$catholic.2008<-recode(data.2008$der22, "2=1;  1=0; 3:5=0; else=NA")
panel.2008$other.2008<-recode(data.2008$der22, "4=1;  1:3=0; 5=0; else=NA")
panel.2008$jewish.2008<-recode(data.2008$der22, "3=1;  1:2=0; 4:5=0; else=NA")
### Presidential Vote
panel.2008$vote.2008<-NA
panel.2008$vote.2009<-NA
## Party Identification
panel.2008$pid.w1<-recode(data.2008$der08w1, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA")
panel.2008$pid.w11<-recode(data.2008$der08w11, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA")
panel.2008$pid.w17<-recode(data.2008$der08w17, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA")
panel.2008$pid.w19<-recode(data.2008$der08w19, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else=NA")


## Merge in Off Wave data ###
## Authoritarianism, Wave 22
panel.2008$auth1.2008<-recode(data.2008$W22SOCI_2B, "1=1;4=0;else=NA") #Obey Authority
panel.2008$auth2.2008<-recode(data.2008$W22SOCI_2A, "1=0;4=1;else=NA") #Respect for elders
panel.2008$auth3.2008<-recode(data.2008$W22SOCI_2C, "1=0;4=1;else=NA") #Good manners
panel.2008$auth4.2008<-recode(data.2008$W22SOCI_2D, "1=0;4=1; else=NA") #Well behaved

for (i in 1:dim(panel.2008)[2]){
  print(table(panel.2008[,i]))
}


###2012-2016 - 2020 YouGov###
require(readstata13)
require(car)
data.2012<-readstata13::read.dta13("vsg.dta", 
                                   convert.factors=FALSE)
names(data.2012)
panel.2012<-data.frame(data.2012$cdid_2020Nov ) #
panel.2012$age.2012<-2016-ifelse(data.2012$birthyr_2011>2000, NA,data.2012$birthyr_2011)
panel.2012$sex.2012<-recode(data.2012$gender_2011, "1=0; 2=1; else=NA")
panel.2012$income.2012=recode(data.2012$faminc_2011, "1:5=0; 6:32=1;else=NA") # 68th percentile in 1999, consistent with the cumulative
panel.2012$white.2012<-recode(data.2012$race_2011, "1=1; 2:8=0; else=NA")
panel.2012$college.2012<-recode(data.2012$educ_2011, "5:6=1; 1:4=0; else=NA")

panel.2012$protestant.2012<-recode(data.2012$religion_2011, "1:4=1; 5:12=0; else=NA")
panel.2012$catholic.2012<-recode(data.2012$religion_2011, "1=0; 2=1;  3:12=0; else=NA")
panel.2012$jewish.2012<-recode(data.2012$religion_2011, "1:4=0; 5=1;  6:12=0; else=NA")
panel.2012$buddhist.2012<-recode(data.2012$religion_2011, "7=1; 1:6=0;  8:12=0; else=NA")
panel.2012$other.2012<-recode(data.2012$religion_2011, "1:6=0; 7:12=1; else=NA")
panel.2012$ft.gay1<-recode(data.2012$ft_gay_2011, "997=NA")
panel.2012$ft.gay2<-recode(data.2012$ft_gay_2011, "997=NA")

panel.2012$great.gen<-ifelse(data.2012$birthyr_2011<1928, 1, 0)
panel.2012$silent.gen<-ifelse(data.2012$birthyr_2011<1946 & data.2012$birthyr_2011>1927, 1, 0)
panel.2012$boomer.gen<-ifelse(data.2012$birthyr_2011>1945 & data.2012$birthyr_2011<1965, 1, 0)
panel.2012$genx.gen<-ifelse(data.2012$birthyr_2011>1964 & data.2012$birthyr_2011<1981, 1, 0)
panel.2012$mil.gen<-ifelse(data.2012$birthyr_2011>1980, 1, 0)

### Presidential Approval ###
panel.2012$approve.2012<-recode(data.2012$obamaapp_2011, "1=4; 2=3; 3=2; 4=1; else=NA") 
panel.2012$approve.2016<-recode(data.2012$obamaapp_2016, "1=4; 2=3;3=2; 4=1; else=NA") 
panel.2012$approve.2017<-recode(data.2012$trumpapp_2017, "1=4; 2=3;3=2; 4=1; else=NA") 
panel.2012$approve.2018<-recode(data.2012$trumpapp_2018, "1=4; 2=3;3=2; 4=1; else=NA")  
panel.2012$approve.2019<-recode(data.2012$trumpapp_2019, "1=4; 2=3;3=2; 4=1; else=NA")  
panel.2012$approve.2020<-recode(data.2012$trumpapp_2020Nov, "1=4; 2=3;3=2; 4=1; else=NA")  

### Additional Items for Chapter 7 ##

table(panel.2012$sex.2012)
table(panel.2012$college.2012)
table(panel.2012$protestant.2012)
table(panel.2012$income.2012)

#panel.2012$ft.mos1<-recode(data.2012$muslims_t_baseline, "997=NA")
#panel.2012$ft.ms2<-recode(data.2012$ft_muslim_2016, "997=NA")
#panel.2012$abortion.2012<-recode(data.2012$abortview3_baseline, "1=1; 2=2; 3=3; else=NA")
#anel.2012$abortion.2016<-recode(data.2012$abortview3_2016, "1=1; 2=2; 3=3; else=NA")

## Authoritarianism
panel.2012$auth1.2016<-recode(data.2012$sc3_obedience_2016, "1=1; 2=0;else=NA") #Obey Authority
panel.2012$auth2.2016<-recode(data.2012$sc1_independent_2016, "1=0;2=1;else=NA") #Respect for elders
panel.2012$auth3.2016<-recode(data.2012$sc2_curiosity_2016, "1=0;2=1;else=NA") #Good manners
panel.2012$auth4.2016<-recode(data.2012$sc4_considerate_2016, "1=0;2=1; else=NA") #Well behaved


### Presidential Vote
panel.2012$vote.2012<-recode(data.2012$presvote_2012, "1=0; 2=1; else=NA")
panel.2012$vote.2016<-recode(data.2012$presvote_2016, "1=0; 2=1; else=NA")
panel.2012$vote.2020<-recode(data.2012$presvote_2020Nov, "1=1; 2=0; else=NA")


panel.2012$ftfem.2016<-recode(data.2012$ft_fem_2016, "997=NA")
panel.2012$ftfem.2017<-recode(data.2012$ft_fem_2017, "997=NA")
panel.2012$ftgay.2012<-recode(data.2012$ft_gay_2011, "997=NA")
panel.2012$ftgay.2016<-recode(data.2012$ft_gay_2016, "997=NA")
panel.2012$ftgay.2020<-recode(data.2012$ft_gay_2019Nov , "997=NA")
panel.2012$gaymarry.2012<-recode(data.2012$view_gaymar_2011, "2=3; 3=2; else=NA")

panel.2012$health.2012<-recode(data.2012$univhealthcov_2011, "1=1; 2=2; else=NA")
panel.2012$health.2016<-recode(data.2012$univhealthcov_2016, "1=1; 2=2; else=NA")
panel.2012$health.2016<-recode(data.2012$univhealthcov_2020Nov, "1=1; 2=2; else=NA")


# panel.2012$tax.2012<-recode(data.2012$taxwealth_baseline, "3=2; 2=3; else=NA")
# panel.2012$tax.2016<-recode(data.2012$taxdoug_2016, "3=2; 2=3; else=NA")
# panel.2012$tax.2017<-recode(data.2012$taxdoug_2017, "3=2; 2=3; else=NA")


# panel.2012$healthreform.2012<-recode(data.2012$healthreformbill_baseline, "1=1; 2=2; 3=3; else=NA")
# panel.2012$healthreform.2016<-recode(data.2012$healthreformbill_2016, "1=1; 2=2; 3=3; else=NA")
# panel.2012$healthreform.2017<-recode(data.2012$healthreformbill_2017, "1=1; 2=2; 3=3; else=NA")

### House Vote
# panel.2012$voteH.2012<-recode(data.2012$post_house12_2012, "1=1; 2=2; else=NA") # Republican
#
## Party Identification
panel.2012$pid.2012a<-recode(data.2012$pid7_2011, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2012$pid.2012b<-recode(data.2012$pid7_2012, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2012$pid.2016<-recode(data.2012$pid7_2016, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2012$pid.2017<-recode(data.2012$pid7_2017, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2012$pid.2018<-recode(data.2012$pid7_2018, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2012$pid.2019<-recode(data.2012$pid7_2019Jan , "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
panel.2012$pid.2020<-recode(data.2012$pid7_2020Nov, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")


for (i in 1:dim(panel.2012)[2]){
  print(table(panel.2012[,i]))
}

##### Internet Recontact Survey ######
anes<-foreign::read.dta("anes2012.dta",
               convert.factors=FALSE)
anes.2012a<-foreign::read.dta("anes2013.dta",
                     convert.factors=FALSE)
anes<-merge(anes, anes.2012a, by=c("caseid"), all.x=T)  ### This tacks on the 2012 Internet Recontact

## Right Wing Authoritarianism ## 
panel.2015<-data.frame(anes$caseid)
panel.2015$rwa1.w2<-recode(anes$C5_U1, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  #Everyone has to create their own way*
panel.2015$rwa2.w2<-recode(anes$C5_U2, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  #Our country needs free thinkers*
panel.2015$rwa3.w2<-recode(anes$C5_U3, "1=5; 2=4; 3=3; 4=1; 5=1; else=NA")  #Place faith in authority figures
panel.2015$rwa4.w2<-recode(anes$C5_U4, "1=5; 2=4; 3=3; 4=1; 5=1; else=NA")  #Needs strong leaders
panel.2015$rwa5.w2<-recode(anes$C5_U5, "1=5; 2=4; 3=3; 4=1; 5=1; else=NA")  #Old fashioned ways

###W1 Child Rearind###
panel.2015$auth1.w1<-recode(anes$auth_obed, "1=3; 2=1; 3=2; else=NA") #Obey Authority
panel.2015$auth2.w1<-recode(anes$auth_ind, "1=1;  2=3; 3=2; else=NA") #Respect for elders
panel.2015$auth3.w1<-recode(anes$auth_cur, "1=1;   2=3; 3=2; else=NA") #Good manners
panel.2015$auth4.w1<-recode(anes$auth_consid, "1=1; 2=3; 3=2; else=NA") #Well behaved
###W2 Child Rearind###
panel.2015$auth1.w2<-recode(anes$C5_H2, "1=1; 2=0; else=NA") #Obey Authority
panel.2015$auth2.w2<-recode(anes$C5_H1, "1=0; 2=1; else=NA") #Respect for elders
panel.2015$auth3.w2<-recode(anes$C5_H3, "1=0;  2=1; else=NA") #Good manners
panel.2015$auth4.w2<-recode(anes$C5_H4, "1=0; 2=1; else=NA") #Well behaved

### Party ID 
panel.2015$pid.w1<-recode(anes$pid_x, "-2=NA")
panel.2015$pid.w2<-recode(anes$C5_G1, "1=1; 2=3; 3:4=2; else=NA")

##Political dispositions
panel.2015$ideology.w1<-recode(anes$libcpre_self, "-9=NA; -8=NA; -2=NA") ##Conservative
panel.2015$ideology.w2<-recode(anes$libcpo_self, "-9=NA; -8=NA; -7=NA; -6=NA; -2=NA") ##Conservative
panel.2015$ideology.w3<-recode(anes$C5_K1, "-9=NA; -8=NA; -7=NA; -6=NA; -2=NA") ##Conservative

panel.2015$protestant<-recode(anes$relig_7cat_x, "1:3=1; 5=1; 4=0; 6:8=0; else=NA") #All Protestant
panel.2015$catholic<-recode(anes$relig_7cat_x, "4=1; 1:3=0; 5:8=0; else=NA") #Catholic
panel.2015$jewish<-recode(anes$relig_7cat_x, "6=1; 1:5=0; 7:8=0; else=NA") #Jewish Dummy
panel.2015$otherrelig<-recode(anes$relig_7cat_x, "7=1; 1:6=0; 8=0; else=NA") #Other dummy
panel.2015$norelig<-recode(anes$relig_7cat_x, "8=1; 1:7=0;  else=NA") #Other dummy
###Demographics
panel.2015$female<-recode(anes$gender_respondent_x, "1=0; 2=1; else=NA")
panel.2015$white<-recode(anes$dem_raceeth_x, "1=1; 2:6=0; else=NA")
panel.2015$black<-recode(anes$dem_raceeth_x, "2=1; 1=0; 3:6=0; else=NA")
panel.2015$asian<-recode(anes$dem_raceeth_x, "3=1; 1:2=0; 4:6=0; else=NA")
panel.2015$college<-recode(anes$dem_edugroup_x, "1:3=0; 4:5=1; else=NA")
panel.2015$income<-recode(anes$inc_incgroup_pre, "1:20=0; 21:28=1; else=NA") #In 10k
panel.2015$age<-recode(anes$dem_age_r_x, "-2=NA")
panel.2015$mode<-recode(anes$mode, "1=1; 2=0")

## describe data
for (i in 1:dim(panel.2015)[2]){
  print(table(panel.2015[,i]))
}
###Save data
data.2016<-foreign::read.dta("anes2016.dta", convert.factors=FALSE)
case_id = data.2016$V160001_orig


## Authoritarianism -- Numbering matches the code above
auth.1.2016<-recode(data.2016$V162241, "1=3; 2=1; 3=2; else=NA") ### Obey
auth.2.2016<-recode(data.2016$V162239, "1=1; 2=3; 3=2; else=NA") ### Child repsect versus indepdnence
auth.3.2016<-recode(data.2016$V162240, "1=1; 2=3; 3=2; else=NA") ### Manners
auth.4.2016<-recode(data.2016$V162242, "1=1; 2=3; 3=2; else=NA") ### Behaved

## Trust

### Racial Resentment
rr1.2016<-recode(data.2016$V162211, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (Irish)
rr2.2016<-recode(data.2016$V162212, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Generations of slavery
rr3.2016<-recode(data.2016$V162213, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less
rr4.2016<-recode(data.2016$V162214, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder


## Government distrust
trust1.2016       <-recode(data.2016$V161215, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # Do what is right
trust2.2016       <-recode(data.2016$V161215, "1=2; 2=1;else=NA") # In it for selves
trust3.2016       <-recode(data.2016$V161215, "1=3; 2=2; 3=1;  else=NA") # Do what is right
trust4.2016       <-recode(data.2016$V161215, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Do what is right


##Vote direction in house and senate -- need to manually construct the split ballot measure
vote.house.2016<-recode(data.2016$V162068x, "1=0; 2=1; else=NA") # Rep vote
vote.senate.2016<-recode(data.2016$V162067x, "1=0; 2=1; else=NA")
vote.2016<-recode(data.2016$V162034a, "2=1; 1=0; else=NA") ### Two party vote, DT=1 

#### Income###
income.2016<-recode(data.2016$V161361x, "16:28=1; 1:15=0; else=NA") ## 68 percentile and higher, 55k, source dydqj.com calculator for 2015 income
#### ideology and pidi
pid.2016<-recode(data.2016$V161158x, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")  ### PID 01

## Religion
protestanta<-recode(data.2016$V161247a, "1=1; 2:4=0; -1=0; else=NA")
catholica<-recode(data.2016$V161247a, "2=1; 1=0; 3:4=0; -1=0; else=NA")
jewisha<-recode(data.2016$V161247a, "3=1; 1:2=0; 4=0; -1=0; else=NA")
othera<-recode(data.2016$V161247a, "4=1; 1:3=0; -1=0; else=NA")

protestantb<-recode(data.2016$V161247b, "1=1; 2:4=0; -1=0; else=NA")
catholicb<-recode(data.2016$V161247b, "2=1; 1=0; 3:4=0; -1=0; else=NA")
jewishb<-recode(data.2016$V161247b, "3=1; 1:2=0; 4=0; -1=0; else=NA")
otherb<-recode(data.2016$V161247b, "4=1; 1:3=0; -1=0; else=NA")

protestant.2016<-ifelse(protestanta==1 | protestantb==1, 1, 0)
catholic.2016<-ifelse(catholica==1 | catholicb==1, 1, 0)
jewish.2016<-ifelse(jewisha==1 | jewishb==1, 1, 0)
other.2016<-ifelse(othera==1 | otherb==1, 1, 0)


### Attend religious service
church.2016<-recode(data.2016$V161245, "-1=0; 5=0; 4=0; 3=0; 2=0; 1=1; -9=0")  ## Attend services at least once per week

### Demographics
### I can't locate census region
age.2016<-recode(data.2016$V161267, "-9=NA; -8=NA")
college.2016<-recode(data.2016$V161270, "13:16=1; 1:12=0; else=NA")  ## College degree
media.2016<-recode(data.2016$V162002, "1=0; 2:4=1; else=NA")  ## Watch tv about campaign, one or two, good many, 3 or 4.
white.2016<-recode(data.2016$V161310x, "1=1; 2:6=0; else=NA")
nonwhite.2016<-recode(data.2016$V161310x, "1=0; 2:6=1; else=NA")
black.2016<-recode(data.2016$V161310x, "2=1; 1=0; 3:6=0; else=NA")
hispanic.2016<-recode(data.2016$V161310x, "5=1; 1:4=0; 6=0; else=NA")
other.race.2016<-recode(data.2016$V161310x, "3:4=1; 1:2=0; 5=0; 6=1; else=NA")
case_id = data.2016$V160001

egal1.2016<-recode(data.2016$V162243, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Unequal opprotunity
egal2.2016<-recode(data.2016$V162244, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Worry less about eqwuality
egal3.2016<-recode(data.2016$V162245, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Tradition
egal4.2016<-recode(data.2016$V162246, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Fewer propnblems if fair treat

female.2016<-recode(data.2016$V161342, "1=0; 2=1; else=NA")

feeling.demc.2016<- recode(data.2016$V161086, "-98=NA; -99=NA; -88=NA")  #Feeling Dems Candidate
feeling.repc.2016<- recode(data.2016$V161087, "-98=NA; -99=NA; -88=NA")  #Feeling Reps Candidate
feeling.dem.2016 <- recode(data.2016$V161095, "-98=NA; -99=NA; -88=NA; -89=NA")  #Feeling Dems
feeling.rep.2016 <- recode(data.2016$V161096, "-98=NA; -99=NA; -88=NA; -89=NA")  #Feeling Reps


ideology.2016<-recode(data.2016$V161126, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")


dat_2016 = data.frame(auth.1.2016, auth.2.2016, auth.3.2016, auth.4.2016,
                      rr1.2016, rr2.2016, rr3.2016, rr4.2016,
                      trust1.2016, trust2.2016, trust3.2016, trust4.2016, 
                      vote.house.2016, vote.senate.2016, vote.2016,
                      income.2016, pid.2016, protestant.2016, catholic.2016,
                      jewish.2016, other.2016, age.2016, college.2016,
                      media.2016, white.2016, black.2016,
                      hispanic.2016, other.race.2016, case_id, 
                      egal1.2016, egal2.2016, egal3.2016, egal4.2016, 
                      female.2016, income.2016, ideology.2016,
                      feeling.demc.2016, feeling.dem.2016,
                      feeling.repc.2016, feeling.rep.2016
)


### 2020 Recodes
data.2020 = read.dta13("anes2020.dta")
data = data.2020
## demographics, 2020.
data = subset(data, V200003 == 2) # Subset on type -- panel only
case_id = data$V160001_orig
## authoritarianism scale
auth.3.2020<- recode(data$V202266, "1=1; 2=3; 3=2; else=NA")### Independence v. Respect
auth.2.2020<- recode(data$V202267, "1=1; 2=3; 3=2; else=NA")### Curisotiy or good manners
auth.1.2020<- recode(data$V202268, "1=3; 2=1; 3=2; else=NA")#### Obedience v. self reliance
auth.4.2020<- recode(data$V202269, "1=1; 2=3; 3=2; else=NA")##### Considerate v., well behaved

rr1.2020<-recode(data$V202300, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (Irish)
rr2.2020<-recode(data$V202301, "1=1; 2=2; 3=3; 4=4; 5=5; 8=NA; 9=NA; else=NA") ### Generations of slavery
rr3.2020<-recode(data$V202302, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less than they deserve
rr4.2020<-recode(data$V202303, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder


##Vote direction in house and senate -- need to manually construct the split ballot measure
vote.house.2020  <-  recode(data$V202106x, "10=0; 11=1; else=NA") # Rep vote
vote.senate.2020 <-  recode(data$V202107x, "10=0; 11=1; else=NA")
vote.2020        <-  recode(data$V202110x, "2=1; 1=0; else=NA") ### Two party vote, DT=1 

protestant.2020<-recode(data$V201435, "1=1; 2:12=0; else=NA")
catholic.2020<-recode(data$V201435, "2=1; 1=0; 3:12=0; else=NA")
jewish.2020<-recode(data$V201435, "5=1; 1:4=0; 6:12=0; else=NA")
other.2020<-recode(data$V201435, "11:12=1; 1:10=0; else=NA")
church.2020<-recode(data$V201453, "1=1; 2:5=0; else=NA") #Attend church at least once per week
bible.2020<-recode(data$V201434, "1=3; 2=2; 3=1; else=NA")

#### Income #### 
income.2020<-rep(NA, length(bible.2020))
income.2020<-recode(data$V202468x, "11:22=1; 1:10=0; else=NA") 
pid.2020<-recode(data$V201231x, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
female.2020   <-recode(data$V201600, "1=0; 2=1; else=NA") #
white.2020    <-ifelse(data$V201549x==1, 1, ifelse(data$V201549x!=1 & data$V201549x>0, 0, NA))
nonwhite.2020 <-abs(white-1)
black.2020    <-ifelse(data$V201549x==2, 1, ifelse(data$V201549x!=2 & data$V201549x>0, 0, NA))
hispanic.2020      <-ifelse(data$V201549x==3, 1, ifelse(data$V201549x!=3 & data$V201549x>0, 0, NA))
asian.2020         <-ifelse(data$V201549x==4, 1, ifelse(data$V201549x!=4 & data$V201549x>0, 0, NA))
other.race.2020    <-ifelse(black.2020==0 & white.2020==0 & hispanic.2020==0 & asian.2020 == 0, 1, 0)
age.2020          <-data$V201507x
college.2020      <-recode(data$V201511x, "4:5 =1; 1:3 = 0; else= NA" )
media.2020        <-recode(data$V201629a, "1=0; 2=1; else=NA")

ideology.2020<-recode(data$V201200, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")


### Trust government to do what is right

## Government distrust
## Government distrust
trust1.2020       <-recode(data$V201233, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # Do what is right
trust2.2020       <-recode(data$V201234, "1=2; 2=1;else=NA") # In it for selves
trust3.2020       <-recode(data$V201235, "1=3; 2=2; 3=1;  else=NA") # Do what is right
trust4.2020       <-recode(data$V201236, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Do what is right


## Anti-Egalitarianism
egal1.2020<-recode(data$V202260, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Unequal opprotunity
egal2.2020<-recode(data$V202261, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Worry less about eqwuality
egal3.2020<-recode(data$V202262, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## 
egal4.2020<-recode(data$V202263, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Fewer propnblems if fair treat

feeling.demc.2020<-recode(data$V201151, "-9=NA; 998=NA; -999=NA; -4=NA")  #Feeling Dems Candidate
feeling.repc.2020<-recode(data$V201152, "-9=NA; 998=NA; -999=NA; -4=NA")  #Feeling Rep Candidate
feeling.dem.2020 <-recode(data$V201156, "-9=NA; 998=NA; -999=NA; -4=NA")   #Feeling Dems
feeling.rep.2020 <-recode(data$V201157, "-9=NA; 998=NA; -999=NA;-4=NA")    #Feeling Reps



dat_2020 =  data.frame(auth.1.2020, auth.2.2020, auth.3.2020, auth.4.2020,
                       rr1.2020, rr2.2020, rr3.2020, rr4.2020,
                       trust1.2020, trust2.2020, trust3.2020, trust4.2020, 
                       vote.house.2020, vote.senate.2020, vote.2020,
                       income.2020, pid.2020, protestant.2020, catholic.2020,
                       jewish.2020, other.2020, age.2020, college.2020,
                       media.2020, white.2020, black.2020,
                       hispanic.2020, other.race.2020, case_id, 
                       egal1.2020, egal2.2020, egal3.2020, egal4.2020, 
                       female.2020, income.2020, ideology.2020,
                       feeling.demc.2020, feeling.dem.2020,
                       feeling.repc.2020, feeling.rep.2020
)



panel.1620 = merge(dat_2016, dat_2020, by = "case_id")


#d1<-panel.1990[ ,order(names(panel.1990))]
panel_data_2000<-panel.2000[ ,order(names(panel.2000))]
#d3<-panel.2008[ ,order(names(panel.2008))]
panel_data_2012<-panel.2012[ ,order(names(panel.2012))]
#d5<-panel.1992[ ,order(names(panel.1992))]
#d6<-panel.2015[ ,order(names(panel.2015))]
panel_data_2016<-panel.1620[ ,order(names(panel.1620))]


save(panel_data_2000, file="/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data/panel_data_2000.rda")
save(panel_data_2012, file="/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data/panel_data_2012.rda")
save(panel_data_2016, file="/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data/panel_data_2016.rda")


