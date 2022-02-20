###  Data Description ##
# Load cumulative data 
#### The Cumulative Data ####
rm(list=ls())
require(readstata13)
library(foreign)
detach("package:dplyr")
library(car)

source("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/configurations/configurations.r")
### Set working directory 
setwd("/Users/chrisweber/Dropbox/Data/Panel_Data_Files")
### These are user functions
### 1990 ANES ####
data_location = "/Users/chrisweber/Dropbox/Data/cross_sectional"
setwd("/Users/chrisweber/Desktop/Authoritarianism_V2/Authoritarianism_V2/clean_data")
detach("package:dplyr")

anes <- read.dta13(paste0(data_location, "/anes_timeseries_cdf_stata12.dta"),
               convert.factors=FALSE)
CD   <- as.numeric(recode(anes$VCF0900, "0=NA; 99=NA")) ##00 no district, 99 DC
STATE<- as.numeric(recode(anes$VCF0901a, "00=NA; 99=NA")) ##00 Missing, 99 NA
year <-   anes$VCF0004
rid  <-   anes$VCF0006
##Authoritarianism
ideology <- recode(anes$VCF0803, "0=NA; 9=NA") ##Conservative
pid      <- (recode(anes$VCF0301, "0=NA; 9=NA")-1)/6##Republican
# ##1970-1990;
# protestant<-recode(anes$VCF0128a, "1=1; 2:7=0; else=NA") #Mainline Protestant Dummy
# evangelical<-recode(anes$VCF0128a, "2=1; 1=0; 3:7=0; else=NA") #Evangelical Protestant Dummy
# catholic<-recode(anes$VCF0128a, "3=1; 1:2=0; 4:7=0; else=NA") #Catholic Dummy
# jewish<-recode(anes$VCF0128a, "4=1; 1:3=0; 5:7=0; else=NA") #Jewish Dummy
# other<-recode(anes$VCF0128a, "5:7=1; 1:4=0; else=NA") #Other dummy
# ##1990-1996
# protestant<-recode(anes$VCF0128b, "0=1; 1=0; 2=1; 3=0; 4=0; 5=0; 6=0; 7=0; else=NA") #Mainline Protestant Dummy/Black Protestant
# evangelical<-recode(anes$VCF0128b, "0=0;1=1;  2=0; 3=0; 4=0; 5=0; 6=0; 7=0; else=NA") #Evangelical Protestant Dummy
# catholic<-recode(anes$VCF0128b, "0=0; 1=0; 2=0; 3=1; 4=0; 5=0; 6=0; 7=0; else=NA") #Catholic Dummy
# jewish<-recode(anes$VCF0128b, "0=0; 1=0; 2=0; 3=0; 4=1; 5=0; 6=0; 7=0; else=NA") #Jewish Dummy
# other<-recode(anes$VCF0128b, "0=1; 1=0; 2=0; 3=0; 4=0; 5=1; 6=1; 7=1; else=NA") #Other dummy
##Time invariant question. This is correct, evangelical wasn't asked post 1996?
protestant    <-    recode(anes$VCF0128, "1=1;  2=0; 3=0; 4=0; else=NA")
catholic      <-    recode(anes$VCF0128, "1=0;  2=1; 3=0; 4=0; else=NA")
jewish        <-    recode(anes$VCF0128, "1=0;  2=0; 3=1; 4=0; else=NA")
other         <-    recode(anes$VCF0128, "1=0;  2=0; 3=0; 4=1; else=NA")
##Church attendance
church        <-    recode(anes$VCF0130, "1=1; 2:7=0; else=NA") #Attend church at least once per week
###Biblical literalism
bible<-recode(anes$VCF0850, "1=3; 2=2; 3=1; else=NA")


#### Income #### E
income<-rep(NA, length(bible))
income<-recode(anes$VCF0114, "0=NA; 4:5=1; 1:3=0; else=NA") #Greater than 68 percentile
primary<-recode(anes$VCF9026, "1:5=1; 7=0; 9=NA")
####Religion importance
religious.importance<-recode(anes$VCF0846, "1=1; 2=0; else=NA")
###Demographics
female<-recode(anes$VCF0104, "1=0; 2=1; 0=NA")
white<-recode(anes$VCF0105a, "2:7=0; 1=1; else=NA")
nonwhite<-recode(anes$VCF0105a, "2:7=1; 1=0; else=NA")
black<-recode(anes$VCF0105a, "1=0; 3:7=0; 2=1; else=NA")
asian<-recode(anes$VCF0105a, "1:2=0; 4:7=0; 3=1; else=NA")
hispanic<-recode(anes$VCF0105a, "1:4=0; 6:7=0; 5=1; else=NA")
other.race<-recode(anes$VCF0105a, "1=0; 2=0; 3:4=1; 5=0; 6:7=1; else=NA")
#other<-recode(anes$VCF0105a, "1:3=0; 5=0; 4=1; 6:7=1; else=NA")
age<-recode(anes$VCF0101, "0=NA")
college<-recode(anes$VCF0110, "1:3=0; 4=1; else=NA")
northeast<-recode(anes$VCF0112, "1=1; 2=0; 3=0; 4=0; else=NA")
north.central<-recode(anes$VCF0112, "1=0; 2=1; 3=0; 4=0; else=NA")
south<-recode(anes$VCF0112, "1=0; 2=0; 3=1; 4=0; else=NA")
west<-recode(anes$VCF0112, "1=0; 2=0; 3=0; 4=1; else=NA")
#income<-recode(anes$VCF0114, "0=NA") #Percentile, change of 16
##Media consumption and knowledge.
media<-recode(anes$VCF0724, "1=0; 2=1; else=NA")  #Media consumption
media.days<-recode(anes$VCF9035, "8:9=NA")/7  #Media consumption
#income<-recode(anes$VCF0114, "0=NA") #Percentile, change of 16

### Generate a sophistication measure ####
knowledge.house<-recode(anes$VCF0729, "2=1; 1=0; else=NA") #Party with House Majority before election

# PARTY. Ideological placement, insurance, guaranteed jobs, services, defense
#ideology.placement<-recode(anes$VCF0502, "2=1; 1=0; 9=0; else=NA") #conservatives are more conservative
ideology.dems<-recode(anes$VCF0503, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #ideology democratic party
ideology.reps<-recode(anes$VCF0504, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #ideology democratic party
knowledge.ideology<-rep(NA, length(ideology.dems))
knowledge.ideology[ideology.reps>ideology.dems]<-1
knowledge.ideology[ideology.reps<=ideology.dems]<-0

##Insurance plan
insurance.dems<-recode(anes$VCF0508, "0=NA; 8=NA") #Private insurance plan, Dems
insurance.reps<-recode(anes$VCF0509, "0=NA; 8=NA") #Private insurance plan, Dems
knowledge.insurance<-rep(NA, length(insurance.dems))
knowledge.insurance[insurance.reps>insurance.dems]<-1
knowledge.insurance[insurance.reps<=insurance.dems]<-0

## Guaranteed jobs and standard of living
government.dems<-recode(anes$VCF0513, "0=NA; 8=NA; 9=NA") #Government living, Dems
government.reps<-recode(anes$VCF0514, "0=NA; 8=NA; 9=NA") #Government living, Dems
knowledge.government<-rep(NA, length(government.dems))
knowledge.government[government.reps>government.dems]<-1
knowledge.government[government.reps<=government.dems]<-0

feel.black = ifelse(anes$VCF0206 >= 97, NA, anes$VCF0206)
feel.latino = ifelse(anes$VCF0217 >= 97, NA, anes$VCF0217)
feel.asian = ifelse(anes$VCF0227 >= 97, NA, anes$VCF0227)

aid.blacks<-recode(anes$VCF0830, "0=NA; 8=NA; 9=NA") #Government living, Dems
affirmatve.action<-recode(anes$VCF0867a, "1=1; 2=2; 4=3;  5=4; 7=NA; 8=NA; 9=NA") #Government living, Dems

## Aid blacks: THis is only through 2004
#aid.dems<-recode(anes$VCF0517, "0=NA; 8=NA; 9=NA") #Aid black, Dems
#aid.reps<-recode(anes$VCF0518, "0=NA; 8=NA; 9=NA") #Aid black, Reps
#knowledge.aid<-rep(NA, length(aid.dems))
#knowledge.aid[aid.reps>aid.dems]<-1
#knowledge.aid[aid.reps<=aid.dems]<-0

## Government services, coded in conservative direction
gov.dems<-recode(anes$VCF0541, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 0=NA; 8=NA; 9=NA") #Services, Dems
gov.reps<-recode(anes$VCF0542, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1;0=NA; 8=NA; 9=NA") # Services, Reps
knowledge.gov<-rep(NA, length(gov.dems))
knowledge.gov[gov.reps>gov.dems]<-1
knowledge.gov[gov.reps<=gov.dems]<-0

## Defense spending
defense.dems<-recode(anes$VCF0549, "0=NA; 8=NA; 9=NA") #Defense, Dems
defense.reps<-recode(anes$VCF0550, "0=NA; 8=NA; 9=NA") # Defnse, Reps
knowledge.defense<-rep(NA, length(defense.dems))
knowledge.defense[defense.reps>defense.dems]<-1
knowledge.defense[defense.reps<=defense.dems]<-0

know.interview.pre<-recode(anes$VCF0050a, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
know.interview.post<-recode(anes$VCF0050b, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")

interest.elections<-recode(anes$VCF0310, "1=1; 2=2; 3=2; else=NA") #Interest in campaign
interest.politics<-recode(anes$VCF0311, "1=0; 2=1;  else=NA") #Interest in party winner
##interest in public affairs wasn't asked in 2012.

#### traits
anes$dem.intelligence<-recode(anes$VCF0350, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$dem.compassion<-recode(anes$VCF0351, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$dem.decent<-recode(anes$VCF0352, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$dem.inspiring<-recode(anes$VCF0353, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$dem.know<-recode(anes$VCF0354, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$dem.moral<-recode(anes$VCF0355, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$dem.leader<-recode(anes$VCF0356, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$dem.cares<-recode(anes$VCF0357, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$dem.angry<-recode(anes$VCF0358, "1=2; 2=1;else=NA") 
anes$dem.afraid<-recode(anes$VCF0359, "1=2; 2=1;else=NA") 
anes$dem.hope<-recode(anes$VCF0360, "1=2; 2=1;else=NA") 
anes$dem.proud<-recode(anes$VCF0361, "1=2; 2=1;else=NA") 

anes$rep.intelligence<-recode(anes$VCF0362, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$rep.compassion<-recode(anes$VCF0363, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$rep.decent<-recode(anes$VCF0364, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$rep.inspiring<-recode(anes$VCF0365, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$rep.know<-recode(anes$VCF0366, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$rep.moral<-recode(anes$VCF0367, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$rep.leader<-recode(anes$VCF0368, "1=4; 2=3; 3=2; 4=1; else=NA") 
anes$rep.cares<-recode(anes$VCF0369, "1=4; 2=3; 3=2; 4=1; else=NA") 

anes$rep.angry<-recode(anes$VCF0370, "1=2; 2=1;else=NA") 
anes$rep.afraid<-recode(anes$VCF0371, "1=2; 2=1;else=NA") 
anes$rep.hope<-recode(anes$VCF0372, "1=2; 2=1;else=NA") 
anes$rep.proud<-recode(anes$VCF0373, "1=2; 2=1;else=NA") 

out = paste0(c("dem."), c("decent", 
                          "know", "leader", 
                          "cares", "angry", 
                          "afraid", "hope", 
                          "pride"

))
dems =   anes[, names(anes) %in% out]

out = paste0(c("rep."), c("decent", 
                          "know", "leader", 
                          "cares", "angry", 
                          "afraid", "hope", 
                          "pride"
                          
))
reps =   anes[, names(anes) %in% out]



##Efficacy###
voted<-recode(anes$VCF0702, "2=1; 1=0; else=NA") #Did Respondent Vote in the National Elections
vote.house<-recode(anes$VCF0707, "1=0; 2=1; else=NA") # Vote for Congress, Republican
vote.senate<-recode(anes$VCF0708, "1=0; 2=1; else=NA") # Republican
split.house<-recode(anes$VCF0709, "1='DP-DC'; 2='DP-RC'; 3='RP-DC'; 4='RP-RC'; else=NA") # Republican
#split.house2<-recode(anes$VCF0709, "1=0; 2=1; 3=0; 4=1; else=NA") # Split Ballot
split.senate<-recode(anes$VCF0710, "1='DP-DS'; 2='DP-RS'; 3='RP-DS'; 4='RP-RS'; else=NA") # Republican
#split.senate2<-recode(anes$VCF0710, "1=0; 2=1; 3=0; 4=1; else=NA") # Split Ballot
#efficacy2<-recode(anes$VCF0717, "1=0; 2=1; else=NA") #Respondent Try to Influence the Vote of Others During the Campaign
efficacy1<-recode(anes$VCF0624, "1=1;2=2; 3=3; else=NA") #How Much Elections Make Government Pay Attention to People
efficacy2<-recode(anes$VCF0609, "1=1; 2=3; 3=2; else=NA") #People care about what I think
efficacy3<-recode(anes$VCF0606, "1=1;2=2; 3=3; else=NA") #Government does not waste money
efficacy4<-recode(anes$VCF0605, "1=1;2=2; else=NA") #Government run for benefit of all
efficacy5<-recode(anes$VCF0604, "1=1;2=2; 3=3; 4=4; else=NA") #Trust government to do what is right
efficacy6<-recode(anes$VCF0613, "1=1;2=3; 3=2; else=NA") #People like me have a say
efficacy7<-recode(anes$VCF0614, "1=1;2=3; 3=2; else=NA") #government is not too complicated

p1<-recode(anes$VCF0718, "1=0; 2=1; else=NA") #Attend rally
p2<-recode(anes$VCF0719, "1=0; 2=1; else=NA") #Work for candidate
p3<-recode(anes$VCF0720, "1=0; 2=1; else=NA") #Button
p4<-recode(anes$VCF0721, "1=0; 2=1; else=NA") #Donate money
p5<-recode(anes$VCF0717, "1=0; 2=1; else=NA") #INfluence others vote

# There is a battery of donation questions that I am missing here.
house.vote<-recode(anes$VCF0876, "1=0; 5=1; else=NA") # Vote Republican

##Salient Issues, There are two VCF0876 questions
#issue.morality<-recode(anes$VCF0875, "7=1; 1:6=0; 8:97=0; else=NA") # Vote Republican
## These aren't the branched graded questions
protect.gays<-recode(anes$VCF0876a, "1=0; 5=1; else=NA") # Do you favor or oppose laws to protect homosexuals against job discrimination?
gays.military<-recode(anes$VCF0877, "1=0; 5=1; else=NA") # Do you favor or oppose laws to protect homosexuals against job discrimination?
gays.adoption<-recode(anes$VCF0878, "1=0; 5=1; else=NA") # Gay Adoption
##Immigration
immigrants<-recode(anes$VCF0879, "8=NA; 9=NA") # Decrease immigration

### Racial Resentment
rr1<-recode(anes$VCF9040, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (IRish)
rr2<-recode(anes$VCF9039, "1=1; 2=2; 3=3; 4=4; 5=5; 8=NA; 9=NA; else=NA") ### Generations of slavery
rr3<-recode(anes$VCF9042, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less than they deserve
rr4<-recode(anes$VCF0508, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder

##Feeling items
feeling.dem<-recode(anes$VCF0218, "98:99=NA")  #Feeling Dems
feeling.rep<-recode(anes$VCF0224, "98:99=NA")  #Feeling Reps
feeling.demc<-recode(anes$VCF0424, "98:99=NA")  #Feeling Dems
feeling.repc<-recode(anes$VCF0426, "98:99=NA")  #Feeling Reps


### Additional Items ####
public.insurance<-recode(as.numeric(anes$VCF0806), "0=NA; 8:9=NA") #Private insurance 
public.insuranceCD<-recode(as.numeric(anes$VCF9085), "0=NA; 8:9=NA") #Private insurance 
public.insuranceCR<-recode(as.numeric(anes$VCF9093), "0=NA; 8:9=NA") #Private insurance 
women.role<-recode(as.numeric(anes$VCF0834), "0=NA; 8:9=NA") #women's role in home
women.roleCD<-recode(as.numeric(anes$VCF9083), "0=NA; 8:9=NA") #women's role in home
women.roleCR<-recode(as.numeric(anes$VCF9091), "0=NA; 8:9=NA") #women's role in home
#Party not candidate
aid.blacks<-recode(as.numeric(anes$VCF0830), "0=NA; 9=NA") #government should not help blacks
aid.blacksD<-recode(as.numeric(anes$VCF0517), "0=NA; 8:9=NA") # Stops in 2004
aid.blacksR<-recode(as.numeric(anes$VCF0518), "0=NA; 8:9=NA") 
aid.blacksCD<-recode(as.numeric(anes$VCF9084), "0=NA; 8:9=NA") # Stops in 2004
aid.blacksCR<-recode(as.numeric(anes$VCF9092), "0=NA; 8:9=NA") 
gov.services<-recode(as.numeric(anes$VCF0839), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1;  else=NA") #Fewer services
gov.servicesCD<-recode(as.numeric(anes$VCF9086), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA") 
gov.servicesCR<-recode(as.numeric(anes$VCF9094), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA") 
gov.servicesD<-recode(as.numeric(anes$VCF0541), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA") 
gov.servicesR<-recode(as.numeric(anes$VCF0542), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA") 
#Jobs and standard of living
jobs<-recode(as.numeric(anes$VCF0809), "0=NA; 9=NA") #People should get ahead on own
jobsCD<-recode(as.numeric(anes$VCF9087), "0=NA; 8:9=NA") 
jobsCR<-recode(as.numeric(anes$VCF9095), "0=NA; 8:9=NA")
jobsD<-recode(as.numeric(anes$VCF0513), "0=NA; 8:9=NA") 
jobsR<-recode(as.numeric(anes$VCF0514), "0=NA; 8:9=NA")
defense.spending<-recode(as.numeric(anes$VCF0843), "0=NA; 9=NA") #government should provide more defense spending
defense.spendingCD<-recode(as.numeric(anes$VCF9081), "0=NA; 8:9=NA") 
defense.spendingCR<-recode(as.numeric(anes$VCF9089), "0=NA; 8:9=NA") 
defense.spendingD<-recode(as.numeric(anes$VCF0549), "0=NA; 8:9=NA") 
defense.spendingR<-recode(as.numeric(anes$VCF0550), "0=NA; 8:9=NA") 

strong.gov <- recode(as.numeric(anes$VCF0829), "1=0; 2=1; else=NA")

ideologyD<-recode(as.numeric(anes$VCF0503), "0=NA; 8:9=NA") 
ideologyR<-recode(as.numeric(anes$VCF0504), "0=NA; 8:9=NA") 
ideologyCD<-recode(as.numeric(anes$VCF9088), "0=NA; 8:9=NA") 
ideologyCR<-recode(as.numeric(anes$VCF9096), "0=NA; 8:9=NA") 


###No party placement questions ####
gays.adoption<-recode(as.numeric(anes$VCF0878), "1=0; 5=1; else=NA") #Ban gay adoptionng
immigrants<-recode(as.numeric(anes$VCF0879), "8=NA; 9=NA") #immigrants
ideology_respondent<-recode(anes$VCF0803, "0=NA; 9=NA")
gays.discrimination<-recode(as.numeric(anes$VCF0876), "1=0; 5=1; else=NA") #Gays Jobs
abortion<-recode(as.numeric(anes$VCF0838), "1=4; 2=3; 3=2; 4=1; 0=NA; 9=NA") #Ban abortion
#Abortion and women role lacking
public.insuranceD<-recode(as.numeric(anes$VCF0508), "0=NA; 8:9=NA") 
public.insuranceR<-recode(as.numeric(anes$VCF0509), "0=NA; 8:9=NA") 
vote<-recode(anes$VCF0704, "1=0; 2=1; else=NA")
### Moral Traditionalism ###
moral1<-recode(anes$VCF0852, "1=1; 2=2; 3=3; 4=4; 5=5 ;else=NA")  #Should Adjust View of Moral Behavior to Changes
moral2<-recode(anes$VCF0853, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")   #Should be More Emphasis on Traditional Values
moral3<-recode(anes$VCF0854, "1=1; 2=2; 3=3; 4=4; 5=5 ;else=NA")  #Tolerance of Different Moral Standards
moral4<-recode(anes$VCF0851, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  #Newer lifestyles contributing to societal breakeown
### Egalitarianism ###
egal1<-recode(anes$VCF9013, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Unequal opprotunity
egal2<-recode(anes$VCF9017, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Worry less about eqwuality
egal3<-recode(anes$VCF9016, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Not a big problem if everyone doesn't have chance
egal4<-recode(anes$VCF9018, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Fewer propnblems if fair treat
mode<-recode(anes$VCF0017, "0='FTF'; 1='Tpre'; 2='Tpost'; 3='Tboth'; 4='Internet'")
### Emotion Questions
anger.dem<-recode(anes$VCF0358, "1=1; 2=0; else=NA")
fear.dem<-recode(anes$VCF0359, "1=1; 2=0; else=NA")
hope.dem<-recode(anes$VCF0360, "1=1; 2=0; else=NA")
proud.dem<-recode(anes$VCF0361, "1=1; 2=0; else=NA")
anger.rep<-recode(anes$VCF0370, "1=1; 2=0; else=NA")
fear.rep<-recode(anes$VCF0371, "1=1; 2=0; else=NA")
hope.rep<-recode(anes$VCF0372, "1=1; 2=0; else=NA")
proud.rep<-recode(anes$VCF0373, "1=1; 2=0; else=NA")


### Feeling Thermometer
gay.therm<-recode(anes$VCF0232, "98=NA; 99=NA")
feminists.therm<-recode(anes$VCF0253, "98=NA; 99=NA")
fundamental.therm<-recode(anes$VCF0234, "98=NA; 99=NA")
union.therm<-recode(anes$VCF0210, "98=NA; 99=NA")

weights.all<-anes$VCF0009z
weights.ftf<-anes$VCF0009x
weights.web<-anes$VCF0009y

###Below I ANES authoritarianism data.

# Additional questions

# knowledge.house, ideology.dems, ideology.reps, knowledge.ideology,
# insurance.dems, insurance.reps, knowledge.insurance,
# government.dems, government.reps, knowledge.government,
# gov.dems, gov.reps, knowledge.gov,
# defense.dem, defense.rep, knowledge.defense,
# government.demC, government.repC, knowledge.governmentc,
# gov.demC, gov.repC, knowledge.govc,
# defense.demC, defense.repC, knowledge.defensec,
# efficacy1, efficacy2, efficacy3, efficacy4, efficacy4, efficacy5, efficacy6,
# split.house, split.house2, split.senate, split.senate2, vote.house,
# protect.gays, gays.military, gays.adopt, immigrants, 
# moral1, moral2, moral3

### Cumulative Data file for merge with 2016
data.cumulative.anes<-data.frame(female, college,
                          church,catholic,
                          jewish,other, bible, year,
                          rid, CD, STATE, pid, media,
                          feeling.dem, feeling.rep, feeling.demc, feeling.repc,
                          abortion, vote, 
                          rr1, rr2, rr3, rr4, white, nonwhite, black, hispanic, other.race,
                          age, primary, income,
                          split.house, split.senate, vote.house,
                          vote.senate, 
                          moral1, moral2, moral3, moral4, voted,
                          interest.elections, interest.politics,
                          know.interview.pre, know.interview.post,
                          p1, p2, p3, p4, p5, efficacy1, efficacy2, efficacy3,
                          efficacy4, efficacy5, efficacy6, efficacy7,
                          public.insurance, public.insuranceCD, public.insuranceCR,
                          women.role, women.roleCD, women.roleCR,
                          aid.blacksD, aid.blacksR, aid.blacksCD, aid.blacksCR,
                          gov.services, gov.servicesCD, gov.servicesCR,
                          gov.servicesR, gov.servicesD, jobs, jobsCR, jobsCD,
                          jobsR, jobsD, defense.spendingCD, defense.spendingCR,
                          defense.spendingD, defense.spendingR, ideology, ideologyR, 
                          ideologyD, ideologyCD, ideologyCR, egal1, egal2, egal3, egal4, 
                          mode, gay.therm, feminists.therm, fundamental.therm,
                          protect.gays, gays.military, gays.adoption, immigrants, union.therm,
                          weights.all, weights.ftf, weights.web,
                          anger.dem, fear.dem, hope.dem, proud.dem, 
                          anger.rep, fear.rep, hope.rep, proud.rep,
                          feel.black, feel.latino, feel.asian, aid.blacks, reps, dem)

require(foreign)                          
####-------------------------------------------------------------------------------------####
####-------------------------------------------------------------------------------------####
####----------------------------------Code authoritarianism cross-------------------------------------####
####-------------------------------------------------------------------------------------####
####-------------------------------------------------------------------------------------####
###  Data Description ##


#### The Cross sections
anes.1990<-read.dta(paste0(data_location, "/NES1990.dta"), convert.factors=FALSE)
anes.1992<-read.dta(paste0(data_location,"/NES1992.dta"), convert.factors=FALSE)
anes.1994<-read.dta(paste0(data_location,"/NES1994.dta"), convert.factors=FALSE)
anes.2000<-read.dta(paste0(data_location,"/anes2000TS.dta"), convert.factors=FALSE)
anes.2004<-read.dta(paste0(data_location,"/anes2004TS.dta"), convert.factors=FALSE)
anes.2008<-read.dta(paste0(data_location,"/anes_timeseries_2008_stata12.dta"), convert.factors=FALSE)
anes.2012<-read.dta(paste0(data_location,"/anes_timeseries_2012_Stata12.dta"), convert.factors=FALSE)
###1990##
id.1990<-anes.1990$V900004 #Identifier
year.1990<-rep(1990, times=(length(id.1990)))
auth1.1990<-recode(anes.1990$V900330, "1=1; 2=2; 3=1; else=NA") #Obey authority

###1992###
id.1992<-anes.1992$V923004 #Identifier
year.1992<-rep(1992, times=(length(id.1992)))
auth1.1992<-recode(anes.1992$V926020, "1=2; 3=1; 5=1; else=NA") #Obey authority
auth2.1992<-recode(anes.1992$V926019, "1=1; 3=1; 5=2; else=NA") #Respect for Elders
auth3.1992<-recode(anes.1992$V926021, "1=1; 3=1; 5=2; else=NA") #Good Manners
auth4.1992<-recode(anes.1992$V926022, "1=1; 3=1; 5=2; else=NA") #Well behaved

###1994###
id.1994<-anes.1994$V940001 #Identifier
year.1994<-rep(1994, times=(length(id.1994)))
auth1.1994<-recode(anes.1994$V926020, "1=2; 3=1; 5=1; else=NA") #Obey Authority
auth2.1994<-recode(anes.1994$V926019, "1=1;3=1; 5=2; else=NA") #Respect for elders
auth3.1994<-recode(anes.1994$V926021, "1=1;3=1; 5=2; else=NA") #Good manners
auth4.1994<-recode(anes.1994$V926022, "1=1;3=1; 5=2; else=NA") #Well behaved
###2000###
id.2000<-anes.2000$V000001 #Identifier
year.2000<-rep(2000, times=(length(id.2000)))
auth1.2000<-recode(anes.2000$V001587, "1=2;3=1; 5=1; else=NA") #Obey Authority
auth2.2000<-recode(anes.2000$V001586, "1=1;3=1; 5=2; else=NA") #Respect for elders
auth3.2000<-recode(anes.2000$V001588, "1=1;3=1; 5=2; else=NA") #Good manners
auth4.2000<-recode(anes.2000$V001589, "1=1;3=1; 5=2; else=NA") #Well behaved
###2004###
id.2004<-anes.2004$V040001 #Identifier
year.2004<-rep(2004, times=(length(id.2004)))
auth1.2004<-recode(anes.2004$V045210, "1=2;3=1; 5=1; else=NA") #Obey Authority
auth2.2004<-recode(anes.2004$V045208, "1=1;3=1; 5=2; else=NA") #Respect for elders
auth3.2004<-recode(anes.2004$V045209, "1=1;3=1; 5=2; else=NA") #Good manners
auth4.2004<-recode(anes.2004$V045211, "1=1;3=1; 5=2; else=NA") #Well behaved


###2008###
id.2008<-anes.2008$V080001 #Identifier
year.2008<-rep(2008, times=(length(id.2008)))
auth1.2008<-recode(anes.2008$V085160, "1=2;3=1; 5=1; else=NA") #Obey Authority
auth2.2008<-recode(anes.2008$V085158, "1=1;3=1; 5=2; else=NA") #Respect for elders
auth3.2008<-recode(anes.2008$V085159, "1=1;3=1; 5=2; else=NA") #Good manners
auth4.2008<-recode(anes.2008$V085161, "1=1;3=1; 5=2; else=NA") #Well behaved
###212###
id.2012<-anes.2012$caseid #Identifier
year.2012<-rep(2012, times=(length(id.2012)))
auth1.2012<-recode(anes.2012$auth_obed, "1=2; 2=1; 3=1; else=NA") #Obey Authority
auth2.2012<-recode(anes.2012$auth_ind, "1=1;  2=2; 3=1; else=NA") #Respect for elders
auth3.2012<-recode(anes.2012$auth_cur, "1=1;   2=2; 3=1; else=NA") #Good manners
auth4.2012<-recode(anes.2012$auth_consid, "1=1; 2=2; 3=1; else=NA") #Well behaved

authoritarianism.data<-data.frame(cbind(
  c(id.1990, id.1992, id.1994, id.2000, id.2004, id.2008, id.2012),
  c(year.1990, year.1992, year.1994, year.2000, year.2004, year.2008, year.2012),
  c(auth1.1990, auth1.1992, auth1.1994, auth1.2000, auth1.2004, auth1.2008, auth1.2012),
  c(rep(NA, times=(length(id.1990))), auth2.1992, auth2.1994, auth2.2000, auth2.2004, auth2.2008, auth2.2012),
  c(rep(NA, times=(length(id.1990))), auth3.1992, auth3.1994, auth3.2000, auth3.2004, auth3.2008, auth3.2012),
  c(rep(NA, times=(length(id.1990))), auth4.1992, auth4.1994, auth4.2000, auth4.2004, auth4.2008, auth4.2012)
))
names(authoritarianism.data)<-c("rid", "year", "auth.1", "auth.2", "auth.3", "auth.4")
## Merge the cross sectional data with the authoritarianism data
auth.data<-merge(data.cumulative.anes, authoritarianism.data, by=c("rid", "year"))  ###Merge the authoritarianism data
##I need a term variable to merge with voteview. Use the term prior to the election
#101-1989-1990
#102-1991-1992
#103-1993-1994
#104-1995-1996
#105-1997-1998
#106-1999-2000
#107-2001-2002
#108-2003-2004
#109-2005-2006
#110-2007-2008
#111-2009-2010
#112-2011-2012
auth.data$term<-NA
auth.data$term[auth.data$year==1990]<- 101  
auth.data$term[auth.data$year==1992]<- 102  
auth.data$term[auth.data$year==1994]<- 103  
auth.data$term[auth.data$year==1996]<- 104  
auth.data$term[auth.data$year==1998]<- 105  
auth.data$term[auth.data$year==2000]<- 106
auth.data$term[auth.data$year==2002]<- 107  
auth.data$term[auth.data$year==2004]<- 108  
auth.data$term[auth.data$year==2006]<- 109  
auth.data$term[auth.data$year==2008]<- 110  
auth.data$term[auth.data$year==2010]<- 111  
auth.data$term[auth.data$year==2012]<- 112  

###  Data Description ##
### This is the 2016 data, which I attach to the main data file ###
#### The 2016 data
#require(foreign)
data<-read.dta(paste0(data_location,"/anes_timeseries_2016_Stata12.dta"), convert.factors=FALSE)

rid<-data$V160001

weights.all<-data$V160102
weights.ftf<-data$V160102f
weights.web<-data$V160102w
#data1$rid<-as.numeric(gsub("^3|^4", 0, rid))

#data2<-read.dta13("/Users/chrisweber/Downloads/anes_timeseries_cdf_dta/anes_timeseries_cdf.dta",convert.factors=FALSE)
#data2<-subset(data2, VCF0004==2016)

#write.csv(data1, "/Users/chrisweber/Desktop/delete1.csv")
#write.csv(data2, "/Users/chrisweber/Desktop/delete2.csv")

## Survey Mode
mode<-recode(data$V160501, "1='FTC/CASI'; 2='Internet'; else=NA")
## Religion
protestanta<-recode(data$V161247a, "1=1; 2:4=0; -1=0; else=NA")
catholica<-recode(data$V161247a, "2=1; 1=0; 3:4=0; -1=0; else=NA")
jewisha<-recode(data$V161247a, "3=1; 1:2=0; 4=0; -1=0; else=NA")
othera<-recode(data$V161247a, "4=1; 1:3=0; -1=0; else=NA")

protestantb<-recode(data$V161247b, "1=1; 2:4=0; -1=0; else=NA")
catholicb<-recode(data$V161247b, "2=1; 1=0; 3:4=0; -1=0; else=NA")
jewishb<-recode(data$V161247b, "3=1; 1:2=0; 4=0; -1=0; else=NA")
otherb<-recode(data$V161247b, "4=1; 1:3=0; -1=0; else=NA")

protestant<-ifelse(protestanta==1 | protestantb==1, 1, 0)
catholic<-ifelse(catholica==1 | catholicb==1, 1, 0)
jewish<-ifelse(jewisha==1 | jewishb==1, 1, 0)
other<-ifelse(othera==1 | otherb==1, 1, 0)

### Attend religious service
church<-recode(data$V161245, "-1=0; 5=0; 4=0; 3=0; 2=0; 1=1; -9=0")  ## Attend services at least once per week
###religious orthodoxy.
###religious importance
religious.importance<-recode(data$V161242, "-1=0; 1=1; 2=2; 3=3; else=NA")  ## Religious importance. Don't include this
### Trump Character Traits
### trump traits.
trump.strong<-recode(data$V161164, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.strong<-recode(data$V161159, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
trump.care<-recode(data$V161165, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.care<-recode(data$V161160, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
trump.knowledge<-recode(data$V161166, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.knowledge<-recode(data$V161161, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
trump.honest<-recode(data$V161167, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.honest<-recode(data$V161162, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
trump.speaks<-recode(data$V161168, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.speaks<-recode(data$V161163, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.even<-recode(data$V161169, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") #Even tempered
trump.even<-recode(data$V161170, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
alpha(cbind(trump.strong, trump.care, trump.knowledge, trump.honest, trump.speaks, trump.even))
alpha(cbind(clinton.strong, clinton.care, clinton.knowledge, clinton.honest, clinton.speaks, clinton.even))
trump.character<-(rowMeans(cbind(trump.strong, trump.care, trump.knowledge, trump.honest, trump.speaks, trump.even), na.rm=T)-1)/4
clinton.character<-(rowMeans(cbind(clinton.strong, clinton.care, clinton.knowledge, clinton.honest, clinton.speaks, clinton.even), na.rm=T)-1)/4
#### Income###
income<-recode(data$V161361x, "16:28=1; 1:15=0; else=NA") ## 68 percentile and higher, 55k, source dydqj.com calculator for 2015 income
#### ideology and pidi
pid<-(recode(data$V161158x, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")-1)/6  ### PID 01


### Sophistication Measures



##Efficacy###
#.t is turnout

voted<-recode(data$V162031x, "1=1; 0=0; else=NA") #Did Respondent Vote in the National Elections
#vote.house<-recode(data$V162039, "1=1; 2=0; else=NA") # voted house
#vote.senate.t<-recode(data$V162046, "1=0; 2=1; else=NA") # voted senate -- careful, some didn't have races

##Vote direction in house and senate -- need to manually construct the split ballot measure
vote.house<-recode(data$V162068x, "1=0; 2=1; else=NA") # Rep vote
vote.senate<-recode(data$V162067x, "1=0; 2=1; else=NA")
vote<-recode(data$V162034a, "2=1; 1=0; else=NA") ### Two party vote, DT=1 

split.house<-rep(NA, length(vote.senate))
split.house[vote.house==1 & vote==1]<-'RP-RC'
split.house[vote.house==0 & vote==1]<-'RP-DC'
split.house[vote.house==0 & vote==0]<-'DP-DC'
split.house[vote.house==1 & vote==0]<-'DP-RC'
#split.house2<-recode(split.house, "1=0; 2=1; 3=0; 4=1; else=NA") # Split Ballot

split.senate<-rep(NA, length(vote.senate))
split.senate[vote.senate==1 & vote==1]<-'RP-RC'
split.senate[vote.senate==0 & vote==1]<-'RP-DC'
split.senate[vote.senate==0 & vote==0]<-'DP-DC'
split.senate[vote.senate==1 & vote==0]<-'DP-RC'

#split.senate2<-recode(split.senate, "1=0; 2=1; 3=0; 4=1; else=NA") # Split Ballot

###
#efficacy2<-recode(anes$VCF0717, "1=1; 2=0; else=NA") #Respondent Try to Influence the Vote of Others During the Campaign
p1<-recode(data$V162011, "1=1; 2=0; else=NA") #Attend rally
p2<-recode(data$V162013, "1=1; 2=0; else=NA") #Work for candidate
p3<-recode(data$V162012, "1=1; 2=0; else=NA") #Button
p4a<-recode(data$V162014, "1=1; 2=0; else=NA") #Donate money
p4b<-recode(data$V162016, "1=1; 2=0; else=NA") #Donate money
p4<-ifelse(p4a==1 | p4b==1, 1, 0)
p5<-recode(data$V162010, "1=1; 2=0; else=NA") #Button
#efficacy7<-recode(anes$VCF0731, "1=2; 5=0; else=NA") #Poliitcal discussion


### Code Efficacy 1 -7
efficacy1<-recode(data$V161220, "1=3; 2=2; 3=1; else=NA") #How Much Elections Make Government Pay Attention to People

efficacy2<-recode(data$V162215, "1=1; 2=1; 3=2; 4=3; 5=3; else=NA") #People care about what I think

efficacy3<-recode(data$V161217, "1=1;2=2; 3=3; else=NA") #Government does not waste money

efficacy4<-recode(data$V161216, "1=1;2=2; else=NA") #Government run for benefit of all

efficacy5<-recode(data$V161215, "1=4; 2=4; 3=3; 4=2; 5=1; else=NA") #Trust government to do what is right

efficacy6<-recode(data$V162216, "1=1; 2=2; 3=2; 4=3; 5=3; else=NA") #People like me have a say

efficacy7<-recode(data$V162217, "1=1; 2=1; 3=2; 4=3; 5=3; else=NA") #government is not too complicated



#FTF only
know.interview.pre<-recode(data$V168016, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
know.interview.post<-recode(data$V168112, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")


interest.elections<-recode(data$V161004, "1=3; 2=2; 3=1; else=NA") #Interest in campaign
interest.politics<-recode(data$V161145, "1=1; 2:5=0; -8=0;  else=NA") #Interest in party winner


##Salient Issues
#issue.morality<-recode(anes$VCF0875, "7=1; 1:6=0; 8:97=0; else=NA") # Vote Republican
## These aren't the branched graded questions
protect.gays<-recode(data$V161229, "1=0; 2=1; else=NA") # Do you favor or oppose laws to protect homosexuals against job discrimination?
gays.military<-NA
gays.adoption<-recode(data$V161230, "1=0; 2=1; else=NA") # Gay Adoption

##Immigration
immigrants<-recode(data$V161192, "1=4; 2=3; 3=2; 4=1; else=NA") # Decrease immigration

### moral traditionalism.
moral1<-recode(data$V162207, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## We should adjust, Reversed
moral2<-recode(data$V162210, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")   #Should be More Emphasis on Traditional Values
moral3<-recode(data$V162209, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  #Tolerance of Different Moral Standards
moral4<-recode(anes$V162208, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  #Newer lifestyles contributing to societal breakeown

alpha(cbind(moral1, moral2, moral3))
moral.traditionalism<-(rowMeans(cbind(moral1, moral2, moral3), na.rm=T)-1)/4



### Racial Resentment
rr1<-recode(data$V162211, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (Irish)
rr2<-recode(data$V162212, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Generations of slavery
rr3<-recode(data$V162213, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less
rr4<-recode(data$V162214, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder
### Interest
interest<-(recode(data$V161003, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")-1)/4 ### Interest in politics
###vote Trump.
vote<-recode(data$V162034a, "2=1; 1=0; else=NA") ### Two party vote
vote.primary<-recode(data$V161021a, "1='AHC'; 2='BS'; 4='DT'; 5='TC';
                      6='JK'; 7='MR'; 3='Other'; 8='Other'; 9='Other'; else=NA") ### Primary Vote
#income<- (recode(data$V161361x, "-9=NA; -5=NA")-1)/27

###voted Obama
vote.obama<-recode(data$V161006, "1=1; 2=0; else=NA") ### Two party vote
## Authoritarianism -- Numbering matches the code above
auth.1<-recode(data$V162241, "1=2; 2=1; 3=1; else=NA") ### Respect
auth.2<-recode(data$V162239, "1=1; 2=2; 3=1; else=NA") ### Child repsect versus indepdnence
auth.3<-recode(data$V162240, "1=1; 2=2; 3=1; else=NA") ### Manners
auth.4<-recode(data$V162242, "1=1; 2=2; 3=1; else=NA") ### Behaved
psych::alpha(cbind(auth.1, auth.2, auth.3, auth.4))
authoritarianism<-(rowMeans(cbind(auth.1, auth.2, auth.3, auth.4), na.rm=T)-1)/2
###hostile sexism.
hostile1<-recode(data$V161507, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Inerpret everything as sexist
hostile2<-recode(data$V161508, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Consider what men do.
hostile3<-recode(data$V161509, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Gain power through feminism
hostile4<-recode(data$V161510, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Put on tight leash
alpha(cbind(hostile1, hostile2, hostile3, hostile4))
hostile<-(rowMeans(cbind(hostile1, hostile2, hostile3, hostile4), na.rm=T)-1)/4


### Demographics
### I can't locate census region
age<-recode(data$V161267, "-9=NA; -8=NA")
college<-recode(data$V161270, "13:16=1; 1:12=0; else=NA")  ## College degree
media<-recode(data$V162002, "1=0; 2:4=1; else=NA")  ## Watch tv about campaign, one or two, good many, 3 or 4.
white<-recode(data$V161310x, "1=1; 2:6=0; else=NA")
nonwhite<-recode(data$V161310x, "1=0; 2:6=1; else=NA")
black<-recode(data$V161310x, "2=1; 1=0; 3:6=0; else=NA")
hispanic<-recode(data$V161310x, "5=1; 1:4=0; 6=0; else=NA")
other.race<-recode(data$V161310x, "3:4=1; 1:2=0; 5=0; 6=1; else=NA")


#other<- recode(data$V161310x, "3=1; 4=1; 6=1; 1=0; 2=1; 5=1; else=NA")
#income<- (recode(data$V161361x, "-9=NA; -5=NA")-1)/27
female<-recode(data$V161342, "1=0; 2=1; else=NA")
####Feeling Therms ####
feeling.demc<-recode(data$V161086, "-98=NA; -99=NA; -88=NA")  #Feeling Dems Candidate
feeling.repc<-recode(data$V161087, "-98=NA; -99=NA; -88=NA")  #Feeling Reps Candidate
feeling.dem<-recode(data$V161095, "-98=NA; -99=NA; -88=NA; -89=NA")  #Feeling Dems
feeling.rep<-recode(data$V161096, "-98=NA; -99=NA; -88=NA; -89=NA")  #Feeling Reps

feel.black<-ifelse(data$V162312 < 0, NA, data$V162312)  
feel.latino<-ifelse(data$V162311 < 0, NA, data$V162311) 
feel.asian<-ifelse(data$V162310 <0, NA, data$V162310)  




#####
primary<-recode(data$V161021, "1=1; 2=0; else=NA")


####Policy Items####
women.role<-NA
women.roleCD<-NA
women.roleCR<-NA
aid.blacksD<-NA
aid.blacksR<-NA
gov.servicesR<-NA
gov.servicesD<-NA
jobsR<-NA
jobsD<-NA
defense.spendingD<-NA
defense.spendingR<-NA

jobs<-recode(data$V161189, "-9=NA; -8=NA; 99=NA") # Government should see to jobs and standard of living, conservative direction
jobsCD<-recode(data$V161190, "-9=NA; -8=NA; 99=NA") #Government should see to jobs and standard of living, conservative direction
jobsCR<-recode(data$V161191, "-9=NA; -8=NA; 99=NA") #Government should see to jobs and standard of living, conservative direction
public.insurance<-recode(data$V161184, "-9=NA; -8=NA; 99=NA") #Private insurance plan
public.insuranceCD<-recode(data$V161185, "-9=NA; -8=NA; 99=NA") #Private insurance plan
public.insuranceCR<-recode(data$V161186, "-9=NA; -8=NA; 99=NA") #Private insurance plan
aid.blacks<-recode(data$V161198, "-9=NA; -8=NA; 99=NA") #government should not help blacks
aid.blacksCD<-recode(data$V161199, "-9=NA; -8=NA; 99=NA") #government should not help blacks
aid.blacksCR<-recode(data$V161200, "-9=NA; -8=NA; 99=NA") #government should not help blacks
gov.services<-recode(data$V161178, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else =NA") #government should provide fewer services  
gov.servicesCD<-recode(data$V161179, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 0=NA; 99=NA; -9=NA; -8=NA") #government should provide fewer services  
gov.servicesCR<-recode(data$V161180, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 0=NA; 99=NA; -9=NA; -8=NA") #government should provide fewer services  
defense.spending<-recode(data$V161181, "0=NA; 99=NA; -9=NA; -8=NA") #government should provide more defense spending
defense.spendingCD<-recode(data$V161182, "0=NA; 99=NA; -9=NA; -8=NA") #government should provide more defense spending
defense.spendingCR<-recode(data$V161183, "0=NA; 99=NA; -9=NA; -8=NA") #government should provide more defense spending
ideology<-recode(data$V161126, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyCD<-recode(data$V161128, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyCR<-recode(data$V161129, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyD<-recode(data$V161130, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyR<-recode(data$V161131, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")

#women.role<-recode(as.numeric(anes$VCF0834), "0=NA; 9=NA") #women's role in home
abortion<-recode(data$V161232, "1=4; 2=3; 3=2; 4=1; -9=NA; -8=NA; 5=NA") #Ban abortion
gays.discrimination<-recode(as.numeric(data$V161229), "1=0; 2=1; else=NA") #Gays Jobs




###Better or worse off than year ago#####
personal.situation<-recode(data$V161110, "-9=NA; -8=NA") #Am worse off than year ago
personal.fsituation<-recode(data$V161111, "-9=NA; -8=NA") #Am worse off than year ago
large.gap<-recode(data$V161138x, "-1=NA; 1=5; 2=4; 3=3; 4=2; 5=1") #Larger income gap toeay
large.unemployment<-recode(data$V161138x, "-1=NA") #Larger unemployment


child<-recode(data$V161324, "-9=NA") #Number of Children
child.1plus<-recode(data$V161324, "0=0; -9=NA; 1:9=1") #at least one




###Non cumulative policy items#####
aca<-recode(data$V161114x, "-1=NA") #Oppose ACA
#immigrants<-recode(data$V161192, "-8=NA; -9=NA; 1=4; 2=3; 3=2; 4=1") #Make immigrants felons and send home
birthright<-recode(data$V161194x, "-8=NA; -9=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") #End Birthright citizenship
dreamers<-recode(data$V161194x, "-8=NA; -9=NA; 1=6; 2=5; 3=4; 4=3; 5=2; 6=1") #Send Children back
wall<-recode(data$V161194x, "-8=NA; -9=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") #Favor a wall

affirmatve.action<-recode(data$V161204x, "-8=NA; -9=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") #favor affirmative action

### Spending Budget Battery ####
social.security<-recode(data$V161205, "-8=NA; -9=NA; 2=3; 3=2") #Reduce spending
public.schools<-recode(data$V161206, "-8=NA; -9=NA; 2=3; 3=2") #Reduce spending
science<-recode(data$V161207, "-8=NA; -9=NA; 2=3; 3=2") #Reduce spending
crime<-recode(data$V161208, "-8=NA; -9=NA; 2=3; 3=2") #Reduce spending
welfare<-recode(data$V161209, "-8=NA; -9=NA; 2=3; 3=2") #Welfare
child.care<-recode(data$V161210, "-8=NA; -9=NA; 2=3; 3=2") #Reduce spending
aid.poor<-recode(data$V161211, "-8=NA; -9=NA; 2=3; 3=2") #Reduce spending
environment<-recode(data$V161212, "-8=NA; -9=NA; 2=3; 3=2") #Reduce spending
isis<-recode(data$V161213x, "-8=NA; -9=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") #Favor troops to fight isis
syrian.refugees<-recode(data$V161213x, "-8=NA; -9=NA") #Oppose taking in Syrian refugees



global1<-recode(data$V161221, "-8=NA; -9=NA") #Global warming hasn't been happening
global2<-recode(data$V161222, "-8=NA; -9=NA;2=3; 3=2") #Anthropengenic climate change
fracking<-recode(data$V161222, "-8=NA; -9=NA;1=3; 2=1; 3=2") #Support Fracking
global3<-recode(data$V161225x, "-8=NA; -9=NA") #Do less about climate change

paid.leave<-recode(data$V161226x, "-8=NA; -9=NA") #Oppose paid leave

gay.services<-recode(data$V161227x, "-8=NA; -9=NA; 1=6; 2=5; 3=4; 4=3; 5=2; 6=1") #Oppose services to same sex couples
transgender.bathroom<-recode(data$V161228x, "-8=NA; -9=NA; 1=6; 2=5; 3=4; 4=3; 5=2; 6=1") #Bathroom of biological sex
gays.discrimination<-recode(data$V161229x, "-8=NA; -9=NA; -1=NA") #Oppose protection of homosexual couples

gay.marriage<-recode(data$V161231, "-8=NA; -9=NA") #Oppose gay marriage
abortion<-recode(data$V161232, "-8=NA; -9=NA; 1=4; 2=3; 3=2; 4=1; 5=NA") #Oppose abortion
death.penalty<-recode(data$V161233x, "-8=NA; -9=NA;-1=NA; 1=4; 2=3; 3=2; 4=1; 5=NA") #Favor capital punishment

torture<-recode(data$V162295x, "-8=NA; -9=NA; -6=NA; -4=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") #Favor torture
### Patriotism and Nationalism ###
patriotism<-recode(data$V162125x, "-8=NA; -9=NA; -7=NA; -6=NA;-7=NA;  1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") #Flag makes feel gooed


### anti-egalitarianism.
egal1<-recode(data$V162243, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Unequal opprotunity
egal2<-recode(data$V162244, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Worry less about eqwuality
egal3<-recode(data$V162245, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Tradition
egal4<-recode(data$V162246, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Fewer propnblems if fair treat
psych::alpha(cbind(egal1, egal2, egal3, egal4))
anti.egalitarianism<-(rowMeans(cbind(egal1, egal2, egal3, egal4), na.rm=T)-1)/4
### knwoeldge
k1<-recode(data$V161513, "6=1; else=0")  ## Senate term
k2<-recode(data$V161515, "2=1; else=0")  ## Party with most members in house
k3<-recode(data$V161516, "2=1; else=0")  ## Party with most members in senate
k4<-recode(data$V162072, "1=1; 0=0; else=NA")  ## VP
k5<-recode(data$V162073a, "1=1; 0=0; else=NA")  ## Speaker of house
k6<-recode(data$V162074a, "1=1; 0=0; else=NA")  ## Angela merkel
k7<-recode(data$V162075a, "1=1; 0=0; else=NA")  ## Putin
k8<-recode(data$V162076a, "1=1; 0=0; else=NA")  ## CHief justice
psych::alpha(data.frame( k1, k2, k3, k4, k5, k6, k7, k8))
#knowledge<-rowMeans(cbind(k1, k2, k3, k4, k5, k6, k7, k8), na.rm=T)

### Feeling Thermometer
gay.therm<-recode(data$V162103, "-9=NA; -7=NA; -6=NA; 998=NA; 999=NA")
feminists.therm<-recode(data$V162096, "-9=NA; -7=NA; -6=NA; 998=NA; 999=NA")
fundamental.therm<-recode(data$V162095, "-9=NA; -7=NA; -6=NA; 998=NA; 999=NA")
union.therm<-recode(data$V162098, "-9=NA; -7=NA; -6=NA; 998=NA; 999=NA")
####
anger.dem<-recode(data$V161116, "1=0; 2:5=1; else=NA")
fear.dem<-recode(data$V161118, "1=0; 2:5=1; else=NA")
hope.dem<-recode(data$V161117, "1=0; 2:5=1; else=NA")
proud.dem<-recode(data$V161119, "1=0; 2:5=1; else=NA")
anger.rep<-recode(data$V161121, "1=0; 2:5=1; else=NA")
fear.rep<-recode(data$V161123, "1=0; 2:5=1; else=NA")
hope.rep<-recode(data$V161122, "1=0; 2:5=1; else=NA")
proud.rep<-recode(data$V161124, "1=0; 2:5=1; else=NA")


### White identity
white.identity<-(recode(data$V162327, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")-1)/4  ## White identity
black.identity<-(recode(data$V162328, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")-1)/4  ## Black identity
data$identity<-NA
data$identity[white==1 & !is.na(white.identity) ]<-white.identity[white==1 & !is.na(white.identity)]
data$identity[black==1 & !is.na(black.identity) ]<-black.identity[black==1 & !is.na(black.identity)]
identity<-data$identity


strong.gov<-recode(data$V161124, "1=0; 2=1; else=NA")

bible<-recode(data$V161243, "1=1; 2=2; 3=3; else=NA")

# 
# data<-data.frame(mode, protestant, catholic, jewish, other, ideology, pid,
#                  church.attendance, bible, religious.importance,
#                  trump.strong, trump.care, trump.knowledge, trump.honest,
#                  trump.speaks, rr1, rr2, rr3, rr4, interest, vote.trump,
#                  vote.obama, child.respect, child.manners, child.obedience,
#                  child.behaved, hostile1, hostile2, hostile3, hostile4,
#                  age, college, white, black, hispanic, other, income,
#                  female, moral1, moral2, moral3, moral4, egal1, egal2,
#                  egal3, egal4, k1, k2, k3, k4, k5, k6, k7, k8,
#                  black.identity, white.identity,
#                  knowledge, anti.egalitarianism, moral.traditionalism, authoritarianism,
#                 racial.resentment, hostile, identity, nonwhite)

###  Data Description ##

#### traits

data$dem.leader<-recode(data$V161159, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.know<-recode(data$V161161,   "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.cares<-recode(data$V161160,  "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.decent<-recode(data$V161162, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA" )
data$rep.leader<-recode(data$V161164, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.know<-recode(data$V161166,   "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.cares<-recode(data$V161165,  "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.decent<-recode(data$V161167, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA" ) ## honest

data$dem.angry<-recode(data$V161116,   "1:2=1; 2:5=2;else=NA") 
data$dem.afraid<-recode(data$V161117,  "1:2=1; 2:5=2;else=NA") 
data$dem.hope<-recode(data$V161118,    "1:2=1; 2:5=2;else=NA") 
data$dem.proud<-recode(data$V161119,   "1:2=1; 2:5=2;else=NA") 

data$rep.angry<-recode(data$V161121,  "1:2=1; 2:5=2;else=NA") 
data$rep.afraid<-recode(data$V161122, "1:2=1; 2:5=2;else=NA") 
data$rep.hope<-recode(data$V161123,   "1:2=1; 2:5=2;else=NA") 
data$rep.proud<-recode(data$V161124,  "1:2=1; 2:5=2;else=NA") 

out = paste0(c("dem."), c("decent", 
                          "know", "leader", 
                          "cares", "angry", 
                          "afraid", "hope", 
                          "pride"
))
dems =   data[, names(data) %in% out]

out = paste0(c("rep."), c("decent", 
                          "know", "leader", 
                          "cares", "angry", 
                          "afraid", "hope", 
                          "pride"
))
reps =   data[, names(data) %in% out]




#### Data save and merge

data<-data.frame(age, female, college, church, catholic, jewish, bible,
                 pid, media, feeling.dem, feeling.rep, feeling.demc, feeling.repc,
                 abortion,
                 vote, auth.1, auth.2, auth.3, auth.4,
                 rr1, rr2, rr3, rr4, white, nonwhite, black, hispanic, other.race, primary, income,
                 split.house, split.senate, vote.house,
                 vote.senate, 
                 moral1, moral2, moral3, moral4, voted,
                 interest.elections, interest.politics,
                 know.interview.pre, know.interview.post,
                 p1, p2, p3, p4, p5, efficacy1, efficacy2, efficacy3,
                 efficacy4, efficacy5, efficacy6, efficacy7, 
                 rid, other,
                 public.insurance, public.insuranceCD, public.insuranceCR,
                 women.role, women.roleCD, women.roleCR,
                 aid.blacksD, aid.blacksR, aid.blacksCD, aid.blacksCR,
                 gov.services, gov.servicesCD, gov.servicesCR,
                 gov.servicesR, gov.servicesD, jobs, jobsCR, jobsCD,
                 jobsR, jobsD, defense.spendingCD, defense.spendingCR,
                 defense.spendingD, defense.spendingR, ideology, ideologyR, 
                 ideologyD, ideologyCD, ideologyCR,  egal1, egal2, egal3, egal4,
                 mode, gay.therm, feminists.therm, fundamental.therm,
                 protect.gays, gays.military, gays.adoption, immigrants, union.therm,
                 weights.all, weights.ftf, weights.web,
                 anger.dem, fear.dem, hope.dem, proud.dem,
                 anger.rep, fear.rep, hope.rep, proud.rep,
                 feel.black, feel.latino, feel.asian, aid.blacks, reps, dems)
data$year<-2016 
data_2016 <- data



#### Need to check data, affirmagive action seems to be missing, recode FT

###  Data Description ##
## FTF is infeasible in 2016. I just use all the data. Recall we truncated the rest to be face-to-face
data<-read.dta13(paste0(data_location,"/t2020anes.dta"), convert.factors = TRUE)
data$case_id = data$V160001_orig
protestant<-recode(data$V201435, "1=1; 2:12=0; else=NA")
catholic<-recode(data$V201435, "2=1; 1=0; 3:12=0; else=NA")
jewish<-recode(data$V201435, "5=1; 1:4=0; 6:12=0; else=NA")
other<-recode(data$V201435, "11:12=1; 1:10=0; else=NA")
church<-recode(data$V201453, "1=1; 2:5=0; else=NA") #Attend church at least once per week
bible<-recode(data$V201434, "1=3; 2=2; 3=1; else=NA")
income<-rep(NA, length(bible))
income<-recode(data$V202468x, "11:22=1; 1:10=0; else=NA") 
pid<-(recode(data$V201231x, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")-1)/6
primary<-recode(data$V201020, "1=1; 2=0; else=NA")
religious.importance<-recode(data$V201433, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
female<-recode(data$V201600, "1=0; 2=1; else=NA") #
white<-ifelse(data$V201549x==1, 1, ifelse(data$V201549x!=1 & data$V201549x>0, 0, NA))
nonwhite<-abs(white-1)
black<-ifelse(data$V201549x==2, 1, ifelse(data$V201549x!=2 & data$V201549x>0, 0, NA))
hispanic<-ifelse(data$V201549x==3, 1, ifelse(data$V201549x!=3 & data$V201549x>0, 0, NA))
asian<-ifelse(data$V201549x==4, 1, ifelse(data$V201549x!=4 & data$V201549x>0, 0, NA))
other.race<-ifelse(black==0 & white==0 & hispanic==0 & asian == 0, 1, 0)
age<-data$V201507x
college<-recode(data$V201511x, "4:5 =1; 1:3 = 0; else= NA" )
media<-recode(data$V201629a, "1=0; 2=1; else=NA")  ## Watch tv about campaign, one or two, good many, 3 or 4.
rid = seq(1:length(media))
year = 2020
feeling.demc<-recode(data$V201151, "-9=NA; 998=NA; -999=NA; -4=NA")  #Feeling Dems Candidate
feeling.repc<-recode(data$V201152, "-9=NA; 998=NA; -999=NA; -4=NA")  #Feeling Rep Candidate
feeling.dem<-recode(data$V201156, "-9=NA; 998=NA; -999=NA; -4=NA")   #Feeling Dems
feeling.rep<-recode(data$V201157, "-9=NA; 998=NA; -999=NA;-4=NA")    #Feeling Reps



abortion<-recode(data$V201336, "1=4; 2=3; 3=2; 4=1; -9=NA; -8=NA; 5=NA") #Ban abortion
gays.discrimination<-recode(as.numeric(data$V201414x), "1:2=0; 3:4=1; else=NA") #Gays Jobs
voted<-recode(data$V202073, "2=1; 1=0; else=NA") #Did Respondent vote for president
### Racial Resentment
rr1<-recode(data$V202300, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (IRish)
rr2<-recode(data$V202301, "1=1; 2=2; 3=3; 4=4; 5=5; 8=NA; 9=NA; else=NA") ### Generations of slavery
rr3<-recode(data$V202302, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less than they deserve
rr4<-recode(data$V202303, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder

vote<-recode(data$V202105x, "11=1; 10=0;else=NA")
vote.house<-recode(data$V202106x, "11=1; 10=0;else=NA")
vote.senate<-recode(data$V202107x, "11=1; 10=0;else=NA")
split.house<-ifelse(vote != vote.house, 1, 0)
split.senate<-ifelse(vote != vote.senate, 1, 0)
efficacy1<-recode(data$V201005, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") #How Much Elections Make Government Pay Attention to People
efficacy2<-recode(data$V202212, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") #People care about what I think
efficacy3<-recode(data$V201235, "1=1;2=2; 3=3; else=NA") #Government does not waste money
efficacy4<-recode(data$V201234, "1=1;2=2; else=NA") #Government run for benefit of all
efficacy5<-recode(data$V201233, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") #Trust government to do what is right
efficacy6<-recode(data$V202213, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") #People like me have a say
efficacy7<-recode(data$V202214, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") #government is not too complicated
p1<-recode(data$V202014, "1=1; 2=0; else=NA") #Attend rally
p2<-recode(data$V202016, "1=1; 2=0; else=NA") #Work for candidate
p3<-recode(data$V202015, "1=1; 2=0; else=NA") #Button
p4<-recode(data$V202017, "1=1; 2=0; else=NA") #Donate money
p5<-recode(data$V202022, "1=1; 2=0; else=NA") #INfluence others vote



moral1<-recode(data$V202264, "1=1; 2=2; 3=3; 4=4; 5=5 ;else=NA")  #Should Adjust View of Moral Behavior to Changes
moral2<-recode(data$V202265, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")   #Should be More Emphasis on Traditional Values
moral3<-NA
moral4<-NA
#moral3<- rep(NA, length(moral1)) ## Can't locate these questions
#moral4<- rep(NA, length(moral1))
interest.elections<-recode(data$V201006, "1=3; 2=2; 3=1; else=NA") #Interest in campaign
interest.politics<-recode(data$V202407, "1=4; 2=3; 3=2; 4=1;  else=NA") #Interest in party winner
## Can't locate these:
know.interview.pre<-rep(NA, length(moral1))
know.interview.post<-rep(NA, length(moral1))
jobs<-recode(data$V201255, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #Government should see to jobs and standard of living, conservative direction
jobsCD<-recode(data$V201256, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #Government should see to jobs and standard of living, conservative direction
jobsCR<-recode(data$V201257, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #Government should see to jobs and standard of living, conservative direction
public.insurance<-recode(data$V201252, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #Private insurance plan
public.insuranceCD<-recode(data$V201253, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #Private insurance plan
public.insuranceCR<-recode(data$V201254, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #Private insurance plan
aid.blacks<-recode(data$V201258, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") 
aid.blacksCD<-recode(data$V201259, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
aid.blacksCR<-recode(data$V201260, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #

gov.services<-recode(data$V201246, "1=7; 2=6; 3=4; 4=4; 5=3; 6=2; 7=1; else=NA") #

gov.servicesCD<-recode(data$V201247, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
gov.servicesCR<-recode(data$V201248, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
defense.spending<-recode(data$V201249, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
defense.spendingCD<-recode(data$V201250, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
defense.spendingCR<-recode(data$V201251, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
ideology<-recode(data$V201200, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyCD<-recode(data$V201202, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyCR<-recode(data$V201203, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyD<-recode(data$V201206, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyR<-recode(data$V201207, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
women.role<-rep(NA, length(moral1))
women.roleCD<-rep(NA, length(moral1))
women.roleCR<-rep(NA, length(moral1))
## Anti-Egalitarianism
egal1<-recode(data$V202260, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Unequal opprotunity
egal2<-recode(data$V202261, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Worry less about eqwuality
egal3<-recode(data$V202262, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## 
egal4<-recode(data$V202263, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Fewer propnblems if fair treat
mode <- data$V200002
gay.therm<-recode(data$V202166, "-9:1=NA; 998=NA; 999=NA")
feminists.therm<-recode(data$V202160, "-9:1=NA; 998=NA; 999=NA")
union.therm<-recode(data$V202162, "-9:1=NA; 998=NA; 999=NA")
fundamental.therm<-recode(data$V202159, "-9:1=NA; 998=NA; 999=NA")
immigrants<-recode(data$V202232, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # Decrease immigration
protect.gays<-recode(data$V201414x, "1:2=0; 3:4=1; else=NA") # Do you favor or oppose laws to protect homosexuals against job discrimination?
gays.military<-recode(data$V202388, "1=0; 3=0; 2=1; else=NA") # Trans military
gays.adoption<-recode(data$V201415, "1=0; 2=1; else=NA") # Gay Adoption
weights.all  <- rep(NA, length(mode))                              
weights.ftf  <- rep(NA, length(mode))                              
weights.web  <- rep(NA, length(mode))                              

#### Repalce these.
anger.dem<-rep(NA, length(mode))  
fear.dem<- rep(NA, length(mode))  
hope.dem<- rep(NA, length(mode))  
proud.dem<-rep(NA, length(mode))  
anger.rep<-rep(NA, length(mode))  
fear.rep<- rep(NA, length(mode))  
hope.rep<- rep(NA, length(mode))  
proud.rep<-rep(NA, length(mode))  
## Authoritarianism -- Numbering matches the code above
auth.2<-recode(data$V202266, "1=1; 2=2; 3=1; else=NA") ### Child repsect versus indepdnence
auth.3<-recode(data$V202267, "1=1; 2=2; 3=1; else=NA") ### Manners
auth.1<-recode(data$V202268, "1=2; 2=1; 3=1; else=NA") ### Respect
auth.4<-recode(data$V202269, "1=1; 2=2; 3=1; else=NA") ### Behaved


aid.blacksD <- rep(NA, length(mode)) 
aid.blacksR <- rep(NA, length(mode)) 
gov.servicesD <- rep(NA, length(mode)) 
gov.servicesR <- rep(NA, length(mode)) 
jobsD <- rep(NA, length(mode)) 
jobsR <- rep(NA, length(mode)) 
defense.spendingD <- rep(NA, length(mode)) 
defense.spendingR <- rep(NA, length(mode)) 
sample_type = data$V200003
## year indicator ##

feel.black = ifelse(data$V202480  < 0, NA, data$V202480)
feel.latino = ifelse(data$V202479 < 0, NA, data$V202479)
feel.asian = ifelse(data$V202478  < 0, NA, data$V202478)

aid.blacks<-recode(data$V201260, "0=NA; -8=NA; -9=NA") #Government living, Dems


data$dem.leader<-recode(data$V201208, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.know<-recode(data$V201210,   "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.cares<-recode(data$V201209,  "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.decent<-recode(data$V201211,"1=4; 2=3; 3=2; 4=1; 5=1; else=NA") 


data$rep.leader<-recode(data$V201212, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.know<-recode(data$V201214,   "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.cares<-recode(data$V201213,  "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.decent<-recode(data$V201215, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA") ## honest

data$dem.angry<-NA
data$dem.afraid<-NA
data$dem.hope<-NA
data$dem.proud<-NA

data$rep.angry<-NA
data$rep.afraid<-NA
data$rep.hope<-NA
data$rep.proud<-NA

out = paste0(c("dem."), c("decent", 
                          "know", "leader", 
                          "cares", "angry", 
                          "afraid", "hope", 
                          "pride"
))
dems =   data[, names(data) %in% out]

out = paste0(c("rep."), c("decent", 
                          "know", "leader", 
                          "cares", "angry", 
                          "afraid", "hope", 
                          "pride"
))
reps =   data[, names(data) %in% out]



data<-data.frame(age, female, college, church, catholic, jewish, bible,
                 pid, media, feeling.dem, feeling.rep, feeling.demc, feeling.repc,
                 abortion,
                 vote, auth.1, auth.2, auth.3, auth.4,
                 rr1, rr2, rr3, rr4, white, nonwhite, black, hispanic, other.race, primary, income,
                 split.house, split.senate, vote.house,
                 vote.senate, 
                 moral1, moral2, moral3, moral4, voted,
                 interest.elections, interest.politics,
                 know.interview.pre, know.interview.post,
                 p1, p2, p3, p4, p5, efficacy1, efficacy2, efficacy3,
                 efficacy4, efficacy5, efficacy6, efficacy7, 
                 rid, other,
                 public.insurance, public.insuranceCD, public.insuranceCR,
                 women.role, women.roleCD, women.roleCR, aid.blacks,
                 aid.blacksD, aid.blacksR, aid.blacksCD, aid.blacksCR,
                 gov.services, gov.servicesCD, gov.servicesCR,
                 gov.servicesR, gov.servicesD, jobs, jobsCR, jobsCD,
                 jobsR, jobsD, defense.spendingCD, defense.spendingCR,
                 defense.spendingD, defense.spendingR, ideology, ideologyR, 
                 ideologyD, ideologyCD, ideologyCR,  egal1, egal2, egal3, egal4,
                 mode, gay.therm, feminists.therm, fundamental.therm,
                 protect.gays, gays.military, gays.adoption, immigrants, union.therm,
                 weights.all, weights.ftf, weights.web,
                 anger.dem, fear.dem, hope.dem, proud.dem,
                 anger.rep, fear.rep, hope.rep, proud.rep, sample_type,
                 feel.black, feel.latino, feel.asian, dems, reps)
data <- subset(data, sample_type!=2)
data$year<-2020   
data_2020 <- data

auth.data<-auth.data[,!(names(auth.data) %in% c("CD", "STATE", "term"))]



### Alphabetical sort and merge!
names(auth.data)  
d1<-auth.data[ ,order(names(auth.data))]
d2<-data_2016[ ,order(names(data_2016))]
d3<-data_2020[ ,order(names(data_2020))]
d3 = d3[,!(names(d3) %in% "sample_type")]
data<-rbind(d1,d2, d3)
#Data check
for(i in 1:dim(data)[2]){
 print(i)
 print(table(data[,i]))
}

# Save data
data$rid2<-seq(1:nrow(data))
##### Measures ######

save(data, file= "pooled.auth.Rdata")

write.dta(data, file="pooled.ANES.dta")


write.csv(data, file="pooled.ANES.csv")

######


## Other race
##________________________________________________________________________##
# Data Description 
# take a subset of items to create a spatial and operational data dataset
##________________________________________________________________________##
# Opertaional ideology
# ## This is analyze two dimensional ideology ###
 operational.data<-subset(data, select=c(abortion, aid.blacks, gays.adoption,
                                         gov.services, immigrants,
                                         public.insurance,
                                         jobs, rid, year, pid, ideology,
                                         white, black, hispanic, auth.1, auth.2, auth.3, auth.4, 
                                         mode, gay.therm, feminists.therm, fundamental.therm, union.therm,
                                         moral1, moral2, moral3, moral4, rid2))

 ##This is orientation towards parties, spatial.
spatial.model<-subset(data, select=c(public.insurance, public.insuranceCD, public.insuranceCR,
                                          women.role, women.roleCD, women.roleCR, aid.blacks,
                                          aid.blacksD, aid.blacksR, aid.blacksCD, aid.blacksCR,
                                          gov.services, gov.servicesCD, gov.servicesCR,
                                          gov.servicesR, gov.servicesD, jobs, jobsCR, jobsCD,
                                          jobsR, jobsD, defense.spendingCD, defense.spendingCR,
                                          defense.spendingD, defense.spendingR, ideology, ideologyR, 
                                          ideologyD, ideologyCD, ideologyCR, rid, year,
                                          auth.1, auth.2, auth.3, auth.4, white, pid,
                                          female, age, college, bible,
                                          church, income, 
                                          jewish, catholic,  other, mode, vote,
                                          feeling.dem, feeling.rep, feeling.demc, feeling.repc, know.interview.pre,
                                          know.interview.post, rid2))

for(i in 1:dim(spatial.model)[2]){
  print(i)
  print(table(spatial.model[,i]))
  print(sum(table(spatial.model[,i])))
}
## For Operational and spatial Ideology Measure ###
save(operational.data, file="operational.auth.Rdata")
save(spatial.model, file="spatial.Rdata")
# This is the full cross section ##
#save(data, file="/Users/chrisweber/Google Drive/Project Folder/projects/Authoritarianism_Sorting/Data/pooled.auth.Rdata")
##########################################################################################
##### Code the constituent 2016 and 2012 Cross Sections -- to compare RWA/authoritarianism, etc #####
####  This is to accompany Chapter 1 #####
####-------------------------------------------------------------------------------------####
####-------------------------------------------------------------------------------------####
rm(list=ls())
require(foreign)
require(car)
require(psych)
####-------------------------------------------------------------------------------------####
### require(foreign)
data<-read.dta("data/anes_timeseries_2016_Stata12.dta",convert.factors=FALSE)
rid<-data$V160001
## Right Wing Authoritarianism ## 
rwa1<-recode(data$V162169, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  #Get rid of rotten apples
rwa2<-recode(data$V162168, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  #Our country needs free thinkers*
rwa3<-recode(data$V162170, "1=5; 2=4; 3=3; 4=1; 5=1; else=NA")  #Needs strong leaders
rwa<-cbind(rwa1, rwa2, rwa3)
#rwa<-recode(rwa, "1:2=1; 3=2; 4:5=3")
alpha(data.frame(rwa))  ## Poor Alpha, 0.47
## Survey Mode
mode<-recode(data$V160501, "1=1; 2=0; else=NA")
## Religion
protestanta<-recode(data$V161247a, "1=1; 2:4=0; -1=0; else=NA")
catholica<-recode(data$V161247a, "2=1; 1=0; 3:4=0; -1=0; else=NA")
jewisha<-recode(data$V161247a, "3=1; 1:2=0; 4=0; -1=0; else=NA")
othera<-recode(data$V161247a, "4=1; 1:3=0; -1=0; else=NA")
protestantb<-recode(data$V161247b, "1=1; 2:4=0; -1=0; else=NA")
catholicb<-recode(data$V161247b, "2=1; 1=0; 3:4=0; -1=0; else=NA")
jewishb<-recode(data$V161247b, "3=1; 1:2=0; 4=0; -1=0; else=NA")
otherb<-recode(data$V161247b, "4=1; 1:3=0; -1=0; else=NA")
protestant<-ifelse(protestanta==1 | protestantb==1, 1, 0)
catholic<-ifelse(catholica==1 | catholicb==1, 1, 0)
jewish<-ifelse(jewisha==1 | jewishb==1, 1, 0)
otherrelig<-ifelse(othera==1 | otherb==1, 1, 0)
### Attend religious service
church<-recode(data$V161245, "-1=0; 5=0; 4=0; 3=0; 2=0; 1=1; -9=0")  ## Attend services at least once per week
###religious orthodoxy.
bible<-recode(data$V161243, "1=1; 2=2; 3=1; else=NA")  ##bible is literal word of God, 0 otherwise
###religious importance
#religious.importance<-recode(data$V161242, "-1=0; 1=1; 2=2; 3=3; else=NA")  ## Religious importance. Don't include this
#### Income###
income<-recode(data$V161361x, "16:28=1; 1:15=0; else=NA") ## 68 percentile and higher, 55k, source dydqj.com calculator for 2015 income
#### ideology and pidi
ideology<-(recode(data$V161126, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")-1)/6  ### Ideology 0 1
pid<-(recode(data$V161158x, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")-1)/6  ### PID 01
### Strong leader ###
strong.leader<-recode(data$V162263, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
will.majority<-recode(data$V162267, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
                
### moral traditionalism.
moral1<-recode(data$V162207, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## We should adjust, Reversed
moral2<-recode(data$V162210, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")   #Should be More Emphasis on Traditional Values
moral3<-recode(data$V162209, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  #Tolerance of Different Moral Standards
moral4<-recode(data$V162208, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  #Newer standards breaking down society
alpha(cbind(moral1, moral2, moral3, moral4))
### Racial Resentment
rr1<-recode(data$V162211, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (Irish)
rr2<-recode(data$V162212, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Generations of slavery
rr3<-recode(data$V162213, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less
rr4<-recode(data$V162214, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder
alpha(cbind(rr1, rr2, rr3, rr4))

## Authoritarianism -- Numbering matches the code above
auth.2<-recode(data$V162239, "1=1; 2=3; 3=2; else=NA") ### Child repsect versus indepdnence
auth.3<-recode(data$V162240, "1=1; 2=3; 3=2; else=NA") ### Manners
auth.1<-recode(data$V162241, "1=3; 2=1; 3=2; else=NA") ### Respect
auth.4<-recode(data$V162242, "1=1; 2=3; 3=2; else=NA") ### Behaved
alpha(cbind(auth.1, auth.2, auth.3, auth.4))
###hostile sexism.
hostile1<-recode(data$V161507, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Inerpret everything as sexist
hostile2<-recode(data$V161508, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Consider what men do.
hostile3<-recode(data$V161509, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Gain power through feminism
hostile4<-recode(data$V161510, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Put on tight leash
alpha(cbind(hostile1, hostile2, hostile3, hostile4))
### Demographics
age<-recode(data$V161267, "-9=NA; -8=NA")
college<-recode(data$V161270, "13:16=1; 1:12=0; else=NA")  ## College degree
#media<-recode(data$V162002, "1=0; 2:4=1; else=NA")  ## Watch tv about campaign, one or two, good many, 3 or 4.
white<-recode(data$V161310x, "1=1; 2:6=0; else=NA")
nonwhite<-recode(data$V161310x, "1=0; 2:6=1; else=NA")
black<-recode(data$V161310x, "2=1; 1=0; 3:6=0; else=NA")
hispanic<-recode(data$V161310x, "5=1; 1:4=0; 6=0; else=NA")
other.race<- recode(data$V161310x, "3=1; 4=1; 6=1; 1=0; 2=1; 5=0; else=NA")
#income<- (recode(data$V161361x, "-9=NA; -5=NA")-1)/27
female<-recode(data$V161342, "1=0; 2=1; else=NA")
### anti-egalitarianism.
egal1<-recode(data$V162243, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Unequal opprotunity
egal2<-recode(data$V162244, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Worry less about eqwuality
egal3<-recode(data$V162245, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Tradition
egal4<-recode(data$V162246, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Fewer propnblems if fair treat
alpha(cbind(egal1, egal2, egal3, egal4))
## Some DVs

data.anes.2016<-as.data.frame(cbind(college, mode, rwa1, rwa2, rwa3, rwa4=NA, rwa5=NA,
                                    catholic, protestant, jewish, otherrelig,
                                    church, bible, income, moral1, moral2, moral3,
                                    moral4, rr1, rr2, rr3, rr4, auth.1, auth.2,
                                    auth.3, auth.4, hostile1, hostile2, hostile3, 
                                    hostile4, age, nonwhite, black, hispanic, other.race,
                                    female, egal1, egal2, egal3, egal4,
                                    ideology, pid, child1_2=NA, child2_2=NA,
                                    child3_2=NA, child4_2=NA, strong.leader, will.majority, white
                          ))
save(data.anes.2016, file="data.anes.2016.Rdata")
write.dta(data.anes.2016, file="ANES2016.dta")


####-------------------------------------------------------------------------------------####
# File to Recode ANES 2012
# This uses internet recontact.
####-------------------------------------------------------------------------------------####
rm(list=ls())
anes<-read.dta("data/anes_timeseries_2012_Stata12.dta",
               convert.factors=FALSE)
anes.2012a<-read.dta("data/anes_panel_2013_inetrecontact.dta",
                     convert.factors=FALSE)
anes<-merge(anes, anes.2012a, by=c("caseid"), all.x=T)  ### This tacks on the 2012 Internet Recontact
###Authoritarianism at T1 ###
###Authoritarianism##
auth.2<-recode(anes$auth_obed, "1=3; 2=1; 3=2; else=NA") #Obey Authority
auth.1<-recode(anes$auth_ind, "1=1;  2=3; 3=2; else=NA") #Respect for elders
auth.3<-recode(anes$auth_cur, "1=1;   2=3; 3=2; else=NA") #Good manners
auth.4<-recode(anes$auth_consid, "1=1; 2=3; 3=2; else=NA") #Well behaved
alpha(data.frame(auth.1, auth.2, auth.3, auth.4)) #0.60


## Right Wing Authoritarianism (Internet Recontact)## 
rwa1<-recode(anes$C5_U3, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  #Place faith in authority figures
rwa2<-recode(anes$C5_U2, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ##Our country needs free thinkers*
rwa3<-recode(anes$C5_U4, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ##Needs strong leaders
rwa4<-recode(anes$C5_U1, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  #Everyone has to create their own way*
rwa5<-recode(anes$C5_U5, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  #Old fashioned ways
rwa<-cbind(rwa1, rwa2, rwa3, rwa4, rwa5)
alpha(data.frame(rwa))

## Child rearing, recontact
#
child1_2<-recode(anes$C5_H1, "1=0; 2=1; else=NA")  #Respect for elders
child2_2<-recode(anes$C5_H2, "1=1; 2=0; else=NA")  #obedience
child3_2<-recode(anes$C5_H3, "1=0; 2=1; else=NA")  #good manners
child4_2<-recode(anes$C5_H4, "1=0; 2=1; else=NA")  #considerate
alpha(data.frame(cbind(child1_2, child2_2, child3_2, child4_2)))

mode<-recode(anes$mode, "1=1; 2=0")

###Religion Questions

#religion.important<-recode(anes$relig_import, "-9=NA; -8=NA; 1=1; 2=0")
bible<-recode(anes$relig_wordgod, "1=3; 2=2; 3=1; else=NA")
religion.church1<-recode(anes$relig_church, "1=1; 2=0; else=NA")
religion.church2<-recode(anes$relig_churchoft, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
religion.church<-rep(NA, length(religion.church1))
for(i in 1:length(religion.church1)){
  if(religion.church1[i]==1 & !is.na(religion.church1[i])){
    religion.church[i]<-religion.church2[i]
  }
  else if(religion.church1[i]==0 & !is.na(religion.church1[i])){
    religion.church[i]<-1
  }
  else{
    religion.church[i]<-NA
  }
}  ##This is church attendance
church<-recode(religion.church, "5=1; else=0")
protestant<-recode(anes$relig_7cat_x, "1:3=1; 5=1; 4=0; 6:8=0; else=NA") #All Protestant
catholic<-recode(anes$relig_7cat_x, "4=1; 1:3=0; 5:8=0; else=NA") #Catholic
jewish<-recode(anes$relig_7cat_x, "6=1; 1:5=0; 7:8=0; else=NA") #Jewish Dummy
otherrelig<-recode(anes$relig_7cat_x, "7=1; 1:6=0; 8=0; else=NA") #Other dummy
norelig<-recode(anes$relig_7cat_x, "8=1; 1:7=0;  else=NA") #Other dummy
##Political dispositions
ideology<-(recode(anes$libcpre_self, "-9=NA; -8=NA; -2=NA")-1)/6 ##Conservative
pid<-(recode(anes$pid_x, "-2=NA")-1)/6##Republican
##Moral Traditionalism
moral1<-recode(anes$trad_adjust, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") #Adjust to changing world
moral2<-recode(anes$trad_famval, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") #Fewer problems with more traditionalism
moral3<-recode(anes$trad_tolerant, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") #More tolerant
moral4<-recode(anes$trad_lifestyle, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") #Newer lifestyles break down society
alpha(cbind(moral1, moral2, moral3, moral4))
##Racial Resentment 
##Racial Resentment##
rr1<-recode(anes$resent_workway, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
rr2<-recode(anes$resent_slavery, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
rr3<-recode(anes$resent_deserve, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
rr4<-recode(anes$resent_try, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
###Demographics
female<-recode(anes$gender_respondent_x, "1=0; 2=1; else=NA")
white<-recode(anes$dem_raceeth_x, "1=1; 2:6=0; else=NA")
black<-recode(anes$dem_raceeth_x, "2=1; 1=0; 3:6=0; else=NA")
hispanic<-recode(anes$dem_raceeth_x, "5=1; 1:4=0; 6=0; else=NA")
other.race<-recode(anes$dem_raceeth_x, "3:4=1; 1=0; 2=0; 5=0; 6=1; else=NA")
nonwhite<-recode(anes$dem_raceeth_x, "2:6=1; 1=0; else=NA")
college<-recode(anes$dem_edugroup_x, "1:3=0; 4:5=1; else=NA")
income<-recode(anes$inc_incgroup_pre, "1:15=0; 16:28=1; else=NA") #55k+
age<-recode(anes$dem_age_r_x, "-2=NA")
#media<-recode(anes$mediapo_tv, "1=1; 2=0; else=NA")  ## Watch tv about campaign, one or two, good many, 3 or 4.
### anti-egalitarianism.
egal1<-recode(anes$egal_equal, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Unequal opprotunity
egal2<-recode(anes$egal_worryless, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Worry less about eqwuality
egal3<-recode(anes$egal_notbigprob, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")  ## Tradition
egal4<-recode(anes$egal_fewerprobs, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")  ## Fewer propnblems if fair treat
alpha(cbind(egal1, egal2, egal3, egal4))

data.anes.2012<-as.data.frame(cbind(mode, rwa1, rwa2, rwa3, rwa4, rwa5,
                                    catholic, protestant, jewish, otherrelig,
                                    church, bible, income, moral1, moral2, moral3,
                                    moral4, rr1, rr2, rr3, rr4, auth.1, auth.2,
                                    auth.3, auth.4, hostile1=NA, hostile2=NA, hostile3=NA, 
                                    hostile4=NA, age, nonwhite, black, hispanic, other.race,
                                    female, egal1, egal2, egal3, egal4,
                                    ideology, pid, child1_2, child2_2,
                                    child3_2, child4_2
))
save(data.anes.2012, file="data.anes.2012.Rdata")
write.dta(data.anes.2012, file="ANES2012.dta")

##### Measurement Models for Chapter 7 #####
rm(list=ls())
load("operational.auth.Rdata")
operational.data$auth<-(rowMeans(cbind(operational.data$auth.1, operational.data$auth.2,operational.data$auth.3, operational.data$auth.4), na.rm=T)-1)/2
data<-operational.data
data<-subset(data, year>1999| year==1992)
data$party3<-car::recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
data$mode<-as.character(data$mode)
data<-subset(data, mode=="FTF"|mode=="FTC/CASI" ) ## Drop 1990
y<-subset(data, white==1)
y<-subset(y, year==1992|year==2000|year==2004|year==2008|year==2012| year==2016 | year == 2020)
y<-subset(y, select=c("abortion", "year", "rid2",
                      "gays.adoption", "gov.services",
                      "public.insurance", "jobs", "gay.therm",
                      "feminists.therm", "fundamental.therm", 
                      "union.therm"))
write.csv(y, file="mplus_ch7.csv")













