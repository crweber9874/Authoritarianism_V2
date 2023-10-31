####-------------------------------------------------------------------------------------####
# This cleans the voters survey collected by YouGov #
####-------------------------------------------------------------------------------------####
rm(list=ls())
rm(list=ls())
detach("package:dplyr")
require(car)
require(foreign)
data<-readstata13::read.dta13("data/VOTER_Survey_Jan217_Release1-dta.dta", 
                        convert.factors=FALSE)
vote.trump<-recode(data$presvote16post_2016, "1=0; 2=1; else=NA")
vote.romney<-recode(data$post_presvote12_2012, "1=0; 2=1; else=NA")
ft.gay1<-recode(data$gays_t_baseline,"997=NA")
ft.gay2<-recode(data$data$ft_gays_2016, "997=NA")
ft.dem<-recode(data$ft_dem_2017, "997=NA")
ft.rep<-recode(data$ft_rep_2017, "997=NA")
ft.trump1<-recode(data$fav_trump_2016, "1=4; 2=3; 3=2; 4=1; else=NA")
ft.trump2<-recode(data$fav_trump_2017, "1=4; 2=3; 3=2; 4=1; else=NA")
ft.trump3<-recode(data$fav_trump_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
ft.trump4<-recode(data$fav_trump_2019, "1=4; 2=3; 3=2; 4=1; else=NA")


vote.against<-recode(data$vote_for_against_2016, "1=0; 2=1; else=NA")
auth1<-recode(data$SOCIAL_CONFORMITY_3_2016, "1=1;2=0;else=NA") #Obey Authority
auth2<-recode(data$SOCIAL_CONFORMITY_1_2016, "1=0;2=1;else=NA") #Respect for elders
auth3<-recode(data$SOCIAL_CONFORMITY_2_2016, "1=0;2=1;else=NA") #Good manners
auth4<-recode(data$SOCIAL_CONFORMITY_4_2016, "1=0;2=1; else=NA") #Well behaved
authoritarianism<-rowMeans(cbind(auth1, auth2, auth3, auth4), na.rm=T)
auth.split1<-ifelse(authoritarianism>=0.5, 1, 0)
auth.split2<-ifelse(authoritarianism>=0.75, 1, 0)



###Demographics. Code 2011, 2012 and 2016
female.2016<-recode(data$gender_baseline, "1=0; 2=1; else=NA")
female.2018<-recode(data$gender_2018, "1=0; 2=1; else=NA")
female.2019<-recode(data$gender_2019, "1=0; 2=1; else=NA")
age.2016<-2016-recode(data$birthyr_baseline, "9998=NA; 9999=NA")
age.2018<-2018-recode(data$birthyr_2018, "9998=NA;  9999=NA")
age.2019<-2019-recode(data$birthyr_2019, "9998=NA;  9999=NA")

###Demographics. Code all
pid.2011<-recode(data$pid7_baseline, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
pid3.2011<-recode(data$pid7_baseline, "1:2=1; 3:5=2; 6:7=3; else=NA")
pid.2012<-recode(data$post_pid7_2012, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
pid3.2012<-recode(data$post_pid7_2012, "1:2=1; 3:5=2; 6:7=3; else=NA")
pid.2016<-recode(data$pid7_2016, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
pid3.2016<-recode(data$pid7_2016, "1:2=1; 3:5=2; 6:7=3; else=NA")
pid.2017<-recode(data$pid7_2017, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
pid3.2017<-recode(data$pid7_2017, "1:2=1; 3:5=2; 6:7=3; else=NA")
pid.2018<-recode(data$pid7_2018, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
pid3.2018<-recode(data$pid7_2018, "1:2=1; 3:5=2; 6:7=3; else=NA")
pid.2019<-recode(data$pid7_2019, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
pid3.2019<-recode(data$pid7_2019, "1:2=1; 3:5=2; 6:7=3; else=NA")
ideology.2011<-(recode(data$ideo5_baseline, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")-1)/4
ideology3.2011<-recode(data$ideo5_baseline, "1:2=1; 3=2; 4:5=3; else=NA")
ideology.2012<-(recode(data$post_ideo5_2012, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")-1)/4
ideology3.2012<-recode(data$post_ideo5_2012, "1:2=1; 3=2; 4:5=3; else=NA")
ideology.2016<-(recode(data$ideo5_2016, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")-1)/4
ideology3.2016<-recode(data$ideo5_2016, "1:2=1; 3=2; 4:5=3; else=NA")
ideology.2017<-(recode(data$ideo5_2017, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")-1)/4
ideology3.2017<-recode(data$ideo5_2017, "1:2=1; 3=2; 4:5=3; else=NA")
ideology.2018<-(recode(data$ideo5_2018, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")-1)/4
ideology3.2018<-recode(data$ideo5_2018, "1:2=1; 3=2; 4:5=3; else=NA")
ideology.2019<-(recode(data$ideo5_2019, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")-1)/4
ideology3.2019<-recode(data$ideo5_2019, "1:2=1; 3=2; 4:5=3; else=NA")


income.2011<-recode(data$faminc_baseline, "1:5=0; 6:32=1;else=NA") #Greater than 55k
income.2016<-recode(data$faminc_2016, "1:5=0; 6:32=1;else=NA") 
income.2017<-recode(data$faminc_2017, "1:5=0; 6:32=1;else=NA") 
income.2018<-recode(data$faminc_2018, "1:5=0; 6:32=1;else=NA") 
income.2019<-recode(data$faminc_2019, "1:5=0; 6:32=1;else=NA") 

white.2011<-recode(data$race_baseline, "1=1; 2:8=0; else=NA")
white.2016<-recode(data$race_2016, "1=1; 2:8=0; else=NA")
white.2017<-recode(data$race_2016, "1=1; 2:8=0; else=NA")
white.2018<-recode(data$race_2016, "1=1; 2:8=0; else=NA")
white.2019<-recode(data$race_2019, "1=1; 2:8=0; else=NA")
nonwhite.2011<-recode(data$race_baseline, "2:8=1; 1=0; else=NA")
nonwhite.2016<-recode(data$race_2016, "2:8=1; 1=0; else=NA")
nonwhite.2017<-recode(data$race_2017, "2:8=1; 1=0; else=NA")
nonwhite.2018<-recode(data$race_2018, "2:8=1; 1=0; else=NA")
nonwhite.2019<-recode(data$race_2019, "2:8=1; 1=0; else=NA")
black.2011<-recode(data$race_baseline, "2=1; 1=0; 3:8=0; else=NA")
black.2016<-recode(data$race_2016, "2=1; 1=0; 3:8=0; else=NA")
black.2017<-recode(data$race_2017, "2=1; 1=0; 3:8=0; else=NA")
black.2018<-recode(data$race_2018, "2=1; 1=0; 3:8=0; else=NA")
black.2019<-recode(data$race_2019, "2=1; 1=0; 3:8=0; else=NA")
asian.2011<-recode(data$race_baseline, "4=1; 1:3=0; 5:8=0; else=NA")
asian.2016<-recode(data$race_2016, "4=1; 1:3=0; 5:8=0; else=NA")
asian.2017<-recode(data$race_2017, "4=1; 1:3=0; 5:8=0; else=NA")
asian.2018<-recode(data$race_2018, "4=1; 1:3=0; 5:8=0; else=NA")
asian.2019<-recode(data$race_2019, "4=1; 1:3=0; 5:8=0; else=NA")
hispanic.2011<-recode(data$race_baseline, "3=1; 1:2=0; 4:8=0; else=NA")
hispanic.2016<-recode(data$race_2016, "3=1; 1:2=0; 4:8=0; else=NA")
hispanic.2017<-recode(data$race_2017, "3=1; 1:2=0; 4:8=0; else=NA")
hispanic.2018<-recode(data$race_2018, "3=1; 1:2=0; 4:8=0; else=NA")
hispanic.2019<-recode(data$race_2019, "3=1; 1:2=0; 4:8=0; else=NA")
other.race.2011<-recode(data$race_baseline, "5:8=1; 1:4=0;  else=NA")
other.race.2016<-recode(data$race_2016, "5:8=1; 1:4=0;  else=NA")
other.race.2017<-recode(data$race_2017, "5:8=1; 1:4=0;  else=NA")
other.race.2018<-recode(data$race_2018, "5:8=1; 1:4=0;  else=NA")
other.race.2019<-recode(data$race_2019, "5:8=1; 1:4=0;  else=NA")
college.2011<-recode(data$educ_baseline, "5:6=1; 1:4=0; else=NA")
college.2016<-recode(data$educ_2016, "5:6=1; 1:4=0; else=NA")
college.2017<-recode(data$educ_2017, "5:6=1; 1:4=0; else=NA")
college.2018<-recode(data$educ_2018, "5:6=1; 1:4=0; else=NA")
college.2019<-recode(data$educ_2019, "5:6=1; 1:4=0; else=NA")
church.2011<-recode(data$pew_churatd_baseline, "1:2=1; 3:7=0; else=NA") #Attend church at least once per week
church.2016<-recode(data$pew_churatd_2016, "1:2=1; 3:7=0; else=NA") #Attend church at least once per week
church.2017<-recode(data$pew_churatd_2017, "1:2=1; 3:7=0; else=NA") #Attend church at least once per week
church.2018<-recode(data$pew_churatd_2018, "1:2=1; 3:7=0; else=NA") #Attend church at least once per week
church.2019<-recode(data$pew_churatd_2019, "1:2=1; 3:7=0; else=NA") #Attend church at least once per week
evangelical.2011<-recode(data$pew_bornagain_baseline, "1=1; 2=0; else=NA") #in lieu of biblical literalism
evangelical.2016<-recode(data$pew_bornagain_2016, "1=1; 2=0; else=NA") #in lieu of biblical literalism
evangelical.2017<-recode(data$pew_bornagain_2017, "1=1; 2=0; else=NA") #in lieu of biblical literalism
evangelical.2018<-recode(data$pew_bornagain_2018, "1=1; 2=0; else=NA") #in lieu of biblical literalism
evangelical.2019<-recode(data$pew_bornagain_2019, "1=1; 2=0; else=NA") #in lieu of biblical literalism
#### Egalitarianism Items
egal1<-recode(data$egalitarian_opportunies_2017, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
egal2<-recode(data$egalitarian_worry_less_2017, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
egal3<-recode(data$egalitarian_no_big_deal_2017, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
egal4<-recode(data$egalitarian_fewer_problems_2017, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
psych::alpha(cbind(egal1, egal2, egal3, egal4))


religious.importance.2011<-recode(data$pew_religimp_, "1=1;2=1; 3:4=0; else=NA")
religious.importance.2016<-recode(data$pew_religimp_2016, "1=1;2=1; 3:4=0; else=NA")
religious.importance.2017<-recode(data$pew_religimp_2017, "1=1;2=1; 3:4=0; else=NA")
religious.importance.2018<-recode(data$pew_religimp_2018, "1=1;2=1; 3:4=0; else=NA")
religious.importance.2019<-recode(data$pew_religimp_2019, "1=1;2=1; 3:4=0; else=NA")

protestant.2011<-recode(data$religpew_baseline, "1=1;  2:12=0;  else=NA")
catholic.2011<-recode(data$religpew_baseline, "2=1;  1=0; 3:12=0; else=NA")
jewish.2011<-recode(data$religpew_baseline, "5=1;  1:4=0; 6:12=0; else=NA")
other.2011<-recode(data$religpew_baseline, "3:4=1; 6:12=1; 1=0; 2=0; 5=0; else=NA")
protestant.2016<-recode(data$religpew_2016, "1=1;  2:12=0;  else=NA")
catholic.2016<-recode(data$religpew_2016, "2=1;  1=0; 3:12=0; else=NA")
jewish.2016<-recode(data$religpew_2016, "5=1;  1:4=0; 6:12=0; else=NA")
other.2016<-recode(data$religpew_2016, "3:4=1; 6:12=1; 1=0; 2=0; 5=0; else=NA")
protestant.2017<-recode(data$religpew_2017, "1=1;  2:12=0;  else=NA")
catholic.2017<-recode(data$religpew_2017, "2=1;  1=0; 3:12=0; else=NA")
jewish.2017<-recode(data$religpew_2017, "5=1;  1:4=0; 6:12=0; else=NA")
other.2017<-recode(data$religpew_2017, "3:4=1; 6:12=1; 1=0; 2=0; 5=0; else=NA")
protestant.2018<-recode(data$religpew_2018, "1=1;  2:12=0;  else=NA")
catholic.2018<-recode(data$religpew_2018, "2=1;  1=0; 3:12=0; else=NA")
jewish.2018<-recode(data$religpew_2018, "5=1;  1:4=0; 6:12=0; else=NA")
other.2018<-recode(data$religpew_2018, "3:4=1; 6:12=1; 1=0; 2=0; 5=0; else=NA")
protestant.2019<-recode(data$religpew_2019, "1=1;  2:12=0;  else=NA")
catholic.2019<-recode(data$religpew_2019, "2=1;  1=0; 3:12=0; else=NA")
jewish.2019<-recode(data$religpew_2019, "5=1;  1:4=0; 6:12=0; else=NA")
other.2019<-recode(data$religpew_2019, "3:4=1; 6:12=1; 1=0; 2=0; 5=0; else=NA")
democracy.2018<-recode(data$democracy_2018, "1=1; 2=0; else=NA")


# Media Consumption 
interest.2011<-recode(data$newsint2_baseline, "1=4; 2=3; 3=2; 4=1; else=NA")  #Interest in politics
interest.2012<-recode(data$post_newsint_2012, "1=4; 2=3; 3=2; 4=1; else=NA")  #Interest in the News
interest.2016<-recode(data$newsint_2016, "1=4; 2=3; 3=2; 4=1; else=NA")  #Interest in politics


## Values are threatened ##
political.correctness<-recode(data$POLITICAL_CORRECTNESS_2016, "1=0; 2=1; else=NA") #Anti PC
america.worse.2016<-recode(data$Americatrend_2016, "1=1; 2=2; 3=3; else=NA") #Life in America today for people like R compared to fifty years ago
america.worse.2017<-recode(data$Americatrend_2016, "1=1; 2=2; 3=3; else=NA") #Life in America today for people like R compared to fifty years ago
economy.trend.2016<-recode(data$econtrend_2016, "1=1; 2=2; 3=3; else=NA") #Overall, do you think the economy is getting better or worse?
economy.trend.2017<-recode(data$econtrend_2017, "1=1; 2=2; 3=3; else=NA") #Overall, do you think the economy is getting better or worse?
country.track.2016<-recode(data$track_2016, "1=0; 2=1; else=NA") #Would you say things in this country today are
country.track.2017<-recode(data$track_2017, "1=0; 2=1; else=NA") #Would you say things in this country today are
moral.track.2017<-recode(data$track_moral_climate_2017, "1=0; 2=1; else=NA") #Moral Track
future.trend.2016<-recode(data$futuretrend_2016, "1=1; 2=2; 3=3; else=NA") #Standard of living for the current children in the future when they are the same
values.2016<-recode(data$values_culture_2016, "1=1; 2=2; 3=3; else=NA") #In America, values and culture of people like R are...
us.threat.2016<-recode(data$US_respect_2016, "1=1; 2=2; 3=3; else=NA") #Values are threatened In America today, do you feel the values and culture of people like you are:
us.threat.2017<-recode(data$US_respect_2017, "1=1; 2=2; 3=3; else=NA") #Values are threatened In America today, do you feel the values and culture of people like you are:

### Media Questions
media.watch<-recode(data$watchtv_baseline, "1=1; 2=2; 3=3; 4=4; else=NA")
media.eveningnews<-recode(data$localeve_baseline, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #local evevning news
## Morning Shows ###
watch.gma<-recode(data$morn_show_1_baseline, "1=1; 2=0; else=NA")
watch.early<-recode(data$morn_show_2_baseline, "1=1; 2=0; else=NA")
watch.today<-recode(data$morn_show_3_baseline, "1=1; 2=0; else=NA")
watch.early2<-recode(data$morn_show_4_baseline, "1=1; 2=0; else=NA")
watch.ff<-recode(data$morn_show_5_baseline, "1=1; 2=0; else=NA")
watch.ammorning<-recode(data$morn_show_6_baseline, "1=1; 2=0; else=NA")
### Prime time
watch.abc<-recode(data$enews_show_1_baseline, "1=1; 2=0; else=NA")
watch.cbs<-recode(data$enews_show_2_baseline, "1=1; 2=0; else=NA")
watch.nbc<-recode(data$enews_show_3_baseline, "1=1; 2=0; else=NA")
watch.fox<-recode(data$enews_show_4_baseline, "1=1; 2=0; else=NA")
watch.pbs<-recode(data$enews_show_5_baseline, "1=1; 2=0; else=NA")
watch.cnn<-recode(data$enews_show_6_baseline, "1=1; 2=0; else=NA")
watch.msnbc<-recode(data$enews_show_7_baseline, "1=1; 2=0; else=NA")
# Sunday
watch.ftn<-recode(data$sundaytalkshow_1_baseline, "1=1; 2=0; else=NA")
watch.thisweek<-recode(data$sundaytalkshow_2_baseline, "1=1; 2=0; else=NA")
watch.foxsunday<-recode(data$sundaytalkshow_3_baseline, "1=1; 2=0; else=NA")
watch.mtp<-recode(data$sundaytalkshow_4_baseline, "1=1; 2=0; else=NA")
watch.stateunion<-recode(data$sundaytalkshow_5_baseline, "1=1; 2=0; else=NA")
# Daily Talk Shows
watch.oreilly<-recode(data$dailytalkshow_1_baseline, "1=1; 2=0; else=NA")
watch.hannity<-recode(data$dailytalkshow_2_baseline, "1=1; 2=0; else=NA")
watch.vansustern<-recode(data$dailytalkshow_3_baseline, "1=1; 2=0; else=NA")
watch.outfront<-recode(data$dailytalkshow_4_baseline, "1=1; 2=0; else=NA")
watch.andersoncooper<-recode(data$dailytalkshow_5_baseline, "1=1; 2=0; else=NA")
watch.hardball<-recode(data$dailytalkshow_6_baseline, "1=1; 2=0; else=NA")
watch.edshow<-recode(data$dailytalkshow_7_baseline, "1=1; 2=0; else=NA")
watch.maddow<-recode(data$dailytalkshow_8_baseline, "1=1; 2=0; else=NA")
watch.larryo<-recode(data$dailytalkshow_9_baseline, "1=1; 2=0; else=NA")
# Late Talk Shows
watch.nightline<-recode(data$latetalk_1_baseline, "1=1; 2=0; else=NA")
watch.letterman<-recode(data$latetalk_2_baseline, "1=1; 2=0; else=NA")
watch.leno<-recode(data$latetalk_3_baseline, "1=1; 2=0; else=NA")
watch.conan<-recode(data$latetalk_4_baseline, "1=1; 2=0; else=NA")
watch.dailyshow<-recode(data$latetalk_5_baseline, "1=1; 2=0; else=NA")
watch.colbert<-recode(data$latetalk_6_baseline, "1=1; 2=0; else=NA")
watch.sixtyminutes<-recode(data$latetalk_7_baseline, "1=1; 2=0; else=NA")
watch.fortyeight<-recode(data$latetalk_8_baseline, "1=1; 2=0; else=NA")
watch.dateline<-recode(data$latetalk_9_baseline, "1=1; 2=0; else=NA")
watch.abclate<-recode(data$latetalk_10_baseline, "1=1; 2=0; else=NA")

cat(ls()[1:3], sep="," )

### Institutional Confidence###
ccourt.2018<-recode(data$inst_court_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
ccourt.2019<-recode(data$inst_court_2019, "1=4; 2=3; 3=2; 4=1; else=NA")

cmedia.2018<-recode(data$inst_court_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
cmedia.2019<-recode(data$inst_court_2019, "1=4; 2=3; 3=2; 4=1; else=NA")

ccongress.2018<-recode(data$inst_congress_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
ccongress.2019<-recode(data$inst_congress_2019, "1=4; 2=3; 3=2; 4=1; else=NA")

cjustice.2018<-recode(data$inst_justice_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
cjustice.2019<-recode(data$inst_justice_2019, "1=4; 2=3; 3=2; 4=1; else=NA")

cfbi.2018<-recode(data$inst_FBI_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
cfbi.2019<-recode(data$inst_FBI_2019, "1=4; 2=3; 3=2; 4=1; else=NA")

cmilitary.2018<-recode(data$inst_justice_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
cmilitary.2019<-recode(data$inst_justice_2019, "1=4; 2=3; 3=2; 4=1; else=NA")

impeach.2019<-recode(data$impeach_2019, "1=4; 2=3; 3=2; 4=1; else=NA")
mainstream.2019<-recode(data$media_2019, "1=4; 2=3; 3=2; 4=1; else=NA")

### Democratic Rule ####
watch.abclate<-recode(data$latetalk_10_baseline, "1=1; 2=0; else=NA")


strong.leader.2018<-recode(data$systems_leader_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
army.rule.2018<-recode(data$systems_army_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
democratic.rule.2018<-recode(data$governed_2018, "1=1; 2=2; 3=3; 4=4; else=NA")
close.democracy.2018<-recode(data$view1_2018, "1=1; 2=3; 3=2; else=NA")
satisfaction.2018<-recode(data$satisf_dem_2018, "1=4; 2=3; 3=2; 4=1; else=NA")
president.laws.2018<-recode(data$view2_2018, "1=1; 2=0; else=NA")
strong.leader.2017<-recode(data$political_system_leader_2017, "1=4; 2=3; 3=2; 4=1; else=NA")
army.rule.2017<-recode(data$political_system_army_2017, "1=4; 2=3; 3=2; 4=1; else=NA")
democratic.important.2017<-recode(data$democracy_importance_2017, "1=4; 2=3; 3=2; 4=1; else=NA")
close.democracy.2017<-recode(data$democracy_preference_2017, "1=1; 2=3; 3=2; else=NA")
satisfaction.2017<-recode(data$us_democracy_satisfaction_2017, "1=4; 2=3; 3=2; 4=1; else=NA")

####### Separation of Powers #######
president.courts.2017<-recode(data$pres_obey_courts_2017, "1=1; 2=0; else=NA")
president.congress.2017<-recode(data$pres_oversight_2017, "1=0; 2=1; else=NA")
president.media.2017<-recode(data$pres_news_scrutiny_2017, "1=0; 2=1; else=NA")
### Speech Issues ###

free.speech<-recode(data$speech_2018, "1=0; 2=1; else=NA") # Anti Free Speech
flag.burn<-recode(data$amendment_2018, "1=4; 2=3; 3=2; 4=1; else=NA") # Anti Free Speech
kneeling<-recode(data$anthem_2018, "1=1; 2=2; 3=3; 4=4; else=NA") # 



### Defection and Third Parties ###
third.social<-recode(data$third_soc_2018, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # More Conservative
third.immigration<-recode(data$third_immi_2018, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # More Conservative
third.economic<-recode(data$third_econ_2018, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # More Conservative
stranger.country.2017<-recode(data$stranger_2017, "1=4; 2=3; 3=2; 4=1; else=NA") # Stranger in My Country
stranger.country.2018<-recode(data$stranger_2018, "1=4; 2=3; 3=2; 4=1; else=NA") 

### Presidential Approval ###
approve.2012<-recode(data$obamaapp_baseline, "1=4; 2=3; 3=2; 4=1; else=NA") 
approve.2016<-recode(data$obamaapp_2016, "1=4; 2=3;3=2; 4=1; else=NA") 
approve.2017<-recode(data$trumpapp_2017, "1=4; 2=3;3=2; 4=1; else=NA") 
approve.2018<-recode(data$trumpapp_2018, "1=4; 2=3;3=2; 4=1; else=NA")  
approve.2019<-recode(data$trumpapp_2019, "1=4; 2=3;3=2; 4=1; else=NA")  

app.2012<-recode(approve.2012, "3:4=1; 1:2=0") 
app.2016<-recode(approve.2016, "3:4=1; 1:2=0") 
app.2017<-recode(approve.2017, "3:4=1; 1:2=0") 
app.2018<-recode(approve.2018, "3:4=1; 1:2=0")  
app.2019<-recode(approve.2019, "3:4=1; 1:2=0")  


weight.2016<-data$weight_2016
weight.2017<-data$weight_2017
weight.panel<-data$weight_panel
weight.overall<-data$weight_overall
weight.latino<-data$weight_latino
weight.18_24<-data$weight_18_24

state2016<-data$inputstate_2016

full.data<-data.frame(age.2016, age.2018, age.2019,
                      america.worse.2016 , 
                      america.worse.2017 , 
                      asian.2011 , asian.2016 ,
                      asian.2017, asian.2018, asian.2018, asian.2019,
                      auth1 , auth2 , auth3 , auth4 , black.2011 , black.2016 ,
                      black.2017 , black.2018, black.2019, 
                      catholic.2011 , catholic.2016 ,
                      catholic.2017 , catholic.2018, catholic.2019,
                      church.2011 , church.2016 , 
                      church.2017 , church.2018 , church.2019,
                      college.2011 , college.2016 , 
                      college.2017 , college.2018 , college.2019,
                      country.track.2016 ,  country.track.2017,
                      economy.trend.2016 , economy.trend.2017,
                      evangelical.2011 , evangelical.2016 , 
                      evangelical.2017 , evangelical.2018 ,
                      evangelical.2019,
                      female.2016 , female.2018, female.2019,
                      future.trend.2016, hispanic.2011 , hispanic.2016 ,
                      hispanic.2017, hispanic.2018, hispanic.2019,
                      ideology.2011 , ideology.2012 , ideology.2016, 
                      ideology.2017,  ideology.2018,  ideology.2019,
                      ideology3.2011 , ideology3.2012 , ideology3.2016 , 
                      ideology3.2017 , ideology3.2018 , ideology3.2019 , 
                      income.2011 , income.2016 , income.2017, income.2018,
                      income.2019,
                      interest.2011 , interest.2012 , interest.2016 , jewish.2011 ,
                      jewish.2016 , jewish.2017,  jewish.2018,  jewish.2019,
                      media.eveningnews , media.watch ,
                      nonwhite.2011 , nonwhite.2016 , other.2011 , other.2016 , 
                      other.2017, other.2018, other.2019,
                       pid.2011 , pid.2012 , 
                      pid.2016 ,pid.2017, pid.2018, pid.2019,
                      pid3.2011 , pid3.2012 , pid3.2016 ,pid3.2017, pid3.2018,
                      pid3.2018,
                      political.correctness , protestant.2011 , protestant.2016 , protestant.2017,
                      protestant.2018, protestant.2019,
                      religious.importance.2011 , religious.importance.2016 ,
                      religious.importance.2017, religious.importance.2018,
                      us.threat.2016 , us.threat.2017,
                      values.2016 , vote.against , vote.romney , vote.trump , 
                      watch.abc , watch.abclate , watch.ammorning , 
                      watch.andersoncooper , watch.cbs , watch.cnn , watch.colbert , 
                      watch.conan , watch.dailyshow , watch.dateline , 
                      watch.early , watch.early2 , watch.edshow , watch.ff ,
                      watch.fortyeight , watch.fox , watch.foxsunday , watch.ftn , 
                      watch.gma , watch.hannity , watch.hardball , watch.larryo , 
                      watch.leno , watch.letterman , watch.maddow , watch.msnbc , 
                      watch.mtp , watch.nbc , watch.nightline , watch.oreilly , 
                      watch.outfront , watch.pbs , watch.sixtyminutes , 
                      watch.stateunion , watch.thisweek , watch.today , 
                      watch.vansustern , white.2011 , white.2016, white.2017, white.2018,
                      strong.leader.2018,  army.rule.2018, democratic.rule.2018, close.democracy.2018,
                      satisfaction.2018, president.laws.2018, strong.leader.2017, army.rule.2017,
                      democratic.important.2017, close.democracy.2017, satisfaction.2017,
                      president.courts.2017, president.congress.2017, president.media.2017, third.social,
                      third.immigration, stranger.country.2017, stranger.country.2018,
                      weight.2016, weight.2017, weight.panel, weight.overall, weight.overall,
                      weight.18_24, state2016,
                      approve.2012, approve.2016, 
                      approve.2017, approve.2018,
                      approve.2019,
                      president.courts.2017,
                      president.congress.2017,
                      president.media.2017,
                      free.speech, flag.burn, kneeling,
                      app.2012, app.2016, 
                      app.2017, app.2018, app.2019,
                      ccongress.2018, ccongress.2019, ccourt.2018,
                      ccourt.2019, cfbi.2018,    
                      cfbi.2019, cjustice.2018, cjustice.2019, cmedia.2018,
                      cmedia.2019, cmilitary.2018, cmilitary.2019,
                      ft.trump1,ft.trump2, ft.trump3, ft.trump4, democracy.2018,
                      egal1, egal2, egal3, egal4
)




 mplus.data<-data.frame(white.2016, pid.2016,
                        auth1 , auth2 , auth3 , auth4 , ideology.2016
                        )
mplus.data<- subset(mplus.data, white.2016==1)
 
table(data$race_baseline, data$race_2016)

for(i in 1:121){
  print(table(full.data[,i]))
}

# for(i in 1:17){
#   print(table(mplus.data[,i]))
# }
save(full.data, file="data/Voter2016.RData")
write.csv(mplus.data, file="data/authsplit.csv")

#mplus.data<-subset(mplus.data, white.2016==1)
#write.csv(mplus.data, file="/Users/chrisweber/Desktop/threat.data.csv")
          
          