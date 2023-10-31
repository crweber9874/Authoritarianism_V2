---
title: "Experiment 1 (Updated)"
author: "Christopher M. Federico"
date: "05/04/2023"
output:
  pdf_document: default
  html_document: default
  word_document: default
---

## Results for Experiment 1

The baseline for this experiment is as follows: a combination of background biographical information and partisanship. With this baseline, the idea is to establish the relationship between authoritarianism and candidate preference that exists on the basis of partisan cues alone. The two treatments then add specific combinations of issue differences to the baseline. Treatment 1 will present the same partisan and biographical information as the control condition, but will add information about signature issues for the two candidates. The Republican will take two socially-conservative positions (e.g., anti-immigration and anti-trans positions) and the Democrat will take two economically-conservative positions (e.g., a flat tax and cutting government spending). Treatment 2 will also present the same partisan and biographical information as the control, but reverse the candidates’ signature issues. The Democrat will take two socially-conservative positions (e.g., anti-immigration and anti-trans positions) and the Republican will take two economically-conservative positions (e.g., a flat tax and cutting government spending).

The expectation is that authoritarianism should more strongly predict Republican candidate preference in Treatment 1 than in the baseline (or perhaps the same), but that authoritarianism will be less strongly related to Republican candidate preference in Treatment 2 compared to baseline because of transgression on issues (immigration, trans rights) that help fuel authoritarian sorting.

This experiment also addresses our basic argument that it is the presence of party competition on issues that specifically divide authoritarians vs non-authoritarians that activates authoritarianism’s impact on vote decisions. Here, the focus is less on the specific content of the party difference than on partisan conformity vs transgression on ‘MAGA-constitutive’ issues.

Load in preliminaries and coded data:

```{r message=FALSE}

################################################################################
#### packages.
library(car)
library(psych)
library(ggplot2)
library(sandwich)
library(expss)
library(summarytools)
library(tidyverse)
library(dotwhisker)
library(lmtest)
library(huxtable)
library(haven)
library(interactions)

################################################################################
######### utility functions

std01<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}
rsumm<-function(x){
  descr(x, stats='common', transpose=T)
}
tab<-function(x){
  freq(as_factor(x))
}

################################################################################
######### persistent Weber ggplot() theme

ggtheme = theme(
  plot.title=element_text(face="bold",hjust=0,vjust=0,colour="#3C3C3C",size=20),
  axis.text.x=element_text(size=16,colour="#535353",face="bold"),
  axis.text.y=element_text(size=16,colour="#535353",face="bold"),
  axis.title = element_text(size=16,colour="#535353",face="bold"),
  axis.title.y=element_text(size=16,colour="#535353",face="bold",vjust=1.5),
  axis.ticks=element_blank(),
  strip.text.x = element_text(size = 16),
  panel.grid.major=element_line(colour="#D0D0D0",size=.25),
  panel.background=element_rect(fill="white"),
  legend.text=element_text(size=14),
  legend.title=element_text(size=16))

################################################################################

##### load Rdata if coded and saved previously
load("wff1.Rdata")

```

Subset to white respondents:

```{r message=FALSE}

###### subset to white respondents (N=720)
wdata<-subset(data, white==1 & bc1==1 & sc1==1)

```

Worth nothing that authoritarianism forms a reliable scale:

```{r message=FALSE}

psych::alpha(with(wdata, cbind(aut1, aut2, aut3, aut4, aut5, aut6, aut7, aut8)))

```

## Vote choice model fit and conditional effects

Here, we see more or less what we expected. There is a clear positive relation
between authoritarianism and Republican vote preference in the control
condition, and this remains more or less the same in the condition where the
Republican is socially conservative. However, the effect of authoritarianism
drops to zero in the condition where the candidates flip positions and the
Democrat is socially conservative:

```{r message=FALSE}

# fit
m1 <- lm(rvc ~ female+age+rinc+college+jewish+cath+other+raut*cond, data=wdata)
coeftest(m1, vcovHC(m1, type = "HC3"))

# conditional effects
sim_margins(m1, pred = raut, modx = cond, robust=T)

```

Plotting this:

```{r message=FALSE, fig.width=11, fig.height=8}

interact_plot(m1, pred = raut,
              modx = cond,
              modx.labels=c("Party Labels\nOnly",
                            "Socially Conservative\nRepublican",
                            "Socially Conservative\nDemocrat"),
              interval = TRUE,
              robust=T,
              legend.main = "Condition",
              data = wdata) +
  ggtheme +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0, 1)) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Candidate Support by Condition (Linear)",
       x = "Authoritarianism",
       y = "Support for Republican Candidate")

```

## Feeling thermometer difference model fit and conditional effects

The second analysis used the Republican candidate FT minus the Democratic
candidate FT (with the difference score recoded 0-1) as the dependent measure.

Again, we get the expected result. There is a clear positive relation
between authoritarianism and evaluating the GOP candidate more positively in the
control condition, and this remains more or less the same in the condition where
the Republican is socially conservative. However, the effect of authoritarianism
drops to zero in the condition where the candidates flip positions and the
Democrat is socially conservative:

```{r message=FALSE}

# fit
m2 <- lm(ftdif ~ female+age+rinc+college+jewish+cath+other+raut*cond,
         data=wdata)
coeftest(m2, vcovHC(m2, type = "HC3"))

# conditional effects
sim_margins(m2, pred = raut, modx = cond, robust=T)

```

Plotting this:

```{r message=FALSE, fig.width=11, fig.height=8}

interact_plot(m2, pred = raut,
                    modx = cond,
                    modx.labels=c("Party Labels\nOnly",
                                  "Socially Conservative\nRepublican",
                                  "Socially Conservative\nDemocrat"),
                    interval = TRUE,
                    robust=T,
                    legend.main = "Condition",
                    data = wdata) +
  ggtheme +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0, 1)) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Comparative Candidate FT Ratings\nby Condition (Linear)",
       x = "Authoritarianism",
       y = "Evaluate R More Positively than D")

```

## Candidate trait attribution difference model fit and conditional effects

The third analysis averaged respondents' ratings of each candidate on
effectiveness and shared values. Then the Republican candidate average minus the Democratic candidate average was computed (with the difference score recoded
0-1). This served as the dependent measure.

Again, we get the expected result. There is a clear positive relation
between authoritarianism and attributing more positive traits to the Republican
in the control condition, and this remains more or less the same in the condition
where  the Republican is socially conservative. However, the effect of
authoritarianism drops to zero in the condition where the candidates flip
positions and the Democrat is socially conservative:

```{r message=FALSE}

# fit
m3 <- lm(bsdif2 ~ female+age+rinc+college+jewish+cath+other+raut*cond,
         data=wdata)
coeftest(m3, vcovHC(m3, type = "HC3"))

# conditional effects
sim_margins(m3, pred = raut, modx = cond, robust=T)

```

Plotting this:

```{r message=FALSE, fig.width=11, fig.height=8}

interact_plot(m3, pred = raut,
                    modx = cond,
                    modx.labels=c("Party Labels\nOnly",
                                  "Socially Conservative\nRepublican",
                                  "Socially Conservative\nDemocrat"),
                    interval = TRUE,
                    robust=T,
                    legend.main = "Condition",
                    data = wdata) +
  ggtheme +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0, 1)) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Comparative Candidate Traits\nby Condition (Linear)",
       x = "Authoritarianism",
       y = "Attribute More Positive Traits to R than D")

```

So, Experiment 1 provides is basically consistent with expectations. It seems
like issue defection by the Democratic candidate on social issues in particular
eliminates authoritarians' preference for Republicans.

If we want to do more experiments, my suggestion is that we follow up with some
version of this -- examining the effect of other kinds of issue defection on
social matters for both Democratic and Republican candidates. It also seems
like having party information in there matters.


