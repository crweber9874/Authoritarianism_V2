## Weight by the margin probabilities ###
rm(list = ls())

# install.packages(c("tidyverse", "msm", "foreign", "boot", "car", 
#                     "nnet", "haven", "lavaan", "cowplot", "readstata13", "reshape2" ),
#                     quiet=TRUE)
#detach("packages:dplyr")
source("/Users/Chris/Dropbox/github_repos/Authoritarianism_V2/configurations/BookFunctions.R")
### User functions to calculate stuff:
### Dependdencies
require(dplyr)
library(msm)
library(boot)
library(ggplot2)
library(nnet)
library(lavaan)
library(cowplot)
require(readstata13)
