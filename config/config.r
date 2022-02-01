## Weight by the margin probabilities ###
rm(list = ls())
#detach("packages:dplyr")
source("/Users/chrisweber/authoritarianism_book_v2/analysis/BookFunctions.r")
### User functions to calculate stuff:
source('/Users/chrisweber/open_projects/panel_models/user_functions.r')
setwd("/Users/chrisweber/authoritarianism_book_v2/")
### Dependdencies
require(dplyr)
require(foreign)
library(msm)
library(boot)
library(ggplot2)
library(nnet)
library(lavaan)
library(cowplot)
