## Models for Chapter 9

rm(list = ls())
library(brms)
library(modelr)
library(dplyr)
library(modelr)
library(tidybayes)
library(haven)
library(ggplot2)
library(dplyr)
library(cowplot)

set.seed(32)


load("/Users/Chris/Dropbox/masterData/ANES_CrossSections/clean/cumulativeANES_White.rda")

# Some recodes
data$authoritarianism <- (rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm = T) - 1)
data$authoritarianism2 <- data$authoritarianism^2
data$party3 <- car::recode(data$pid * 6 + 1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA")
data$republican <- car::recode(data$pid * 6 + 1, "1:2=0; 3:5=0; 6:7=1")
data$democrat <- car::recode(data$pid * 6 + 1, "1:2=1; 3:5=0; 6:7=0")
data$independent <- car::recode(data$pid * 6 + 1, "1:2=0; 3:5=1; 6:7=0")

data$ideologyCD <- (data$ideologyCD - 1) / 6
data$ideologyCR <- (data$ideologyCR - 1) / 6

data$ideologyD <- (data$ideologyD - 1) / 6
data$ideologyR <- (data$ideologyR - 1) / 6

### Difference measures
data$difference_2 <- (data$ideologyR - data$ideologyD)^2
data$difference_abs <- data$ideologyR - data$ideologyD %>% abs()
data$difference <- data$ideologyR - data$ideologyD

data$differenceR1 <- data$ideology - data$ideologyR
data$differenceD1 <- data$ideology - data$ideologyD
data$proximity <- (data$ideology - data$ideologyD)^2 - (data$ideology - data$ideologyR)^2
data$differenceR2 <- (data$ideology - data$ideologyR)^2
data$differenceD2 <- (data$ideology - data$ideologyD)^2
data$mode <- as.character(data$mode)
data$diffScore <- data$differenceR1 - data$differenceD1

data$diffR <- data$ideology - data$ideologyR
data$diffD <- data$ideology - data$ideologyD
data$differenceR <- data$ideology - data$ideologyCR
data$differenceD <- data$ideology - data$ideologyCD

data <- data %>%
  mutate(cenD = if_else(diffD >= 1, "right", "none")) %>%
  mutate(cenR = if_else(diffR <= -1, "left", "none"))

dim(data)
# 16% Missing
filteredData <- data %>%
  mutate(confused = ifelse(ideologyR < ideologyD, 1, 0)) %>%
  filter(confused == 0)
# This is Figure 1
polarization <- brm(difference ~ female + age + college + income + jewish +
  catholic + other + authoritarianism + authoritarianism2 + (1 + authoritarianism + authoritarianism2 | year),
data = filteredData,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000
)


# For the Appendix
polarizationEduc <- brm(difference ~ female + age + college + income + jewish +
  catholic + other + authoritarianism + authoritarianism2 +
  college * authoritarianism + college * authoritarianism2 +
  (1 + authoritarianism + authoritarianism2 +
    college * authoritarianism + college * authoritarianism | year),
data = filteredData,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000
)

# These constitute Figure 2
distanceDem <- brm(differenceD1 ~ female + age + college + income + jewish +
  catholic + other + authoritarianism + authoritarianism2 + (1 + authoritarianism + authoritarianism2 | year),
data = filteredData,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000
)

distanceDemEduc <- brm(differenceD1 ~ female + age + college + college *
  authoritarianism + authoritarianism2 + income + jewish +
  catholic + other + authoritarianism * college + authoritarianism2 * college + (1 + authoritarianism + authoritarianism2 + authoritarianism * college + authoritarianism2 * college | year),
data = filteredData,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000
)

distanceRep <- brm(differenceR1 ~ female + age + college + income + jewish +
  catholic + other + authoritarianism + authoritarianism2 + (1 + authoritarianism + authoritarianism2 | year),
data = filteredData,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000
)

distanceRepEduc <- brm(differenceR1 ~ female + age + college + college *
  authoritarianism + authoritarianism2 + income + jewish +
  catholic + other + authoritarianism * college + authoritarianism2 * college + (1 + authoritarianism + authoritarianism2 + authoritarianism * college + authoritarianism2 * college | year),
data = filteredData,
chains = 3,
cores = 8,
seed = 1234,
iter = 1000
)
# ## Supplementary information about the distribution of ideology.
# filteredData$ideol <- recode(filteredData$ideology * 6 + 1,
#   `1` = "Liberal",
#   `2` = "Liberal",
#   `3` = "Moderate",
#   `4` = "Moderate",
#   `5` = "Moderate",
#   `6` = "Conservative",
#   `7` = "Conservative"
# )
# ideologyModel <- brm(ideol ~ female + age + college + income + jewish +
#   catholic + other + authoritarianism + authoritarianism2 +
#   (1 + authoritarianism + authoritarianism2 | year),
# data = data,
# family = "categorical",
# chains = 2,
# cores = 6,
# seed = 1234,
# iter = 4000
# )

set.seed(32)
spatial_dat <- filteredData %>%
  select(vote, differenceD, differenceR, authoritarianism, authoritarianism2, year) %>%
  na.omit() %>%
  mutate(diffCR = differenceR * differenceR) %>%
  mutate(diffCD = differenceD * differenceD) %>%
  mutate(diffOfdiff = diffCD - diffCR)

# Contour Figures
spatialModel <- brm(vote ~ authoritarianism + authoritarianism2 + diffOfdiff +
  authoritarianism * diffOfdiff + authoritarianism2 * diffOfdiff +
  ((1 + diffOfdiff + authoritarianism + authoritarianism2 + diffOfdiff * authoritarianism + authoritarianism2 * diffOfdiff) | year),
family = bernoulli(link = "probit"),
data = spatial_dat,
chains = 3,
cores = 8,
seed = 1234,
iter = 2000,
control = list(adapt_delta = 0.95)
)


set.seed(32)
spatial_dat <- filteredData %>%
  select(vote, differenceD, differenceR, authoritarianism, authoritarianism2, college, year) %>%
  na.omit() %>%
  mutate(diffCR = differenceR * differenceR) %>%
  mutate(diffCD = differenceD * differenceD) %>%
  mutate(diffOfdiff = diffCD - diffCR)


modelFitcollege <- brm(vote ~ authoritarianism + authoritarianism2 + college +
  college * authoritarianism + college * authoritarianism2 + diffCR + diffCD +
  diffCD * authoritarianism + diffCR * authoritarianism +
  diffCD * authoritarianism2 + diffCR * authoritarianism2 +
  diffCD * college + diffCR * college +
  diffCD * college * authoritarianism + diffCR * college * authoritarianism +
  diffCD * college * authoritarianism2 + diffCR * college * authoritarianism2 +
  (1 + authoritarianism + authoritarianism2 + college * authoritarianism + college * authoritarianism2 + diffCR + diffCD + diffCD * authoritarianism + diffCR * authoritarianism + diffCD * authoritarianism2 + diffCR * authoritarianism2 + diffCD * college + diffCR * college + diffCD * college * authoritarianism + diffCR * college * authoritarianism + diffCD * college * authoritarianism2 + diffCR * college * authoritarianism2 | year),
family = bernoulli(link = "probit"),
data = spatial_dat,
chains = 3,
cores = 8,
seed = 1234,
iter = 3000,
control = list(adapt_delta = 0.9)
)

spatial_dat <- filteredData %>%
  filter(ideologyR >= ideologyD) %>%
  select(vote, ideology, ideologyD, ideologyR, authoritarianism, authoritarianism2, female, age, college, income, jewish, catholic, other, year) %>%
  na.omit() %>%
  mutate(polarization = (ideologyR - ideologyD))

polarizationMod <- brm(vote ~ female + age + college + income + jewish +
  catholic + other + authoritarianism + authoritarianism2 + ideology +
  polarization + authoritarianism:polarization + authoritarianism2:polarization +
  (1 + authoritarianism + authoritarianism2 +
    authoritarianism:polarization + authoritarianism2:polarization | year),
data = spatial_dat,
family = "bernoulli",
chains = 3,
cores = 8,
seed = 1234,
iter = 1000
)

spatial_dat <- data %>%
  filter(ideologyR >= ideologyD) %>%
  select(vote, ideology, ideologyD, ideologyR, authoritarianism, authoritarianism2, female, age, college, income, jewish, catholic, other, year) %>%
  na.omit() %>%
  mutate(polarization = (ideologyR - ideologyD))

modelEducPolarization <- brm(vote ~ female + age + college + income + jewish +
  catholic + other + authoritarianism + authoritarianism2 + ideology +
  polarization + authoritarianism:polarization + college:authoritarianism + college:authoritarianism2 + polarization:authoritarianism + authoritarianism2:polarization + college:authoritarianism:polarization + college:authoritarianism2:polarization +
  (1 + authoritarianism:polarization + college:authoritarianism + college:authoritarianism2 + polarization:authoritarianism + college:authoritarianism:polarization + college:authoritarianism2:polarization | year),
data = spatial_dat,
family = "bernoulli",
chains = 3,
cores = 8,
seed = 1234,
iter = 1000
)

spatial_dat <- filteredData %>%
  select(vote, differenceD, differenceR, authoritarianism, authoritarianism2, year) %>%
  na.omit() %>%
  mutate(diffCR = differenceR * differenceR) %>%
  mutate(diffCD = differenceD * differenceD) %>%
  mutate(diffOfdiff = diffCD - diffCR)




spatial_dat <- filteredData %>%
  select(vote, differenceD, differenceR, authoritarianism, authoritarianism2, college, year) %>%
  na.omit() %>%
  mutate(diffCR = differenceR * differenceR) %>%
  mutate(diffCD = differenceD * differenceD) %>%
  mutate(diffOfdiff = diffCD - diffCR)


ch9models <- list(
  filteredData = filteredData,
  polarization = polarization,
  polarizationEduc = polarizationEduc,
  distanceDem = distanceDem,
  distanceDemEduc = distanceDemEduc,
  distanceRep = distanceRep,
  distanceRepEduc = distanceRepEduc,
  spatialModel = spatialModel,
  modelFitcollege = modelFitcollege,
  polarizationMod = polarizationMod,
  modelEducPolarization = modelEducPolarization,
  spatialDat = spatial_dat
)
save(ch9models, file = "/Users/Chris/AuthoritarianismBookProject_FedericoFeldmanWeber/Chapter Analysis/Chapter 9 Analysis/ch9models.rda")
