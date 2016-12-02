library(tidyverse)
library(quickpsy)
library(cowplot)
library(purrr)
library(broom)
library(R.utils)
sourceDirectory('R')
source('graphical_parameters.R')

################################################################################
### read and prepare the data ##################################################
################################################################################
dat2 <- read_csv('data/exp2.csv')

dat2 <- dat2 %>%
  select(subject, session, orLarge, orSmall, task, rt, response) %>%
  mutate(response = ifelse(
    (orLarge == 0 & response == 'right') |
    (orLarge == 180 & response == 'left') |
    (response == 'm'), 1, 0),
    vertical = ifelse(orLarge==0 | orLarge==180, TRUE, FALSE))

dat2 <- dat2 %>% 
  mutate(orLarge = orLarge %>% recode(`0` = 'Top', `180` = 'Bottom'))

conditions <- dat2 %>% group_by(subject, session) %>% 
  distinct(orLarge) %>% summarise(mix = n(), first = first(orLarge))

dat2 <- dat2 %>% left_join(conditions)
################################################################################
### sym preliminary ############################################################
################################################################################
fitsym2 <- quickpsy(dat2, orSmall, response,
                    grouping = .(subject, orLarge),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'none')

fitsym2 %>% plot(xpanel = subject, ypanel = orLarge)

dat2 <- dat2 %>% filter(!subject %in% 6) #eliminating subj with problems

################################################################################
### sym  #######################################################################
################################################################################
fitsym2 <- quickpsy(dat2, orSmall, response,
                    grouping = .(subject, orLarge, mix),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'nonparametric',
                    B = 20)

fitsym2 %>% plot(xpanel = subject, ypanel = mix)
#save(fitsym2, file = 'fitsym2.RData')
#load('fitsym2.RData')




