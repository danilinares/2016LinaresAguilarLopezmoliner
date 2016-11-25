library(tidyverse)
library(quickpsy)

### read and prepare the data ##################################################
dat <- read_csv('data/exp1.csv')

dat <- dat %>%
  select(subject, session, orLarge, orSmall, task, response) %>%
  mutate(response = ifelse(
    (orLarge == 0 & response == 'right') |
    (orLarge == 90 & response == 'down') |
    (orLarge == 180 & response == 'left') |
    (orLarge == 270 & response == 'up') |
    (response == 'm'), 1, 0),
    vertical = ifelse(orLarge==0 | orLarge==180, TRUE, FALSE))

dat <- dat %>% 
  mutate(orLarge = orLarge %>% 
        recode(`0` = 'Top', `90` = 'Right', `180` = 'Bottom', `270` = 'Left'))

datcomp <- dat %>% filter(task == 'comp')
datequ <- dat %>% filter(task == 'equ')