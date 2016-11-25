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

datsym <- dat %>% filter(task == 'comp')
datasym <- dat %>% filter(task == 'equ')

### sym preliminary ############################################################
fitsym <- quickpsy(datsym, orSmall, response,
                    grouping = .(subject, orLarge),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'none')

fitsym %>% plot(xpanel = subject, ypanel = orLarge)

dat <- dat %>% filter(!subject %in% 12:15) #problems with 12, 13, 14, 15


