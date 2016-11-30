library(tidyverse)
library(quickpsy)
library(grid)
library(gridExtra)
library(R.utils)
sourceDirectory('R')
source('graphical_parameters.R')

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

### sym preliminary ############################################################
fitsym <- quickpsy(dat %>% filter(task == 'comp'), orSmall, response,
                    grouping = .(subject, orLarge),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'none')

fitsym %>% plot(xpanel = subject, ypanel = orLarge)

dat <- dat %>% filter(!subject %in% 18:21) #eliminating subj with problems

### sym  #######################################################################
fitsym <- quickpsy(dat %>% filter(task == 'comp'), orSmall, response,
                    grouping = .(subject, orLarge, vertical),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'nonparametric',
                    B = 500)
save(fitsym, file = 'fitsym.RData')
load('fitsym.RData')


### psychometric functions 
theme_set(theme_classic(10))
plotsym0 <- plotting_sym(fitsym, TRUE, FALSE) 
plotsym90 <- plotting_sym(fitsym, FALSE, TRUE)

psym <- plot_grid(plotsym0, plotsym90, labels = c('A', 'B'), ncol = 1, 
                  hjust = 0, vjust = 1)
save_plot('figures/sym.pdf', psym, base_width = one_column_width,
          base_height = 1.75 * one_column_width)

### correlation
thresholds <-fitsym$thresholds %>% select(-vertical) %>% 
  gather(key = thre_cond, value =  threshold, thre, threinf, thresup) %>% 
  group_by(thre_cond) %>% spread(orLarge, threshold)

plotting_corr(thresholds)

### biases 

psymbias <- ggplot(fitsym$thresholds) + 
  facet_wrap(~vertical, ncol = 1, as.table = F) +
  geom_col(aes(x = subject, y = thre, fill = factor(orLarge)), 
           position = position_dodge(.9)) +
  geom_linerange(aes(x = subject, ymin = threinf, ymax = thresup, group = orLarge), 
   position = position_dodge(.9))
psymbias

