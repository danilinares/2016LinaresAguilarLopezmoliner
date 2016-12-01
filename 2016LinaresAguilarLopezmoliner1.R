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
dat <- read_csv('data/exp1.csv')

dat <- dat %>%
  select(subject, session, orLarge, orSmall, task, rt, response) %>%
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
################################################################################
### sym preliminary ############################################################
################################################################################
fitsym <- quickpsy(dat %>% filter(task == 'comp'), orSmall, response,
                    grouping = .(subject, orLarge),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'none')

fitsym %>% plot(xpanel = subject, ypanel = orLarge)

dat <- dat %>% filter(!subject %in% 18:21) #eliminating subj with problems

################################################################################
### sym  #######################################################################
################################################################################
# fitsym <- quickpsy(dat %>% filter(task == 'comp'), orSmall, response,
#                     grouping = .(subject, orLarge, vertical),
#                     guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
#                     parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
#                     bootstrap = 'nonparametric',
#                     B = 500)
# save(fitsym, file = 'fitsym.RData')
#load('fitsym.RData')

### comparisons ################################################################
fitsym$thresholdcomparisons %>% filter(subject==subject2, vertical == vertical2)

### psychometric functions #####################################################
theme_set(theme_classic(10))
plotsym0 <- plotting_sym(fitsym, TRUE, FALSE) 
plotsym90 <- plotting_sym(fitsym, FALSE, TRUE)

psym <- plot_grid(plotsym0, plotsym90, labels = c('A', 'B'), ncol = 1, 
                  hjust = 0, vjust = 1)
save_plot('figures/sym.pdf', psym, base_width = one_column_width,
          base_height = 2.5 * one_column_width)

### correlation ################################################################
thresholds <-fitsym$thresholds %>% select(-vertical) %>% 
  gather(key = thre_cond, value =  threshold, thre, threinf, thresup) %>% 
  group_by(thre_cond) %>% spread(orLarge, threshold)

pcor <- plotting_corr(thresholds)
save_plot('figures/corr.pdf', pcor, base_width = one_half_column_width,
          base_height = one_column_width)

### biases #####################################################################

### biases
thre_long <- fitsym$thresholds 
thre_long_pred <- predicting(thre_long) 

plotbar0 <- plotting_bars(thre_long_pred, TRUE, FALSE)
plotbar90 <- plotting_bars(thre_long_pred, FALSE, FALSE)

pbar <- plot_grid(plotbar0, plotbar90, labels = c('A', 'B'), ncol = 1, 
                  hjust = 0, vjust = 1)
save_plot('figures/biases.pdf', pbar, base_width = one_half_column_width,
          base_height = 1.5 * one_column_width)

### absolute biases 
thre_long_abs <- thre_long %>% # un poco cutre, manera mas elegante ?
  group_by(subject, vertical) %>% do(changing_signs(.)) %>% ungroup()
thre_long_abs_pred <- predicting(thre_long_abs)

plotbarabs0 <- plotting_bars(thre_long_abs_pred, TRUE, FALSE)
plotbarabs90 <- plotting_bars(thre_long_abs_pred, FALSE, FALSE)

pbarabs <- plot_grid(plotbarabs0, plotbarabs90, labels = c('A', 'B'), ncol = 1, 
                  hjust = 0, vjust = 1)
save_plot('figures/biasesabs.pdf', pbarabs, base_width = one_half_column_width,
          base_height = 1.5 * one_column_width)

### absolute biases across subjects
thre_long_abs_av <- thre_long_abs %>% 
  group_by(orLarge, vertical) %>% 
  summarise(model = list(t.test(thre))) %>% 
  mutate(tidy = map(model, tidy)) %>% 
  unnest(tidy, .drop = TRUE) %>% 
  rename(thre = estimate, threinf = conf.low, thresup = conf.high) %>% 
  ungroup() %>% 
  mutate(subject = 'All participants')

thre_long_abs_av_pred <- predicting(thre_long_abs_av)
plotbarabsav0 <- plotting_bars(thre_long_abs_av_pred, TRUE, TRUE)
plotbarabsav90 <- plotting_bars(thre_long_abs_av_pred, FALSE, TRUE)

pbarabsav <- plot_grid(plotbarabsav0, plotbarabsav90, labels = c('A', 'B'),  
                      hjust = 0, vjust = 1)
save_plot('figures/biasesabsav.pdf', pbarabsav, base_width = one_half_column_width,
          base_height = 1 * one_column_width)

### t.tests
verttop <- thre_long_abs_pred %>% filter(vertical, orLarge == 'Top')
vertbot <- thre_long_abs_pred %>% filter(vertical, orLarge == 'Bottom')
vertbotpred <- thre_long_abs_pred %>% 
  filter(vertical, orLarge == 'Bottom prediction')

horrig <- thre_long_abs_pred %>% filter(!vertical, orLarge == 'Right')
horlef <- thre_long_abs_pred %>% filter(!vertical, orLarge == 'Left')
horlefpred <- thre_long_abs_pred %>% 
  filter(!vertical, orLarge == 'Left prediction')

ttesting <- function(d1, d2) t.test(d1$thre, d2$thre, paired = TRUE)
ttesting(verttop, vertbot)
ttesting(verttop, vertbotpred)
ttesting(horrig, horlef)
ttesting(horrig, horlefpred)

### sym  times #################################################################
ggplot(dat, aes(x = rt)) + facet_wrap(~subject, scales = 'free') +
  geom_histogram(bins = 20) + xlim(0, 2)

datsub <- dat %>% filter(task == 'comp', rt < .9)
                         
fitsymsub <- quickpsy(datsub, 
                   orSmall, response,
                   grouping = .(subject, orLarge, vertical),
                   guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                   parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                   bootstrap = 'nonparametric',
                   B = 5)

plotting_sym(fitsymsub, TRUE, FALSE) 

fitsym %>% plot(xpanel = subject, ypanel = orLarge)

### david probando un mierdote


