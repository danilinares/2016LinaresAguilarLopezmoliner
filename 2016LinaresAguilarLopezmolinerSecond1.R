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

### esto se usa mucho david, calcular algun summary en un data frame
### y despues añadirlo al data frame original con joints (normalmente left join)
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

dat2filt <- dat2 %>% filter(!subject %in% 9) #eliminating subj with problems

################################################################################
### sym  #######################################################################
################################################################################
fitsym2 <- quickpsy(dat2filt, orSmall, response,
                    grouping = .(subject, orLarge, mix),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'nonparametric',
                    B = 5)
#save(fitsym2, file = 'fitsym2.RData')
#load('fitsym2.RData')
fitsym2 %>% plot(xpanel = subject, ypanel = mix) + geom_vline(xintercept = 0)


### comparisons ################################################################
fitsym2$thresholdcomparisons %>% filter(subject==subject2, mix == mix2)

### psychometric functions #####################################################
theme_set(theme_classic(10))
plotsym20 <- plotting_sym2(fitsym2, 1, TRUE) 
plotsym290 <- plotting_sym2(fitsym2, 2, TRUE)

psym2 <- plot_grid(plotsym20, plotsym290, labels = c('A', 'B'), ncol = 1, 
                  hjust = 0, vjust = 1)
save_plot('figures/sym2.pdf', psym2, base_width = one_column_width,
          base_height = 2 * one_column_width)

### correlation ################################################################
thresholds2 <-fitsym2$thresholds %>% 
  gather(key = thre_cond, value =  threshold, thre, threinf, thresup) %>% 
  group_by(thre_cond) %>% spread(orLarge, threshold)

pcor2 <- plotting_corr2(thresholds2)
save_plot('figures/corr2.pdf', pcor2, base_width = one_half_column_width,
          base_height = one_column_width)

### biases #####################################################################
thre_long2 <- fitsym2$thresholds 
thre_long_pred2 <- predicting(thre_long2) 

plotbar20 <- plotting_bars2(thre_long_pred2, 1, FALSE)
plotbar290 <- plotting_bars2(thre_long_pred2, 2, FALSE)

pbar2 <- plot_grid(plotbar20, plotbar290, labels = c('A', 'B'), ncol = 1, 
                  hjust = 0, vjust = 1)
save_plot('figures/biases2.pdf', pbar2, base_width = one_half_column_width,
          base_height = 1.5 * one_column_width)

### absolute biases 
thre_long_abs2 <- thre_long2 %>% # un poco cutre, manera mas elegante ?
  group_by(subject, mix) %>% do(changing_signs(.)) %>% ungroup()
thre_long_abs_pred2 <- predicting(thre_long_abs2)

plotbarabs20 <- plotting_bars2(thre_long_abs_pred2, 1, FALSE)
plotbarabs290 <- plotting_bars2(thre_long_abs_pred2, 2, FALSE)

pbarabs2 <- plot_grid(plotbarabs20, plotbarabs290, labels = c('A', 'B'), ncol = 1, 
                     hjust = 0, vjust = 1)
save_plot('figures/biasesabs2.pdf', pbarabs2, base_width = one_half_column_width,
          base_height = 1.5 * one_column_width)

### absolute biases across subjects
thre_long_abs_av2 <- thre_long_abs2 %>% 
  group_by(orLarge, mix) %>% 
  summarise(model = list(t.test(thre))) %>% 
  mutate(tidy = map(model, tidy)) %>% 
  unnest(tidy, .drop = TRUE) %>% 
  rename(thre = estimate, threinf = conf.low, thresup = conf.high) %>% 
  ungroup() %>% 
  mutate(subject = 'All participants')

thre_long_abs_av_pred2 <- predicting(thre_long_abs_av2)
plotbarabsav20 <- plotting_bars2(thre_long_abs_av_pred2, 1, TRUE)
plotbarabsav290 <- plotting_bars2(thre_long_abs_av_pred2, 2, TRUE)

pbarabsav2 <- plot_grid(plotbarabsav20, plotbarabsav290, labels = c('A', 'B'),  
                       hjust = 0, vjust = 1)
save_plot('figures/biasesabsav2.pdf', pbarabsav2, base_width = one_half_column_width,
          base_height = 1 * one_column_width)

### t.tests
top12 <- thre_long_abs_pred2 %>% filter(mix == 1, orLarge == 'Top')
bot12 <- thre_long_abs_pred2 %>% filter(mix == 1, orLarge == 'Bottom')
botpred12 <- thre_long_abs_pred2 %>% 
  filter(mix == 1, orLarge == 'Bottom prediction')

top22 <- thre_long_abs_pred2 %>% filter(mix == 2, orLarge == 'Top')
bot22 <- thre_long_abs_pred2 %>% filter(mix == 2, orLarge == 'Bottom')
botpred22 <- thre_long_abs_pred2 %>% 
  filter(mix == 2, orLarge == 'Bottom prediction')

ttesting <- function(d1, d2) t.test(d1$thre, d2$thre, paired = TRUE)
ttesting(top12, bot12)
ttesting(top12, botpred12)
ttesting(top22, bot22)
ttesting(top22, botpred22)

################################################################################
### serial effects #############################################################
################################################################################

### mix 1
datsermix1 <- dat2filt %>% filter(mix == 1) %>% group_by(orLarge) %>% 
  mutate(lagorSmall = lag(orSmall)) %>% filter(!is.na(lagorSmall)) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) 

fitser1 <- quickpsy(datsermix1, orSmall, response,
                       grouping = .(subject, sign, orLarge),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 50)
pser1 <- plot(fitser1, color = sign, xpanel = subject, ypanel = orLarge) +
  labs(title = 'Top and bottom in different blocks', 
       caption = 'There is a typical aftereffect in some participants')

save_plot('figures/ser1.pdf', pser1, base_width = 4*one_column_width,
          base_height = 1 * one_column_width)


### mix 2
datsermix2 <- dat2filt %>% filter(mix == 2) %>% 
  mutate(lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall)) %>% 
  filter(!is.na(lagorSmall)) %>% 
  mutate(sign = if_else(lagorSmall > 0, 1, -1), 
         equalorLarge = if_else(lagorLarge == orLarge, TRUE, FALSE)) 

fitser2 <- quickpsy(datsermix2 , orSmall, response,
                       grouping = .(subject, sign, equalorLarge),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 50)
pser2 <- plot(fitser2, color = sign, xpanel = subject, ypanel = equalorLarge) +
  labs(title = 'Top and bottom mixed', 
       subtitle = 'TRUE: There is not change',
       caption = 'The typical aftereffect in some participants vanishes')


save_plot('figures/ser2.pdf', pser2, base_width = 4*one_column_width,
          base_height = 1 * one_column_width)


