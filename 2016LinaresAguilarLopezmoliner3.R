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
    vertical = ifelse(orLarge == 0 | orLarge == 180, TRUE, FALSE))

dat <- dat %>% 
  mutate(orLarge = orLarge %>% 
         recode(`0` = 'Top', `90` = 'Right', `180` = 'Bottom', `270` = 'Left'))

################################################################################
### sym task preliminary #######################################################
################################################################################
fitsympre <- quickpsy(dat %>% filter(task == 'comp'), orSmall, response,
                    grouping = .(subject, orLarge),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'none')

plotallsym <- fitsympre %>% plot(ypanel = subject, xpanel = orLarge)

save_plot('figures/all/allsym.pdf', plotallsym, base_width = 2 * one_column_width,
          base_height = 5 * one_column_width)

datfilt <- dat %>% filter(!subject %in% 18:21) #eliminating subj with problems

################################################################################
### sym final data #############################################################
################################################################################
# fitsym <- quickpsy(datfilt %>% filter(task == 'comp'), orSmall, response,
#                     grouping = .(subject, orLarge, vertical),
#                     guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
#                     parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
#                     bootstrap = 'nonparametric',
#                     B = 500)
#save(fitsym, file = 'fitsym.RData')
#load('fitsym.RData')


################################################################################
### sym biases #################################################################
################################################################################

### biases
thresholds <- fitsym$thresholds %>% 
  mutate(signif = if_else(threinf * thresup > 0, '*', ' '))
thresholds_pred <- predicting(thresholds) 

plotbar0 <- plotting_bars(thresholds_pred, TRUE)
plotbar90 <- plotting_bars(thresholds_pred, FALSE)

pbar <- plot_grid(plotbar0, plotbar90, labels = c('A', 'B'), ncol = 1, 
                  hjust = 0, vjust = 1)

save_plot('figures/all/biases.pdf', pbar, base_width = one_half_column_width,
          base_height = 1.5 * one_column_width)

### absolute biases 
thresholds_abs <- thresholds %>% 
  group_by(subject, vertical) %>% do(changing_signs(.)) %>% ungroup()
thresholds_abs_pred <- predicting(thresholds_abs)

plotbarabs0 <- plotting_bars(thresholds_abs_pred, TRUE)
plotbarabs90 <- plotting_bars(thresholds_abs_pred, FALSE)

pbarabs <- plot_grid(plotbarabs0, plotbarabs90, labels = c('A', 'B'), ncol = 1, 
                  hjust = 0, vjust = 1)

save_plot('figures/all/biasesabs.pdf', pbarabs, base_width = one_half_column_width,
          base_height = 1.5 * one_column_width)

### absolute biases across subjects
thresholds_abs_av <- average_abs_bias(thresholds_abs)

thresholds_abs_av_pred <- predicting(thresholds_abs_av)

plotbarabsav90 <- plotting_bars_av(thresholds_abs_av_pred, 
                                   thresholds_abs_pred, FALSE)
plotbarabsav0 <- plotting_bars_av(thresholds_abs_av_pred, 
                                  thresholds_abs_pred, TRUE)

################################################################################
### threshold comparisons ######################################################
################################################################################
threscomp <- fitsym$thresholdcomparisons %>% 
  filter(subject == subject2, vertical == vertical2)

thresholdsbootstrap <- fitsym$thresholdsbootstrap %>% 
  mutate(pred = FALSE) %>% filter(orLarge == 'Bottom' | orLarge == 'Left')

thresholdsbootstrappred <- fitsym$thresholdsbootstrap %>% 
  filter(orLarge == 'Top' | orLarge == 'Right') %>% 
  mutate(thre = -thre, pred = TRUE) %>% ungroup() %>% 
  mutate(orLarge = if_else(orLarge == 'Top', 'Bottom', 'Left'))

thresholdsbootstrapall <- bind_rows(thresholdsbootstrap, 
                                    thresholdsbootstrappred)

threscomppred <- threscomppredfun(thresholdsbootstrapall)

################################################################################
### best explanation for each participant ######################################
################################################################################
subj_bias_sign <- thresholds %>% 
  filter(signif == '*', orLarge == 'Top' | orLarge == 'Right') %>% 
  select(subject, vertical)

subj_sen_bias_dif_sign <- threscomp %>% filter(signif == '*') %>% 
  select(subject, vertical)

subj_sen_bias_dif_no_sign <- threscomp %>% filter(signif == ' ') %>% 
  select(subject, vertical)

subj_choic_bias_dif_sign <- threscomppred %>% filter(signif == '*') %>% 
  select(subject, vertical)

subj_choic_bias_dif_no_sign <- threscomppred %>% filter(signif == ' ') %>% 
  select(subject, vertical)

choic_sign_sen_no_sign <- subj_choic_bias_dif_sign %>% 
  semi_join(subj_sen_bias_dif_no_sign) %>% 
  semi_join(subj_bias_sign) %>% ungroup()

choic_no_sign_sen_sign <- subj_choic_bias_dif_no_sign %>% 
  semi_join(subj_sen_bias_dif_sign) %>% 
  semi_join(subj_bias_sign) %>% ungroup()

choic_sign_sen_sign <- subj_choic_bias_dif_sign %>% 
  semi_join(subj_sen_bias_dif_sign) %>% 
  semi_join(subj_bias_sign) %>% ungroup()

sub_sen_or_choic <- thresholds_pred %>% group_by(subject, vertical) %>% 
  do(diferences(.)) %>% 
  mutate(sensory = if_else(dif < difpred, TRUE, FALSE))

################################################################################
### figure 2 ###################################################################
################################################################################
theme_set(theme_classic(10))

plotsym0 <- plotting_sym(fitsym, choic_sign_sen_no_sign, 
                         choic_no_sign_sen_sign, 
                         choic_sign_sen_sign, TRUE, TRUE) 
plotsym90 <- plotting_sym(fitsym, choic_sign_sen_no_sign, 
                          choic_no_sign_sen_sign, 
                          choic_sign_sen_sign,FALSE, TRUE)

pfig2 <- plot_grid(plotsym90, plotbarabsav90, plotsym0, plotbarabsav0, 
                   labels = c('a', 'b', 'c', 'd'), 
                   rel_widths = c(2, 1), hjust = 0, vjust = 1)

save_plot('figures/paper/fig2.pdf', pfig2, base_width = two_column_width,
          base_height = .7* two_column_width)

################################################################################
### sym t-tests ################################################################
################################################################################
verttop <- thresholds_abs_pred %>% filter(vertical, orLarge == 'Top')
vertbot <- thresholds_abs_pred %>% filter(vertical, orLarge == 'Bottom')
vertbotpred <- thresholds_abs_pred %>% 
  filter(vertical, orLarge == 'Bottom prediction')

horrig <- thresholds_abs_pred %>% filter(!vertical, orLarge == 'Right')
horlef <- thresholds_abs_pred %>% filter(!vertical, orLarge == 'Left')
horlefpred <- thresholds_abs_pred %>% 
  filter(!vertical, orLarge == 'Left prediction')

ttesting(verttop, vertbot)
ttesting(verttop, vertbotpred)
ttesting(horrig, horlef)
ttesting(horrig, horlefpred)

################################################################################
### asym preliminary ###########################################################
################################################################################
f <- function(x, p) pnorm(x, p[1] - p[3], p[2]) - pnorm(x, p[1] + p[3], p[2])
fitasympre <- quickpsy(datfilt %>% filter(task == 'equ'), orSmall, response,
                    grouping = .(subject, orLarge),
                    fun = f,
                    parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                    bootstrap = 'none', thresholds = F)

plotallasym <- fitasympre %>% plot(ypanel = subject, xpanel = orLarge)

save_plot('figures/allasym.pdf', plotallasym, base_width = 2 * one_column_width,
          base_height = 5 * one_column_width)

datfilt2 <- datfilt %>% filter(!subject %in% c(7, 17)) # eliminating subj 
#with problems

################################################################################
### asym  ######################################################################
################################################################################
# fitasym <- quickpsy(datfilt2 %>% filter(task == 'equ'), orSmall, response,
#                           grouping = .(subject, orLarge, vertical),
#                           B = 500, fun = f,
#                           parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
#                           bootstrap = 'nonparametric', thresholds = F)
#save(fitsym, file = 'fitsym.RData')
#load('fitsym.RData')

### correlation  
parthre <- fitasym$par %>% filter(parn =='p1') %>% left_join(fitsym$thresholds)
cor.test(parthre$par, parthre$thre)

################################################################################
### collapsing data ############################################################
################################################################################
fitsymv <- quickpsy(datfilt %>% filter(task == 'comp'), orSmall, response,
                    grouping = .(subject,  vertical),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'none')

fitasymv <- quickpsy(datfilt2 %>% filter(task == 'equ'), orSmall, response,
                     grouping = .(subject, vertical),
                     fun = f,
                     parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                     bootstrap = 'none', thresholds = F)

parthrev <- fitasymv$par %>% filter(parn =='p1') %>% 
  left_join(fitsymv$thresholds) 

### correlation
cor.test(parthrev$par, parthrev$thre)
lm(parthrev$thre~parthrev$par) %>% confint()

################################################################################
### asym absolute biases #######################################################
################################################################################
thre_longa <- fitasym$par %>% filter(parn == 'p1') %>% 
  rename(thre = par, threinf = parinf, thresup = parsup)

thre_long_absa <- thre_longa %>% 
  group_by(subject, vertical) %>% do(changing_signs(.)) %>% ungroup()

thre_long_abs_ava <- average_abs_bias(thre_long_absa)

################################################################################
### asym significances #########################################################
################################################################################
fitasym$par %>% filter(parn =='p1', orLarge == 'Right' | orLarge == 'Top') %>%
  mutate(signif = if_else(parinf * parsup > 0, TRUE, FALSE))

threscompa <- fitasym$parcomparisons %>%
  filter(parn =='p1', subject==subject2, vertical==vertical2)

subj_sen_bias_dif_signa <- threscompa %>% filter(signif == '*') %>% 
  ungroup() %>% select(subject, vertical)

verttopa <- thre_long_absa %>% filter(vertical, orLarge == 'Top')
vertbota <- thre_long_absa %>% filter(vertical, orLarge == 'Bottom')
horriga <- thre_long_absa %>% filter(!vertical, orLarge == 'Right')
horlefa <- thre_long_absa %>% filter(!vertical, orLarge == 'Left')
ttesting(verttopa, vertbota)
ttesting(horriga, horlefa)

################################################################################
### figure 3 ###################################################################
################################################################################
plotasym0 <- plotting_asym(fitasym, subj_sen_bias_dif_signa, TRUE, TRUE) 
plotasym90 <- plotting_asym(fitasym, subj_sen_bias_dif_signa, FALSE, TRUE)

plotbarabsava <- plotting_bars_ava(thre_long_abs_ava, thre_long_absa)
pcorcompv <- plotting_corr_compv(parthrev)

pfig3 <- plot_grid(plotasym90, plotbarabsava, plotasym0, pcorcompv, 
                   labels = c('a', 'b', 'c', 'd'), 
                   rel_widths = c(2, 1), hjust = 0, vjust = 1)

save_plot('figures/fig3.pdf', pfig3, base_width = two_column_width,
          base_height = .7* two_column_width)



### correlation ################################################################
### sym 

thre_wide <- fitsym$thresholds %>% select(-vertical) %>% 
  gather(key = thre_cond, value =  threshold, thre, threinf, thresup) %>% 
  group_by(thre_cond) %>% spread(orLarge, threshold) %>% 
  filter(thre_cond == 'thre')


thre_hv <- fitsymv$thresholds %>% spread(vertical, thre, sep = '_') %>% 
  rename(Horizontal = vertical_FALSE, Vertical = vertical_TRUE)

### asynm 
par_wide <- fitasym$par %>% filter(parn =='p1') %>% ungroup() %>%   
  select(-vertical) %>% 
  gather(key = par_cond, value =  parameter, par, parinf, parsup) %>% 
  group_by(par_cond) %>% spread(orLarge, parameter) %>% 
  filter(par_cond == 'par')

par_hv <- fitasymv$par %>% filter(parn =='p1') %>% 
  spread(vertical, par, sep = '_') %>% 
  rename(Horizontal = vertical_FALSE, Vertical = vertical_TRUE)


################################################################################
### Figure correlations ########################################################
################################################################################
pcorsym1 <- plotting_corr(thre_wide, 'Top', 'Bottom')
pcorsym2 <- plotting_corr(thre_wide, 'Right', 'Left')
pcorsym3 <- plotting_corr(thre_wide, 'Top', 'Right')
pcorsym4 <- plotting_corr(thre_wide, 'Bottom', 'Left')

pcorsymind <- plot_grid(pcorsym1, pcorsym2, pcorsym3, pcorsym4,
                     labels = 'Symmetric task\na', 
                     ncol = 4, hjust = 0, vjust = 1.1)


pcorsymhv <- plot_grid(
  plotting_corr(thre_hv, 'Horizontal', 'Vertical'),
  labels = '\nb', hjust = 0, vjust = 1.1)


pcorsym <- plot_grid(pcorsymind, pcorsymhv,rel_widths = c(4,1), 
                     hjust = 0, vjust = 1)

pcorasym1 <- plotting_corr(par_wide, 'Top', 'Bottom')
pcorasym2 <- plotting_corr(par_wide, 'Right', 'Left')
pcorasym3 <- plotting_corr(par_wide, 'Top', 'Right')
pcorasym4 <- plotting_corr(par_wide, 'Bottom', 'Left')

pcorasymind <- plot_grid(pcorasym1, pcorasym2, pcorasym3, pcorasym4,
                        labels = 'Asymmetric task\nc', ncol = 4, 
                        hjust = 0, vjust = 1.1)

pcorasymhv <- plot_grid(
  plotting_corr(par_hv, 'Horizontal', 'Vertical'),
  labels = '\nd', hjust = 0, vjust = 1.1)

pcorasym <- plot_grid(pcorasymind, pcorasymhv, rel_widths = c(4,1),
                      hjust = 0, vjust = 1)

pcorr <- plot_grid(pcorsym, pcorasym, ncol =1, hjust = 0, vjust =1)

save_plot('figures/corr.pdf', pcorr, base_width = two_column_width,
          base_height = 1.25 * one_column_width)
                  

                     
         

################################################################################
### sym and asym comparison ####################################################
################################################################################


pcorcomp <- plotting_corr_comp(parthre %>% semi_join(choic_no_sign_sen_sign))

save_plot('figures/corrchoic_no_sign_sen_sign.pdf', pcorcomp, base_width = one_half_column_width,
          base_height = one_column_width)









