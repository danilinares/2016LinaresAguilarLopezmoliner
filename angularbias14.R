### libraries# #################################################################
library(cowplot)
library(tidyverse)
library(quickpsy)
### parameters #################################################################
oneColumnWidth <- 3.42
onehalfColumnWidth <- 4.5
twoColumnWidth <- 7
sizeLine1 <- .25
sizePoint1 <- 1
sizePoint2 <- 1.2
B <- 20
### read and prepare the data ##################################################
# second participants cc -> mc, ad: le he visto no haciendo la tarea en algunos trials
# participants that did the task poorly: eb, hw, lm, ym 
first_participants <- c('pa', 'da', 'cc', 'cd', 'ja', 'al')
second_participants <- c('ac', 'dl', 'if', 'ld', 'ls', 'mc', 'mp', 'nv', 'xf', 
                         'zg', 'ad', 'eb', 'hw', 'lm', 'ym')
                         
participants <- c(first_participants, second_participants)

dat <- quickreadfiles(path = 'data', 
                      subject = second_participants,
                      cond = 'angle', session = as.character(1:6))

dat <- dat %>%
  select(subject, session, orLarge, orSmall, task, response) %>%
  mutate(response = ifelse(
                    (orLarge == 0 & response == 'right') |
                    (orLarge == 90 & response == 'down') |
                    (orLarge == 180 & response == 'left') |
                    (orLarge == 270 & response == 'up') |
                    (response == 'm'), 1, 0),
         vertical = ifelse(orLarge==0 | orLarge==180, TRUE, FALSE))

#dat$subject <- factor(dat$subject,
#              labels = paste0('Participant ', 1:length(levelsSubj)))

dat$orLarge <- factor(dat$orLarge,
                      levels = c(0, 90, 180, 270),
                      labels = c('Top', 'Right', 'Botton', 'Left'))

datcomp <- dat %>% filter(task == 'comp')
datequ <- dat %>% filter(task == 'equ')
### comp preliminary ###########################################################
fitcomp <- quickpsy(datcomp, orSmall, response,
                    grouping = .(subject, orLarge),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'none')

fitcomp %>% plot(xpanel = subject, ypanel = orLarge)

participants_ok <- participants %>% setdiff(c('eb', 'hw', 'lm', 'ym'))

dat <- dat %>% filter(subject %in% participants_ok)

### removing participants that could not do the task ###########################
participants_ok <- participants %>% setdiff(c('eb', 'hw', 'lm', 'ym'))

dat <- dat %>% filter(subject %in% participants_ok)


fitcomp <- quickpsy(datcomp, orSmall, response,
                    grouping = .(subject, orLarge, vertical),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'nonparametric',
                    B = B)

### frequency of button presses
datcomp %>% group_by(subject,orLarge) %>% summarise(p = mean(response))
### comparisons
fitcomp$thresholdcomparisons %>%
  filter(subject==subject2, vertical, vertical2)
### choice bias prediction
choicebiascurves <- fitcomp$curves %>%
  filter(orLarge=='Top' | orLarge=='Right' ) %>%
  merge(fitcomp$par %>% filter(parn == 'p1')) %>%
  mutate(x = x - 2*par) #%>%
  filter(x > -2.1, x < 2.1)
### thresholds
fitcompthrelong <- fitcomp$thresholds %>%
  select(-threinf, -thresup, -vertical) %>% spread(orLarge, thre)
fitcompthrelonginf <- fitcomp$thresholds %>%
  select(-thre, -thresup, -vertical) %>% spread(orLarge, threinf) %>%
  rename(Topinf = Top, Rightinf = Right, Bottoninf = Botton, Leftinf = Left)
fitcompthrelongsup <- fitcomp$thresholds %>%
  select(-thre, -threinf, -vertical) %>% spread(orLarge, thresup) %>%
  rename(Topsup = Top, Rightsup = Right, Bottonsup = Botton, Leftsup = Left)

fitcompthrelongwithci <- merge(fitcompthrelong,
                               merge(fitcompthrelonginf, fitcompthrelongsup))
### correlations
topbot <- fitcompthrelongwithci %>% select(subject, Top, Botton) %>%
  rename(x = Top, y = Botton)
riglef <- fitcompthrelongwithci %>% select(subject, Right, Left) %>%
  rename(x = Right, y = Left)
topbotriglef <- rbind(topbot, riglef)
cor.test(topbotriglef$x, topbotriglef$y)

toprig <- fitcompthrelongwithci %>% select(subject, Top, Right) %>%
  rename(x = Top, y = Right)
lefbot <- fitcompthrelongwithci %>% select(subject, Left, Botton) %>%
  rename(x = Left, y = Botton)
topriglefbot <- rbind(toprig, lefbot)
cor.test(topriglefbot$x, topriglefbot$y)


### figure comp
textReference <- 'Reference'
textProb <- 'Prob. responding clockwise'
theme_bias <- theme_set(theme_classic(10))
#theme_bias <- theme_update(axis.line = element_line(size = sizeLine1),
#                           axis.ticks= element_line(size = sizeLine1))

theme_bias <- theme_update(axis.line.x = element_line(colour = 'black', 
                                                    size=sizeLine1, 
                                                    linetype='solid'),
                           axis.line.y = element_line(colour = 'black', 
                                                      size=sizeLine1, 
                                                      linetype='solid'),
                           axis.ticks= element_line(size = sizeLine1))


funpsychocomp <- function(flagVertical, flagOrder) {
  colorcurves1 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  colorcurves2 <- ifelse(flagVertical,'#377eb8','#984ea3')
  colorchoicebiascurves <- ifelse(flagVertical,'#377eb8','#984ea3')
  ggplot(fitcomp$averages %>% filter(vertical==flagVertical),
         aes(x = orSmall, y = prob, color = orLarge, shape=orLarge)) +
    facet_wrap(~subject,scales = 'free_x') +
    geom_vline(xintercept = 0, lty = 2, size  = sizeLine1)+
    geom_point(size = sizePoint1) +
    geom_line(data = fitcomp$curves %>% filter(vertical==flagVertical),
                aes(x = x, y = y),
                size  = sizeLine1) +
     geom_line(data = choicebiascurves %>% filter(vertical==flagVertical),
               aes(x = x, y = y),
               size  = sizeLine1, lty =2, color = colorchoicebiascurves) +
    geom_segment(data = fitcomp$thresholds %>%
                   filter(vertical==flagVertical),
                 aes(x=threinf,xend = thresup, y = .5, yend = 0.5,
                     color=orLarge),
                 size  = sizeLine1) +
    scale_color_manual(values = c(colorcurves1,colorcurves2)) +
    guides(color = guide_legend(reverse=flagOrder),
           shape = guide_legend(reverse=flagOrder)) +
    labs(x = 'Orientation (deg)', y = textProb,
         color = textReference, shape = textReference) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    coord_cartesian(xlim=c(-2.1, 2.1),ylim=c(0,1)) +
    theme(strip.background = element_blank())

}
plotcomp0 <- funpsychocomp(TRUE, FALSE)
plotcomp90 <- funpsychocomp(FALSE, TRUE)

pcor2 <- ggplot(data = fitcompthrelongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=Top,y=Right, color ='Top-Right'),
             size = sizePoint2) +
  geom_segment(aes(x = Topinf, xend = Topsup, y = Right, yend = Right,
                   color ='Top-Right'), size  = sizeLine1) +
  geom_segment(aes(x = Top, xend = Top, y = Rightinf, yend = Rightsup,
                   color ='Top-Right'), size  = sizeLine1) +
  geom_point(aes(x=Left,y=Botton, color='Left-Bottom', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Leftinf, xend = Leftsup, y = Botton, yend = Botton,
                   color ='Left-Bottom'), size  = sizeLine1) +
  geom_segment(aes(x = Left, xend = Left, y = Bottoninf, yend = Bottonsup,
                   color ='Left-Bottom'), size  = sizeLine1) +
  guides(shape = FALSE) +
  scale_color_manual(values = c('#a65628','#f781bf')) +
  scale_shape_discrete(solid=F) +
  labs(x = 'PND (deg)', y = 'PND (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) 
 # coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) 

pcor1 <- ggplot(data = fitcompthrelongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=Top,y=Botton, color ='Top-Bottom'),
             size = sizePoint2) +
  # geom_segment(aes(x = Topinf, xend = Topsup, y = Botton, yend = Botton,
  #                  color ='Top-Bottom'), size  = sizeLine1) +
  # geom_segment(aes(x = Top, xend = Top, y = Bottoninf, yend = Bottonsup,
  #                  color ='Top-Bottom'), size  = sizeLine1) +
  geom_point(aes(x=Right,y=Left, color='Right-Left'),
             size = sizePoint2) +
  # geom_segment(aes(x = Rightinf, xend = Rightsup, y = Left, yend = Left,
  #                  color ='Right-Left'), size  = sizeLine1) +
  # geom_segment(aes(x = Right, xend = Right, y = Leftinf, yend = Leftsup,
  #                  color ='Right-Left'), size  = sizeLine1) +
  guides(shape = FALSE) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = c('#ff7f00','#999999')) +
  labs(x = 'PND (deg)', y = 'PND (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1, 1))
  coord_equal()


pcomppsy <- plot_grid(plotcomp90, plotcomp0, labels = c('A','B'),
                      ncol = 1, hjust = 0, vjust = 1)
pcor <- plot_grid(pcor1, labels = 'C',  hjust = 0)

pcomp <- plot_grid(pcomppsy, pcor, ncol =1, rel_heights = c(2.6,.8))

save_plot('figures/comp.pdf', pcomp,
          base_width = onehalfColumnWidth,
          base_height = 2.5 * onehalfColumnWidth)


### fit two cumulative normal ##################################################
f <- function(x, p) pnorm(x, p[1] - p[3], p[2]) - pnorm(x, p[1] + p[3], p[2])
fitequcumnorm <- quickpsy(datequ, orSmall, response,
                          grouping = .(subject,orLarge,vertical),
                          B = 20, fun = f,
                          parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                          bootstrap = 'nonparametric', thresholds = F)

### calculating the maximum
equcumnormmax <- fitequcumnorm$curves %>%
  summarise(maxi=approx(x=y,y=x,xout=max(y))[[1]])
### comparisons
fitequcumnorm$parcomparisons %>%
  filter(parn =='p1', subject==subject2, vertical, vertical2)

### pse
pse <- fitequcumnorm$par %>% filter(parn=='p1') %>% merge(equcumnormmax)
fiteqpselong <- pse %>%
  select(-parinf, -parsup, -vertical, -maxi) %>% spread(orLarge, par)
fiteqpselonginf <- pse %>%
  select(-par, -parsup, -vertical,-maxi) %>% spread(orLarge, parinf) %>%
  rename(Topinf = Top, Rightinf = Right, Bottoninf = Botton, Leftinf = Left)
fiteqpselongsup <- pse %>%
  select(-par, -parinf, -vertical,-maxi) %>% spread(orLarge, parsup) %>%
  rename(Topsup = Top, Rightsup = Right, Bottonsup = Botton, Leftsup = Left)

fiteqpselongwithci <- merge(fiteqpselong,
                               merge(fiteqpselonginf, fiteqpselongsup))

### correlations
topboteq <- fiteqpselong %>% select(subject, Top, Botton) %>%
  rename(x = Top, y = Botton)
riglefeq <- fiteqpselong %>% select(subject, Right, Left) %>%
  rename(x = Right, y = Left)
topbotriglefeq <- rbind(topboteq, riglefeq)
cor.test(topbotriglefeq$x, topbotriglefeq$y)

toprigeq <- fiteqpselong %>% select(subject, Top, Right) %>%
  rename(x = Top, y = Right)
lefboteq <- fiteqpselong %>% select(subject, Left, Botton) %>%
  rename(x = Left, y = Botton)
topriglefboteq <- rbind(toprigeq, lefboteq)

cor.test(topriglefboteq$x, topriglefboteq$y)

### figure eq
textProb2 <- 'Prob. responding aligned'
funpsychoeq <- function(flagVertical, flagOrder) {
  colorcurves1 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  colorcurves2 <- ifelse(flagVertical,'#377eb8','#984ea3')
  colorchoicebiascurves <- ifelse(flagVertical,'#377eb8','#984ea3')
  ggplot() +
    facet_wrap(~subject,scales = 'free_x') +
    geom_vline(xintercept = 0, lty = 2, size  = sizeLine1)+
    geom_rect(data = fitcomp$thresholds %>%
                   filter(vertical==flagVertical),
                 aes(xmin=threinf,xmax=thresup,ymin=0, ymax=1, fill=orLarge),
                  show.legend = FALSE,alpha = .25) +
#     geom_segment(data = fitcomp$thresholds %>%
#                    filter(vertical==flagVertical),
#                  aes(x=threinf,xend = thresup, y = 0, yend = 0,
#                      color=orLarge), show.legend = FALSE,
#                  alpha = .25, size = 2.25) +
    geom_point(data=fitequcumnorm$averages %>% filter(vertical==flagVertical),
               aes(x = orSmall, y = prob, color = orLarge, shape=orLarge),
               size = sizePoint1) +
    geom_segment(data = pse %>% filter(vertical==flagVertical),
                 aes(x = parinf, xend = parsup, y = 0, yend = 0,
                     color = orLarge),size  = sizeLine1) +
    geom_segment(data = pse %>% filter(vertical==flagVertical),
                 aes(x = par, xend = par, y = 0, yend = maxi,
                     color = orLarge),size  = sizeLine1) +
    geom_line(data = fitequcumnorm$curves %>% filter(vertical==flagVertical),
              aes(x = x, y = y, color = orLarge),
              size  = sizeLine1) +
    scale_color_manual(values = c(colorcurves1,colorcurves2)) +
    guides(color = guide_legend(reverse=flagOrder),
           shape = guide_legend(reverse=flagOrder)) +
    labs(x = 'Orientation (deg)', y = textProb2,
         color = textReference, shape = textReference) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    coord_cartesian(xlim=c(-2.1, 2.1),ylim=c(0,1)) +
    theme(strip.background = element_blank())

}
ploteq0 <- funpsychoeq(TRUE, FALSE)
ploteq90 <- funpsychoeq(FALSE, TRUE)

pcoreq2 <- ggplot(data = fiteqpselongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=Top,y=Right, color ='Top-Right', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Topinf, xend = Topsup, y = Right, yend = Right,
                   color ='Top-Right', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Top, xend = Top, y = Rightinf, yend = Rightsup,
                   color ='Top-Right', shape = subject), size  = sizeLine1) +
  geom_point(aes(x=Left,y=Botton, color='Left-Bottom', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Leftinf, xend = Leftsup, y = Botton, yend = Botton,
                   color ='Left-Bottom', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Left, xend = Left, y = Bottoninf, yend = Bottonsup,
                   color ='Left-Bottom', shape = subject), size  = sizeLine1) +
  guides(shape = FALSE) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = c('#a65628','#f781bf')) +
  labs(x = 'PMR (deg)', y = 'PMR (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

pcoreq1 <- ggplot(data = fiteqpselongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=Top,y=Botton, color ='Top-Bottom'),
             size = sizePoint2) +
  # geom_segment(aes(x = Topinf, xend = Topsup, y = Botton, yend = Botton,
  #                  color ='Top-Bottom'), size  = sizeLine1) +
  # geom_segment(aes(x = Top, xend = Top, y = Bottoninf, yend = Bottonsup,
  #                  color ='Top-Bottom'), size  = sizeLine1) +
  geom_point(aes(x=Right,y=Left, color='Right-Left'),
             size = sizePoint2) +
  # geom_segment(aes(x = Rightinf, xend = Rightsup, y = Left, yend = Left,
  #                  color ='Right-Left'), size  = sizeLine1) +
  # geom_segment(aes(x = Right, xend = Right, y = Leftinf, yend = Leftsup,
  #                  color ='Right-Left'), size  = sizeLine1) +
  guides(shape = FALSE) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = c('#ff7f00','#999999')) +
  labs(x = 'PMR (deg)', y = 'PMR (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

peqpsy <- plot_grid(ploteq90, ploteq0, labels = c('A','B'),
                      ncol = 1, hjust = 0, vjust = 1)
pcoreq <- plot_grid(pcoreq1, labels = 'C', hjust = 0)

peq <- plot_grid(peqpsy, pcoreq, ncol =1, rel_heights = c(2.6,.8))

save_plot('figures/eq.pdf', peq,
          base_width = onehalfColumnWidth,
          base_height = 2.5 * onehalfColumnWidth)

### fig sup ###################################################################
psup <-plot_grid(pcor2, pcoreq2, labels = c('A','B'), ncol=1, hjust = 0)

save_plot('figures/sup.pdf', psup,
          base_width = oneColumnWidth,
          base_height = 1.5*oneColumnWidth)
### fig all ####################################################################
all <- merge(pse, fitcomp$thresholds)

confint(lm(all$thre~all$par))
cor.test(all$par,all$thre)

pcorcompeq <- ggplot(data = all )+ #facet_wrap(~orLarge)+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=par,y=thre,color=orLarge))+
  # geom_segment(aes(x = parinf, xend = parsup, y = thre, yend = thre,
  #                  color =orLarge), size  = sizeLine1) +
  # geom_segment(aes(x = par, xend = par, y = threinf, yend = thresup,
  #                  color =orLarge), size  = sizeLine1) +
  scale_shape_discrete(solid=F) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))+
  labs(x = 'PMR asymmetric task (deg)', y = 'PND symmetric task (deg)') +
  guides(shape = FALSE) +
  scale_colour_brewer(palette = 'Set1')+
  labs(color = textReference) +
  theme(legend.key.size = unit(1,'line'),
        plot.margin = unit(c(-5,0,0,0), 'line'))

save_plot('figures/all.pdf', pcorcompeq,
          base_width = oneColumnWidth,
          base_height = oneColumnWidth)

#### comp and eq comp ##########################################################
compeq <- fitequcumnorm$parbootstrap %>% filter(parn == 'p1') %>%
  select(-parn) %>% merge(fitcomp$thresholdsbootstrap %>% select(-prob)) %>%
  mutate(dif = par - thre) %>% group_by(subject, orLarge) %>%
  summarise(inf = quantile(dif, .025), sup = quantile(dif,.975),
            sign = ifelse(inf * sup > 0, 1, 0))

################################################################################
### serial effects #############################################################
################################################################################
#### orLarge == lagorLarge, task == lagTask
datser <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(orLarge == lagorLarge, task == lagTask) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 
datser %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitcompser <- quickpsy(datser, orSmall, response,
                       grouping = .(subject, sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1000)
dife <- fitcompser$thresholdcomparisons %>% filter(subject==subject2)
plot(fitcompser, color = sign) 

datser %>% group_by(sign, orSmall) %>% summarise(n=n())
fitcompserind <- quickpsy(datserind, orSmall, response,
                       grouping = .(sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1000)
difeind <- fitcompserind$thresholdcomparisons 
plot(fitcompserind, color = sign) 

datserpse <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(orLarge == lagorLarge, task == lagTask) %>% 
  merge(fitcomp$thresholds) %>%
  mutate(sign = ifelse(lagorSmall > thre, 1, -1)) %>%
  filter(task == 'comp') 
datserpse %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitcompserpse <- quickpsy(datserpse, orSmall, response,
                       grouping = .(subject, sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1000)
difepse <- fitcompserpse$thresholdcomparisons %>% filter(subject==subject2)
plot(fitcompserpse, color = sign) 



datser2 <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(task == lagTask, 
    (orLarge=='Top' & lagorLarge=='Botton') |
    (orLarge=='Botton' & lagorLarge=='Top') |
    (orLarge=='Left' & lagorLarge=='Rigth') |
    (orLarge=='Right' & lagorLarge=='Left') 
  ) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 
datser2 %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitcompser2 <- quickpsy(datser2, orSmall, response,
                       grouping = .(subject, sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1000)
dife2 <- fitcompser2$thresholdcomparisons %>% filter(subject==subject2)
plot(fitcompser2, color = sign) 

datser2 %>% group_by(sign, orSmall) %>% summarise(n=n())
fitcompser2ind <- quickpsy(datser2, orSmall, response,
                        grouping = .(sign),
                        guess = TRUE, lapses = TRUE, 
                        parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                        bootstrap = 'nonparametric',
                        B = 1000)
dife2ind <- fitcompser2ind$thresholdcomparisons #%>% filter(subject==subject2)
plot(fitcompser2ind, color = sign) 


datser3 <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter( task == lagTask, 
          orLarge==lagorLarge |
          (orLarge=='Top' & lagorLarge=='Botton') |
            (orLarge=='Botton' & lagorLarge=='Top') |
            (orLarge=='Left' & lagorLarge=='Rigth') |
            (orLarge=='Right' & lagorLarge=='Left') 
  ) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 
datser3 %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitcompser3 <- quickpsy(datser3, orSmall, response,
                        grouping = .(subject, sign),
                        guess = TRUE, lapses = TRUE, 
                        parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                        bootstrap = 'nonparametric',
                        B = 1000)
dife3 <- fitcompser3$thresholdcomparisons %>% filter(subject==subject2)
plot(fitcompser3, color = sign) 

fitcompser3ind <- quickpsy(datser3, orSmall, response,
                        grouping = .(sign),
                        guess = TRUE, lapses = TRUE, 
                        parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                        bootstrap = 'nonparametric',
                        B = 1000)
dife3ind <- fitcompser3ind$thresholdcomparisons 
plot(fitcompser3ind, color = sign) 


#### orLarge == lagorLarge
datserc <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(orLarge == lagorLarge) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 
datserc %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitcompserc <- quickpsy(datserc, orSmall, response,
                       grouping = .(subject, sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1000)
difec <- fitcompserc$thresholdcomparisons %>% filter(subject==subject2)
plot(fitcompserc, color = sign) 


datser2c <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter( 
          (orLarge=='Top' & lagorLarge=='Botton') |
            (orLarge=='Botton' & lagorLarge=='Top') |
            (orLarge=='Left' & lagorLarge=='Rigth') |
            (orLarge=='Right' & lagorLarge=='Left') 
  ) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 
datser2c %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitcompser2c <- quickpsy(datser2c, orSmall, response,
                        grouping = .(subject, sign),
                        guess = TRUE, lapses = TRUE, 
                        parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                        bootstrap = 'nonparametric',
                        B = 1000)
dife2c <- fitcompser2c$thresholdcomparisons %>% filter(subject==subject2)
plot(fitcompser2c, color = sign) 


datser3c <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(    orLarge==lagorLarge |
            (orLarge=='Top' & lagorLarge=='Botton') |
            (orLarge=='Botton' & lagorLarge=='Top') |
            (orLarge=='Left' & lagorLarge=='Rigth') |
            (orLarge=='Right' & lagorLarge=='Left') 
  ) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 
datser3c %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitcompser3c <- quickpsy(datser3c, orSmall, response,
                        grouping = .(subject, sign),
                        guess = TRUE, lapses = TRUE, 
                        parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                        bootstrap = 'nonparametric',
                        B = 1000)
dife3c <- fitcompser3c$thresholdcomparisons %>% filter(subject==subject2)
plot(fitcompser3c, color = sign) 


#### orLarge == lagorLarge, task == lagTask
datsereq <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(orLarge == lagorLarge, task == lagTask) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'equ') 
datsereq %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
f <- function(x, p) pnorm(x, p[1] - p[3], p[2]) - pnorm(x, p[1] + p[3], p[2])
fitequcumnormser <- quickpsy(datsereq, orSmall, response,
                             grouping = .(subject,sign),
                             B = 1000, fun = f,
                             parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                             bootstrap = 'nonparametric', thresholds = F)
fitequcumnormser$parcomparisons %>% filter(parn=='p1', subject==subject2)
plot(fitequcumnormser, color = sign) 

datsereq2 <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter( task == lagTask, 
          (orLarge=='Top' & lagorLarge=='Botton') |
            (orLarge=='Botton' & lagorLarge=='Top') |
            (orLarge=='Left' & lagorLarge=='Rigth') |
            (orLarge=='Right' & lagorLarge=='Left') 
  ) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'equ') 
datsereq2 %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitequcumnormser2 <- quickpsy(datsereq2, orSmall, response,
                             grouping = .(subject,sign),
                             B = 1000, fun = f,
                             parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                             bootstrap = 'nonparametric', thresholds = F)
fitequcumnormser2$parcomparisons %>% filter(parn=='p1', subject==subject2)
plot(fitequcumnormser2, color = sign) 

datsereq3 <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall),
         lagTask = lag(task)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(task == lagTask, 
        orLarge==lagorLarge |
       (orLarge=='Top' & lagorLarge=='Botton') |
       (orLarge=='Botton' & lagorLarge=='Top') |
       (orLarge=='Left' & lagorLarge=='Rigth') |
       (orLarge=='Right' & lagorLarge=='Left')) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'equ')
datsereq3 %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitequcumnormser3 <- quickpsy(datsereq3, orSmall, response,
                              grouping = .(subject,sign),
                              B = 1000, fun = f,
                              parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                              bootstrap = 'nonparametric', thresholds = F)
fitequcumnormser3$parcomparisons %>% filter(parn=='p1', subject==subject2)
plot(fitequcumnormser3, color = sign) 

#### orLarge == lagorLarge
datsereqc <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(orLarge == lagorLarge) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'equ') 
datsereqc %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitequcumnormserc <- quickpsy(datsereqc, orSmall, response,
                             grouping = .(subject,sign),
                             B = 1000, fun = f,
                             parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                             bootstrap = 'nonparametric', thresholds = F)
fitequcumnormserc$parcomparisons %>% filter(parn=='p1', subject==subject2)
plot(fitequcumnormserc, color = sign) 

datsereq2c <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  filter((orLarge=='Top' & lagorLarge=='Botton') |
         (orLarge=='Botton' & lagorLarge=='Top') |
         (orLarge=='Left' & lagorLarge=='Rigth') |
         (orLarge=='Right' & lagorLarge=='Left')) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'equ') 
datsereq2c %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitequcumnormser2c <- quickpsy(datsereq2c, orSmall, response,
                              grouping = .(subject,sign),
                              B = 1000, fun = f,
                              parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                              bootstrap = 'nonparametric', thresholds = F)
fitequcumnormser2c$parcomparisons %>% filter(parn=='p1', subject==subject2)
plot(fitequcumnormser2c, color = sign) 

datsereq3c <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(orLarge==lagorLarge |
        (orLarge=='Top' & lagorLarge=='Botton') |
        (orLarge=='Botton' & lagorLarge=='Top') |
        (orLarge=='Left' & lagorLarge=='Rigth') |
        (orLarge=='Right' & lagorLarge=='Left')) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'equ')
datsereq3c %>% group_by(subject, sign, orSmall) %>% summarise(n=n())
fitequcumnormser3c <- quickpsy(datsereq3c, orSmall, response,
                              grouping = .(subject,sign),
                              B = 1000, fun = f,
                              parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                              bootstrap = 'nonparametric', thresholds = F)
fitequcumnormser3c$parcomparisons %>% filter(parn=='p1', subject==subject2)
plot(fitequcumnormser3c, color = sign) 












datser %>% group_by(subject,orLarge,sign,orSmall) %>% summarise(n=n())

fitcompser <- quickpsy(datser, orSmall, response,
                       grouping = .(subject,sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B=1)

dife <- fitcompser$thresholdcomparisons %>% 
  filter(subject==subject2)

t.test(dife$dif)

plot(fitcompser, color = sign) 

### comp orLarge 
datser <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(orLarge =='Left', lagorLarge=='Left') %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 

datser %>% group_by(subject,orLarge,sign,orSmall) %>% summarise(n=n())

fitcompser <- quickpsy(datser, orSmall, response,
                       grouping = .(subject, orLarge, sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B=1)

dife <- fitcompser$thresholdcomparisons %>% 
  filter(subject==subject2,orLarge==orLarge2)

t.test(dife$dif)

p1<- plot(fitcompser, color = sign) 

datser <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(orLarge =='Left', lagorLarge=='Right') %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 

datser %>% group_by(subject,orLarge,sign,orSmall) %>% summarise(n=n())

fitcompser <- quickpsy(datser, orSmall, response,
                       grouping = .(subject, orLarge, sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B=1)

dife <- fitcompser$thresholdcomparisons %>% 
  filter(subject==subject2,orLarge==orLarge2)

t.test(dife$dif)

p2<- plot(fitcompser, color = sign) 

plot_grid(p1,p2, ncol = 1)





### comp orLarge 
datser <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(lagorLarge == 'Top', orLarge == 'Top') %>% 
  #filter(vertical == lagvertical) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 

datser %>% group_by(subject,orLarge,sign,orSmall) %>% summarise(n=n())

fitcompser <- quickpsy(datser, orSmall, response,
                       grouping = .(subject, orLarge,sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1)

plot(fitcompser, color = sign) 


+
  geom_vline(xintercept = 0,lty=2) +
  geom_line(data=fitcomp$curves, aes(x=x,y=y))+
  geom_vline(data=fitcomp$thresholds,aes(xintercept=thre)) +
  geom_point(data=fitcomp$averages,aes(x=orSmall,y=prob)) +xlim(-2,2)
### comp orLarge 
### comp orLarge 
datser <- dat %>% 
  mutate(lagvertical = lag(vertical), 
         lagorLarge = lag(orLarge),
         lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(orLarge == lagorLarge) %>% 
  #filter(vertical == lagvertical) %>% 
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 

datser %>% group_by(subject,orLarge,sign,orSmall) %>% summarise(n=n())



fitcompser <- quickpsy(datsercomp, orSmall, response,
                       grouping = .(subject, orLarge,sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1)

fitcompser$thresholdcomparisons %>% filter(subject==subject2,orLarge==orLarge2)

plot(fitcompser, color = sign) +
  geom_vline(xintercept = 0,lty=2) +
  geom_line(data=fitcomp$curves, aes(x=x,y=y))+
  geom_vline(data=fitcomp$thresholds,aes(xintercept=thre)) +
  geom_point(data=fitcomp$averages,aes(x=orSmall,y=prob)) +xlim(-2,2)
### comp orLarge 
datser <- dat %>% merge(fitcomp$thresholds) %>%
  mutate(lagvertical = lag(vertical), lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  filter(vertical == lagvertical) %>% 
  mutate(sign = ifelse(lagorSmall > thre, 1, -1)) %>%
  filter(task == 'comp') 

datser %>% group_by(subject,orLarge,sign,orSmall) %>% summarise(n=n())



fitcompser <- quickpsy(datsercomp, orSmall, response,
                       grouping = .(subject, orLarge,sign),
                       guess = TRUE, lapses = TRUE, 
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 50)

fitcompser$thresholdcomparisons %>% filter(subject==subject2,orLarge==orLarge2)

plot(fitcompser, color = sign) +
  geom_vline(xintercept = 0,lty=2) +
  geom_line(data=fitcomp$curves, aes(x=x,y=y))+
  geom_vline(data=fitcomp$thresholds,aes(xintercept=thre)) +
  geom_point(data=fitcomp$averages,aes(x=orSmall,y=prob)) +xlim(-2,2)
  


fitcompserthrelong <- fitcompser$thresholds %>% 
  select(-threinf, -thresup) %>% spread(orLarge, thre) 


p1<- ggplot(data = fitcompthrelongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Left,y=Right, color ='Top-Bottom', shape = subject)) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

p2<- ggplot(data = fitcompserthrelong ) +
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Left,y=Right, shape =subject, color = factor(sign))) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

p3<- ggplot(data = fitcompthrelongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Top,y=Botton, color ='Top-Bottom', shape = subject)) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

p4<- ggplot(data = fitcompserthrelong ) +
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Top,y=Botton, shape =subject, color = factor(sign))) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

plot_grid(p1,p2,p3,p4)





### comp orLarge 
datsercomp <- dat %>% group_by(subject) %>%
  mutate(lagvertical = lag(vertical), lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  group_by(vertical, lagvertical) %>%
  filter(vertical==lagvertical) %>%
  filter(lagorSmall != 0) %>%
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') 

fitcompser <- quickpsy(datsercomp, orSmall, response,
                       grouping = .(subject, orLarge,sign),
                       guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1)

plot(fitcompser, color = sign)
plotthresholds(fitcompser, color=sign)


fitcompserthrelong <- fitcompser$thresholds %>% 
  select(-threinf, -thresup) %>% spread(orLarge, thre)


p1<- ggplot(data = fitcompthrelongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Left,y=Right, color ='Top-Bottom', shape = subject)) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

p2<- ggplot(data = fitcompserthrelong ) +
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Left,y=Right, shape =subject, color = factor(sign))) +
    coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

p3<- ggplot(data = fitcompthrelongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Top,y=Botton, color ='Top-Bottom', shape = subject)) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

p4<- ggplot(data = fitcompserthrelong ) +
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Top,y=Botton, shape =subject, color = factor(sign))) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

plot_grid(p1,p2,p3,p4)

### equ orLarge 
datsereq <- dat %>% group_by(subject) %>%
  mutate(lagvertical = lag(vertical), lagtask=lag(task),
         lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  group_by(vertical, lagvertical) %>%
  filter(vertical==lagvertical,task==lagtask) %>%
  filter(lagorSmall != 0) %>%
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'equ') 


f <- function(x, p) pnorm(x, p[1] - p[3], p[2]) - pnorm(x, p[1] + p[3], p[2])
fitequcumnormser <- quickpsy(datsereq, orSmall, response,
                          grouping = .(subject,orLarge,sign),
                          B = 1, fun = f,
                          parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                          bootstrap = 'nonparametric', thresholds = F)

plot(fitequcumnormser,color=sign)

pp<-plot_grid(plot(fitcompser, color = sign),plot(fitequcumnormser,color=sign))

save_plot('figures/pp.pdf',pp, base_height = 10,base_width = 30)

fitequserthrelong <- fitequcumnormser$par %>% filter(parn=='p1') %>% 
  select(-parinf, -parsup) %>% spread(orLarge, par)



pe1<- ggplot(data = fiteqpselongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Top,y=Botton, color ='Top-Bottom', shape = subject)) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

pe2<- ggplot(data = fitequserthrelong ) +
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Top,y=Botton, shape =subject, color = factor(sign))) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

pe3<- ggplot(data = fiteqpselongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Left,y=Right, color ='Top-Bottom', shape = subject)) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

pe4<- ggplot(data = fitequserthrelong ) +
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=Left,y=Right, shape =subject, color = factor(sign))) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))


plot_grid(pe1,pe2,pe3,pe4)

fitequserthrelong <-fitequserthrelong %>% mutate(task='equ') %>%
  select(-parn)

fitcompserthrelong <- fitcompserthrelong %>% mutate(task='comp') %>%
  select(-prob)

allser <- rbind(fitequserthrelong,fitcompserthrelong)

allserlong <- allser %>% gather(orLarge,pse,-subject,-task,-sign)

allserwide <- allserlong %>% spread(task,pse)

ggplot(data = allserwide ) + facet_grid(subject~orLarge)+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=comp,y=equ, color = factor(sign))) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))

allserlongdif <- allserlong %>% group_by(subject,task,orLarge) %>% 
  summarise(dif=diff(pse))

allserlongdiflong<- allserlongdif %>% spread(task,dif)

ggplot(data = allserlongdiflong ) +# facet_grid(~orLarge)+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_abline(slope = 0, lty =2, size  = sizeLine1)+
  geom_abline(slope = 99999, lty =2, size  = sizeLine1)+
  geom_point(size=3,aes(x=comp,y=equ, shape=subject,color = factor(orLarge))) +
  coord_equal(xlim = c(-1.5, 1), ylim = c(-1.5,1))



  geom_segment(aes(x = Topinf, xend = Topsup, y = Botton, yend = Botton,
                   color ='Top-Bottom', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Top, xend = Top, y = Bottoninf, yend = Bottonsup,
                   color ='Top-Bottom', shape = subject), size  = sizeLine1) +
  geom_point(aes(x=Right,y=Left, color='Right-Left', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Rightinf, xend = Rightsup, y = Left, yend = Left,
                   color ='Right-Left', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Right, xend = Right, y = Leftinf, yend = Leftsup,
                   color ='Right-Left', shape = subject), size  = sizeLine1) +
  guides(shape = FALSE) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = c('#ff7f00','#999999')) +
  labs(x = 'PSE (deg)', y = 'PSE (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))




pcorcompser1 <- ggplot(data = fitcompserthrelong )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=Top,y=Botton, shape =subject, color = factor(sign))) +
#   geom_segment(aes(x = Topinf, xend = Topsup, y = Botton, yend = Botton,
#                    color ='Top-Bottom', shape = subject), size  = sizeLine1) +
#   geom_segment(aes(x = Top, xend = Top, y = Bottoninf, yend = Bottonsup,
#                    color ='Top-Bottom', shape = subject), size  = sizeLine1) +
  geom_point(aes(x=Right,y=Left, shape=subject, color = factor(sign))) +



#   geom_segment(aes(x = Rightinf, xend = Rightsup, y = Left, yend = Left,
#                    color ='Right-Left', shape = subject), size  = sizeLine1) +
#   geom_segment(aes(x = Right, xend = Right, y = Leftinf, yend = Leftsup,
#                    color ='Right-Left', shape = subject), size  = sizeLine1) +
 # guides(shape = FALSE) +
#  scale_shape_discrete(solid=F) +
 # scale_color_manual(values = c('#ff7f00','#999999')) +
  labs(x = 'PSE (deg)', y = 'PSE (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

plot_grid(pcor1,pcorcompser1)


datsercomp <- dat %>% group_by(subject) %>%
  mutate(lagorLarge = lag(orLarge), lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagorLarge)) %>%
  group_by(orLarge, lagorLarge) %>%
  filter(orLarge==lagorLarge) %>%
  filter(lagorSmall != 0) %>%
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp')

fitcompser <- quickpsy(datsercomp, orSmall, response,
                       grouping = .(subject, sign),
                     #  guess = TRUE, lapses = TRUE, 
                    #   parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1)

plot(fitcompser,panel=subject)



### comp vertical 
datsercomp <- dat %>% group_by(subject) %>%
  mutate(lagvertical = lag(vertical), lagorSmall = lag(orSmall)) %>%
  filter(!is.na(lagvertical)) %>%
  group_by(vertical, lagvertical) %>%
  filter(vertical==lagvertical) %>%
  filter(lagorSmall != 0) %>%
  mutate(sign = ifelse(lagorSmall > 0, 1, -1)) %>%
  filter(task == 'comp') %>% 
  group_by(subject,orLarge,sign) %>%
  mutate(vertical = ifelse(orLarge=='Top' | orLarge=='Botton', TRUE, FALSE))

fitcompser <- quickpsy(datsercomp, orSmall, response,
                       grouping = .(subject, vertical,sign),
                      # guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                      # parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 100)

plot(fitcompser,xpanel=subject,fill=sign)

plotthresholds(fitcompser, color=sign)

plot(fitcompser,xpanel=subject,ypanel=orLarge)


ggplot(fitcompser$averages %>% filter(orLarge=='Top' | orLarge=='Botton'),
        aes(x=orSmall,y=prob,lty=orLarge,color=factor(sign)))+
  facet_wrap(~subject)+
         geom_point()+
  geom_line(data=fitcompser$curves%>% filter(orLarge=='Top' | orLarge=='Botton'),
            aes(x=x,y=y,lty=orLarge,color=factor(sign)))


ggplot(fitcompser$averages %>% filter(orLarge=='Top' | orLarge=='Botton'),
       aes(x=orSmall,y=prob,lty=orLarge,color=factor(sign)))+
  facet_wrap(~subject)+
  geom_point()+
  geom_line(data=fitcompser$averages%>% filter(orLarge=='Top' | orLarge=='Botton'))



datsercomp <- dat %>% group_by(subject) %>%
  mutate(lagvertical = lag(vertical), lagresponse = lag(response)) %>%
  filter(!is.na(lagvertical)) %>%
  group_by(vertical, lagvertical) %>%
  filter(vertical==lagvertical) %>%
  filter(task == 'comp')

fitcompser <- quickpsy(datsercomp, orSmall, response,
                       grouping = .(subject, orLarge, lagresponse),
                       guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                       parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                       bootstrap = 'nonparametric',
                       B = 1)

plot(fitcompser,xpanel=subject,ypanel=orLarge)

### comparisons
fitcompser$thresholdcomparisons %>%
  filter(subject==subject2, orLarge==orLarge2)

  do(print(head(.)))

  filter(!is.na(lagvertical)) %>% do(print(as.data.frame(.)))
  group_by(lagvertical) %>% do(print(.))
  filter(!is.na(lagorSmall), lagorSmall != 0) %>%
  mutate(sign = ifelse(lagorSmall > 0, 1, -1))

datser <- dat %>% ungroup() %>% mutate(lagorSmall=lag(orSmall)) %>%
  filter(!is.na(lagorSmall), lagorSmall != 0) %>%
  mutate(sign = ifelse(lagorSmall > 0, 1, -1))


datcompser <- datser %>% filter(task=='comp') %>%
  mutate()

fitcompser <- quickpsy(datcompser, orSmall, response,
                    grouping = .(subject, orLarge, sign),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1,3), c(0,.4), c(0,.4)),
                    bootstrap = 'nonparametric',
                    B = 1000)

plot(fitcompser,xpanel=subject,ypanel=orLarge)

datm <- datm %>% group_by(name) %>%
  mutate(lagcoh=lag(cohwithsign), lagresp=lag(resp),
         signcoh=ifelse(lagcoh>0,'Preferred','Null')) %>%
  filter(!is.na(lagcoh),!is.na(lagresp),coh != 0)


### HHT NO SYM  psi = . 5 ######################################################
hhtfun <-function(x, p, reference) {
  mu <- p[1] + p[2] * x
  if (reference==0)
    return(pnorm(mu - p[3]) +  p[4] * (pnorm(p[3]- mu) - pnorm(p[5] - mu)))
  if (reference==1)
    return(pnorm(mu  + p[5]) +  (1 - p[4]) *(pnorm(-p[5]- mu) - pnorm(p[5] - mu)))

}

hhtparfun <- function(d) {
  create_nll <- function(d){
    function(p) {
      d1 <- d %>% filter(orLarge=='Top' | orLarge=='Right')
      d2 <- d %>% filter(orLarge=='Botton' | orLarge=='Left')
      phi1 <- hhtfun(d1$orSmall, p, 0)
      phi2 <- hhtfun(d2$orSmall, p, 1)
      phi1[phi1 < .Machine$double.eps] <- .Machine$double.eps
      phi1[phi1 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      phi2[phi2 < .Machine$double.eps] <- .Machine$double.eps
      phi2[phi2 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      sum1 <- -sum(d1$response * log(phi1) +
                     (d1$n - d1$response) * log(1 - phi1) )
      sum2 <- -sum(d2$response * log(phi2) +
                     (d2$n - d2$response) * log(1 - phi2) )

      sum1 + sum2
    }
  }
  nll <- create_nll(d)
  p <- optim(c(0,1,0, 0.5,1), nll, method = 'L-BFGS-B',
             lower = c(-3,.1, -3,-3),
             upper = c( 3,3, 0, 0))$p
  data.frame(p,parn=paste0('p', seq(1, length(p))))
}
hhtpar <- fitcomp$averages %>% group_by(subject, vertical) %>% do(hhtparfun(.))

hhtcurvesfun <- function(d) {
  x <- seq(min(fitcomp$curves$x),max(fitcomp$curves$x),.01)
  y1 <- hhtfun(x, d$p, 0)
  y2 <- hhtfun(x, d$p, 1)
  cond1 <- ifelse(first(d$vertical), 'Top','Right')
  cond2 <- ifelse(first(d$vertical), 'Botton','Left')
  rbind(data.frame(x, y = y1) %>% mutate(orLarge = cond1),
        data.frame(x, y = y2) %>% mutate(orLarge = cond2))
}

hhtcurves <- hhtpar %>% do(hhtcurvesfun(.))

hhtparlong <- hhtpar %>% spread(parn,p)

ggplot()+ facet_grid(subject~vertical)+
  geom_point(data=fitcomp$averages, aes(x=orSmall,y=prob,color=orLarge))+
  geom_line(data=hhtcurves,aes(x=x,y=y,color=orLarge))+
  #geom_vline(data=hhtpar %>% filter(parn=='p3'),aes(xintercept = p))+
  geom_vline(data=hhtpar %>% filter(parn=='p5'),aes(xintercept = p))+
  geom_vline(data = hhtparlong, aes(xintercept = -p1/p2),lty=2)

### pru ########################################################################
garciafun <- function(x,p) {
  mu <- p[1] + p[2] *x
  return(pnorm(mu - p[3]) +  p[4] * (pnorm(p[3]- mu) - pnorm(p[5] - mu)))
}
xseq <- seq(-2,2,.01)
p1 <- c(0, 4.4, 1.5, 0.6, -1)
p2 <- c(0, 4.4, 1, .4, -1.5)
#p1 <- c(2, 4.4, 1, 0.6, -1)
#p2 <- c(2, 4.4, 1, .4, -1)
yseq <- garciafun(xseq,p1)
yseq2 <- garciafun(xseq,p2)
dd <- data.frame(x=xseq,y=yseq,y2=yseq2)

ggplot(dd) +
  geom_line(aes(x=x,y=y)) +
  geom_line(aes(x=x,y=y2),lty=2) +
  geom_vline(xintercept = c(p1[3],p1[5]))+
  ylim(0,1)

### HHT NO SYM  xi = .5 ########################################################
hhtfun <-function(x, p, reference) {
  mu <- p[1] + p[2] * x
  if (reference==0)
    return(pnorm(mu - (p[3]+p[5])) +  .5 * (pnorm(p[3]+p[5]- mu) - pnorm(p[4]+p[5] - mu)))
  if (reference==1)
    return(pnorm(mu  + p[4]+p[5]) +  .5 *(pnorm(-p[4]-p[5]- mu) - pnorm(-p[3]-p[5] - mu)))

}

hhtparfun <- function(d) {
  create_nll <- function(d){
    function(p) {
      d1 <- d %>% filter(orLarge=='Top' | orLarge=='Right')
      d2 <- d %>% filter(orLarge=='Botton' | orLarge=='Left')
      phi1 <- hhtfun(d1$orSmall, p, 0)
      phi2 <- hhtfun(d2$orSmall, p, 1)
      phi1[phi1 < .Machine$double.eps] <- .Machine$double.eps
      phi1[phi1 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      phi2[phi2 < .Machine$double.eps] <- .Machine$double.eps
      phi2[phi2 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      sum1 <- -sum(d1$response * log(phi1) +
                     (d1$n - d1$response) * log(1 - phi1) )
      sum2 <- -sum(d2$response * log(phi2) +
                     (d2$n - d2$response) * log(1 - phi2) )

      sum1 + sum2
    }
  }
  nll <- create_nll(d)
  #p <- optim(c(0,1,-1,1, 0.5), nll)$p
  p <- optim(c(0,1,0, 0.5,1), nll, method = 'L-BFGS-B',
             lower = c(-3,.1, 0,-3,-3),
             upper = c( 3,10, 3, 0,3))$p
  data.frame(p,parn=paste0('p', seq(1, length(p))))
}
hhtpar <- fitcomp$averages %>% group_by(subject, vertical) %>% do(hhtparfun(.))

hhtcurvesfun <- function(d) {
  x <- seq(min(fitcomp$curves$x),max(fitcomp$curves$x),.01)
  y1 <- hhtfun(x, d$p, 0)
  y2 <- hhtfun(x, d$p, 1)
  cond1 <- ifelse(first(d$vertical), 'Top','Right')
  cond2 <- ifelse(first(d$vertical), 'Botton','Left')
  rbind(data.frame(x, y = y1) %>% mutate(orLarge = cond1),
        data.frame(x, y = y2) %>% mutate(orLarge = cond2))
}

hhtcurves <- hhtpar %>% do(hhtcurvesfun(.))

hhtparlong <- hhtpar %>% spread(parn,p)

ggplot()+ facet_grid(subject~vertical)+
  geom_point(data=fitcomp$averages, aes(x=orSmall,y=prob,color=orLarge))+
  geom_line(data=hhtcurves,aes(x=x,y=y,color=orLarge))+
  geom_vline(data=hhtpar %>% filter(parn=='p3'),aes(xintercept = p))+
  geom_vline(data=hhtpar %>% filter(parn=='p4'),aes(xintercept = p))+
  geom_vline(data=hhtpar %>% filter(parn=='p5'),aes(xintercept = p))




### HHT NO SYM  xi = .5 ########################################################
hhtfun <-function(x, p, reference) {
  mu <- p[1] + p[2] * x
  if (reference==0)
    return(pnorm(mu - p[3]) +  .5 * (pnorm(p[3]- mu) - pnorm(p[4] - mu)))
  if (reference==1)
    return(pnorm(mu  + p[4]) +  .5 *(pnorm(-p[4]- mu) - pnorm(-p[3] - mu)))

}

hhtparfun <- function(d) {
  create_nll <- function(d){
    function(p) {
      d1 <- d %>% filter(orLarge=='Top' | orLarge=='Right')
      d2 <- d %>% filter(orLarge=='Botton' | orLarge=='Left')
      phi1 <- hhtfun(d1$orSmall, p, 0)
      phi2 <- hhtfun(d2$orSmall, p, 1)
      phi1[phi1 < .Machine$double.eps] <- .Machine$double.eps
      phi1[phi1 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      phi2[phi2 < .Machine$double.eps] <- .Machine$double.eps
      phi2[phi2 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      sum1 <- -sum(d1$response * log(phi1) +
                     (d1$n - d1$response) * log(1 - phi1) )
      sum2 <- -sum(d2$response * log(phi2) +
                     (d2$n - d2$response) * log(1 - phi2) )

      sum1 + sum2
    }
  }
  nll <- create_nll(d)
  #p <- optim(c(0,1,-1,1, 0.5), nll)$p
  p <- optim(c(0,1,0, 0.5,1), nll, method = 'L-BFGS-B',
             lower = c(-3,.1, -3,-3),
             upper = c( 3,10, 3, 3))$p
  data.frame(p,parn=paste0('p', seq(1, length(p))))
}
hhtpar <- fitcomp$averages %>% group_by(subject, vertical) %>% do(hhtparfun(.))

hhtcurvesfun <- function(d) {
  x <- seq(min(fitcomp$curves$x),max(fitcomp$curves$x),.01)
  y1 <- hhtfun(x, d$p, 0)
  y2 <- hhtfun(x, d$p, 1)
  cond1 <- ifelse(first(d$vertical), 'Top','Right')
  cond2 <- ifelse(first(d$vertical), 'Botton','Left')
  rbind(data.frame(x, y = y1) %>% mutate(orLarge = cond1),
        data.frame(x, y = y2) %>% mutate(orLarge = cond2))
}

hhtcurves <- hhtpar %>% do(hhtcurvesfun(.))

hhtparlong <- hhtpar %>% spread(parn,p)

ggplot()+ facet_grid(subject~vertical)+
  geom_point(data=fitcomp$averages, aes(x=orSmall,y=prob,color=orLarge))+
  geom_line(data=hhtcurves,aes(x=x,y=y,color=orLarge))+
  geom_vline(data=hhtpar %>% filter(parn=='p3'),aes(xintercept = p))+
  geom_vline(data=hhtpar %>% filter(parn=='p5'),aes(xintercept = p))+
  geom_vline(data = hhtparlong, aes(xintercept = -p1/p2),lty=2)





### HHT SYM ####################################################################
hhtfun <-function(x, p, reference) {
  mu <- p[1] + p[2] * x
  if (reference==0)
    return(pnorm(mu - p[3]) +  p[4] * (pnorm(p[3]- mu) - pnorm(-p[3] - mu)))
  if (reference==1)
    return(pnorm(mu - p[3]) +  (1 - p[4]) *(pnorm(p[3]- mu) - pnorm(-p[3] - mu)))

}


hhtparfun <- function(d) {
  create_nll <- function(d){
    function(p) {
      d1 <- d %>% filter(orLarge=='Top' | orLarge=='Right')
      d2 <- d %>% filter(orLarge=='Botton' | orLarge=='Left')
      phi1 <- hhtfun(d1$orSmall, p, 0)
      phi2 <- hhtfun(d2$orSmall, p, 1)
      phi1[phi1 < .Machine$double.eps] <- .Machine$double.eps
      phi1[phi1 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      phi2[phi2 < .Machine$double.eps] <- .Machine$double.eps
      phi2[phi2 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      sum1 <- -sum(d1$response * log(phi1) +
                     (d1$n - d1$response) * log(1 - phi1) )
      sum2 <- -sum(d2$response * log(phi2) +
                     (d2$n - d2$response) * log(1 - phi2) )

      sum1 + sum2
    }
  }
  nll <- create_nll(d)
  #p <- optim(c(0,1,-1,1, 0.5), nll)$p
  p <- optim(c(0,1,0, 0.55), nll, method = 'L-BFGS-B',
             lower = c(-3,.1, -10,  0),
             upper = c(3,  10, 10,1))$p
  data.frame(p,parn=paste0('p', seq(1, length(p))))
}
hhtpar <- fitcomp$averages %>% group_by(subject, vertical) %>% do(hhtparfun(.))

hhtcurvesfun <- function(d) {
  x <- seq(min(fitcomp$curves$x),max(fitcomp$curves$x),.01)
  y1 <- hhtfun(x, d$p, 0)
  y2 <- hhtfun(x, d$p, 1)
  cond1 <- ifelse(first(d$vertical), 'Top','Right')
  cond2 <- ifelse(first(d$vertical), 'Botton','Left')
  rbind(data.frame(x, y = y1) %>% mutate(orLarge = cond1),
        data.frame(x, y = y2) %>% mutate(orLarge = cond2))
}

hhtcurves <- hhtpar %>% do(hhtcurvesfun(.))

ggplot()+ facet_grid(subject~vertical)+
  geom_point(data=fitcomp$averages, aes(x=orSmall,y=prob,color=orLarge))+
  geom_line(data=hhtcurves,aes(x=x,y=y,color=orLarge))




### HHT NO SYM #################################################################
hhtfun <-function(x, p, reference) {
  mu <- p[1] + p[2] * x
  if (reference==0)
    return(pnorm(mu - p[3]) +  p[4] * (pnorm(p[3]- mu) - pnorm(p[5] - mu)))
  if (reference==1)
    return(pnorm(mu  + p[5]) +  (1 - p[4]) *(pnorm(-p[5]- mu) - pnorm(-p[3] - mu)))

}

hhtparfun <- function(d) {
  create_nll <- function(d){
    function(p) {
      d1 <- d %>% filter(orLarge=='Top' | orLarge=='Right')
      d2 <- d %>% filter(orLarge=='Botton' | orLarge=='Left')
      phi1 <- hhtfun(d1$orSmall, p, 0)
      phi2 <- hhtfun(d2$orSmall, p, 1)
      phi1[phi1 < .Machine$double.eps] <- .Machine$double.eps
      phi1[phi1 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      phi2[phi2 < .Machine$double.eps] <- .Machine$double.eps
      phi2[phi2 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      sum1 <- -sum(d1$response * log(phi1) +
                     (d1$n - d1$response) * log(1 - phi1) )
      sum2 <- -sum(d2$response * log(phi2) +
                     (d2$n - d2$response) * log(1 - phi2) )

      sum1 + sum2
    }
  }
  nll <- create_nll(d)
  #p <- optim(c(0,1,-1,1, 0.5), nll)$p
  p <- optim(c(0,1,0, 0.5,1), nll, method = 'L-BFGS-B',
             lower = c(-3,.1, -3,  0,-3),
             upper = c( 3,10, 0, 1, 0))$p
  data.frame(p,parn=paste0('p', seq(1, length(p))))
}
hhtpar <- fitcomp$averages %>% group_by(subject, vertical) %>% do(hhtparfun(.))

hhtcurvesfun <- function(d) {
  x <- seq(min(fitcomp$curves$x),max(fitcomp$curves$x),.01)
  y1 <- hhtfun(x, d$p, 0)
  y2 <- hhtfun(x, d$p, 1)
  cond1 <- ifelse(first(d$vertical), 'Top','Right')
  cond2 <- ifelse(first(d$vertical), 'Botton','Left')
  rbind(data.frame(x, y = y1) %>% mutate(orLarge = cond1),
        data.frame(x, y = y2) %>% mutate(orLarge = cond2))
}

hhtcurves <- hhtpar %>% do(hhtcurvesfun(.))

hhtparlong <- hhtpar %>% spread(parn,p)

ggplot()+ facet_grid(subject~vertical)+
  geom_point(data=fitcomp$averages, aes(x=orSmall,y=prob,color=orLarge))+
  geom_line(data=hhtcurves,aes(x=x,y=y,color=orLarge))+
 geom_vline(data=hhtpar %>% filter(parn=='p3'),aes(xintercept = p))+
  geom_vline(data=hhtpar %>% filter(parn=='p5'),aes(xintercept = p))+
  geom_vline(data = hhtparlong, aes(xintercept = -p1/p2),lty=2)












hhtfun <-function(x, p, reference) {
  mu <- p[1] + p[2] * x
  if (reference==0)
    return(pnorm(mu - p[3]) +  p[5] *(pnorm(p[3]- mu) - pnorm(p[4] - mu)))
  if (reference==1)
    return(pnorm(mu + p[4]) +  (1 - p[5]) *(pnorm(-p[4]- mu) - pnorm(-p[3] - mu)))

}


hhtparfun <- function(d) {
  create_nll <- function(d){
    function(p) {
      d1 <- d %>% filter(orLarge=='Top' | orLarge=='Right')
      d2 <- d %>% filter(orLarge=='Botton' | orLarge=='Left')
      phi1 <- hhtfun(d1$orSmall, p, 0)
      phi2 <- hhtfun(d2$orSmall, p, 1)
      phi1[phi1 < .Machine$double.eps] <- .Machine$double.eps
      phi1[phi1 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      phi2[phi2 < .Machine$double.eps] <- .Machine$double.eps
      phi2[phi2 > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
      sum1 <- -sum(d1$response * log(phi1) +
                     (d1$n - d1$response) * log(1 - phi1) )
      sum2 <- -sum(d2$response * log(phi2) +
                     (d2$n - d2$response) * log(1 - phi2) )

      sum1 + sum2
    }
  }
  nll <- create_nll(d)
 #p <- optim(c(0,1,-1,1, 0.5), nll)$p
  p <- optim(c(0,1,-2,2, 0.55), nll, method = 'L-BFGS-B',
             lower = c(-3,.1, -10, -10, 0),
             upper = c(3,  10, 10, 10,1))$p
  data.frame(p,parn=paste0('p', seq(1, length(p))))
}
hhtpar <- fitcomp$averages %>% group_by(subject, vertical) %>% do(hhtparfun(.))

hhtcurvesfun <- function(d) {
  x <- seq(min(fitcomp$curves$x),max(fitcomp$curves$x),.01)
  y1 <- hhtfun(x, d$p, 0)
  y2 <- hhtfun(x, d$p, 1)
  cond1 <- ifelse(first(d$vertical), 'Top','Right')
  cond2 <- ifelse(first(d$vertical), 'Botton','Left')
  rbind(data.frame(x, y = y1) %>% mutate(orLarge = cond1),
        data.frame(x, y = y2) %>% mutate(orLarge = cond2))
}

hhtcurves <- hhtpar %>% do(hhtcurvesfun(.))

ggplot()+ facet_grid(subject~vertical)+
  geom_point(data=fitcomp$averages, aes(x=orSmall,y=prob,color=orLarge))+
  geom_line(data=hhtcurves,aes(x=x,y=y,color=orLarge))






+
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(data = sdtperceptualbiasparlong, aes(xintercept = -(p3-p2)/p1)) +
  geom_vline(data = sdtperceptualbiasparlong, aes(xintercept = -(p3+p2)/p1)) +
  geom_vline(data = sdtperceptualbiasparlong, aes(xintercept = -p3/p1))








