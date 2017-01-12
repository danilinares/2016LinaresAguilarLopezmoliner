plotting_bars_av <- function(d, d2, flagVertical) {
  if (flagVertical){
    d$orLarge <- factor(d$orLarge,
                        levels = c('Top', 'Bottom', 'Bottom prediction'), 
                        labels = c('Top', 'Bottom', 'Choice\nbias\nprediction'))  
    d2$orLarge <- factor(d2$orLarge,
                        levels = c('Top', 'Bottom', 'Bottom prediction'), 
                        labels = c('Top', 'Bottom', 'Choice\nbias\nprediction'))
  }
  else{
    d$orLarge <- factor(d$orLarge,
                        levels = c('Right', 'Left', 'Left prediction'), 
                        labels = c('Right', 'Left', 'Choice\nbias\nprediction'))  
    d2$orLarge <- factor(d2$orLarge,
                        levels = c('Right', 'Left', 'Left prediction'), 
                        labels = c('Right', 'Left', 'Choice\nbias\nprediction')) 
  }

  
  fillbar1 <- ifelse(flagVertical,'#377eb8','#984ea3')
  fillbar2 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  fillbar3 <- 'white'
  
  
  psymbias <- ggplot(d %>% filter(vertical == flagVertical)) + 
    geom_col(aes(x = orLarge, y = thre,
                 fill = orLarge, color = orLarge, lty = orLarge),
             width=0.7, position = position_dodge(0.75)) +
    # geom_point(aes(x = orLarge, y = thre, 
    #             color = orLarge)) +
    geom_point(data = d2 %>% filter(vertical == flagVertical, !pred),
               aes(x = orLarge, y = thre),
               color = 'grey', shape = 21,
               size = 1, position = position_jitter(width = .15)) +
    geom_errorbar(aes(x = orLarge, ymin = threinf, ymax = thresup,
                       group = orLarge), size = size_line,
                   position = position_dodge(0.75), width = .5) +
    scale_fill_manual(values = c(fillbar2, fillbar1, fillbar3)) +
    scale_color_manual(values = c(fillbar2, fillbar1, fillbar1)) +
    scale_linetype_manual(values = c(1, 1, 2)) +
    ylim(-.75, 1) +
   # guides(lty = FALSE) +
    labs(x = text_reference, y = 'Sign-corrected bias (deg)') +
    theme(legend.position = 'none',
          axis.line =  element_line(size = size_line),
          axis.ticks = element_line(size = size_line),
          plot.margin = margin(1, 0.5, 1, 1, 'line'))
  
  psymbias
  
}