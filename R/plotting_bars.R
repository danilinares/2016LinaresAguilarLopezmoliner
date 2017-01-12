plotting_bars <- function(d, flagVertical, flagAverage) {
  if (flagVertical){
    d$orLarge <- factor(d$orLarge,
                        levels = c('Top', 'Bottom', 'Bottom prediction'), 
                        labels = c('Top', 'Bottom', 'Choice\nbias\nprediction'))    
  }
  else{
    d$orLarge <- factor(d$orLarge,
                        levels = c('Right', 'Left', 'Left prediction'), 
                        labels = c('Right', 'Left', 'Choice\nbias\nprediction'))    
  }

  
  fillbar1 <- ifelse(flagVertical,'#e41a1c','#984ea3')
  fillbar2 <- ifelse(flagVertical,'#377eb8','#4daf4a')
  fillbar3 <- 'white'
  
  if (flagAverage) legend_dir <- 'vertical'
  else legend_dir <- 'horizontal'
  
  psymbias <- ggplot(d %>% filter(vertical == flagVertical)) + 
    geom_col(aes(x = orLarge, y = thre, 
                 fill = orLarge, color = orLarge, lty = orLarge), 
             width=0.7, position = position_dodge(0.75)) +
    geom_linerange(aes(x = orLarge, ymin = threinf, ymax = thresup,
                       group = orLarge), size = size_line,
                   position = position_dodge(0.75)) +
    scale_fill_manual(values = c(fillbar2, fillbar1, fillbar3)) +
    scale_color_manual(values = c(fillbar2, fillbar1, fillbar1)) +
    scale_linetype_manual(values = c(1, 1, 2)) +
   # guides(lty = FALSE) +
    labs(x = text_reference, y = 'PND (deg)') +
    theme(legend.position = 'none',
          legend.direction = legend_dir,
          axis.line =  element_line(size = size_line),
          axis.ticks = element_line(size = size_line),
          plot.margin = margin(2, 0.5, 2, 1, 'line'))
  
  psymbias
  
}