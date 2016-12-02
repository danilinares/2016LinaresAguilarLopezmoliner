plotting_bars2 <- function(d, flagMix, flagAverage) {
  d$orLarge <- factor(d$orLarge,
                      levels = c('Top', 'Bottom', 'Bottom prediction'))
  
  fillbar1 <- ifelse(flagMix,'#e41a1c','#4daf4a')
  fillbar2 <- ifelse(flagMix,'#377eb8','#984ea3')
  fillbar3 <- 'white'
  
  if (flagAverage) legend_dir <- 'vertical'
  else legend_dir <- 'horizontal'
  psymbias <- ggplot(d %>% filter(mix == flagMix)) + 
    geom_col(aes(x = subject, y = thre, fill = orLarge, color = orLarge), 
             width=0.7, position = position_dodge(0.75)) +
    geom_linerange(aes(x = subject, ymin = threinf, ymax = thresup, 
                       group = orLarge, lty = pred), size = size_line, 
                   position = position_dodge(0.75)) +
    scale_fill_manual(values = c(fillbar2, fillbar1, fillbar3)) +
    scale_color_manual(values = c(fillbar2, fillbar1, fillbar1)) +
    guides(lty = FALSE) +
   # ylim(-1.27, 1.27) +
    labs(x = 'Participant',y = 'PND (deg)', 
         color = text_reference, fill = text_reference) +
    theme(legend.position = 'top',
          legend.direction = legend_dir,
          axis.line = element_line(size = size_line),
          axis.ticks= element_line(size = size_line))
  
}