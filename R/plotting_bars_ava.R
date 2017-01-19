plotting_bars_ava <- function(d, d2) {
  d$orLarge <- factor(d$orLarge, levels = c('Right', 'Left', 'Top', 'Bottom'))
  d2$orLarge <- factor(d2$orLarge, levels = c('Right', 'Left', 'Top', 'Bottom'))

   ggplot(d) + 
    geom_col(aes(x = orLarge, y = thre,
                 fill = orLarge),
             width=0.7, position = position_dodge(0.75)) +
    geom_point(data = d2, aes(x = orLarge, y = thre),
               color = 'grey', shape = 21,
               size = 1, position = position_jitter(width = .15)) +
    geom_errorbar(aes(x = orLarge, ymin = threinf, ymax = thresup,
                      group = orLarge), size = size_line,
                   position = position_dodge(0.75), width = .5) +
    scale_fill_manual(values = c('#4daf4a','#984ea3','#e41a1c','#377eb8')) +
    guides(fill = guide_legend(reverse = TRUE)) +
    ylim(-.75, 1.3) +
    labs(x = text_reference, y = 'Sign-corrected PMR (deg)') +
    theme(legend.position = 'none',
          axis.line =  element_line(size = size_line),
          axis.ticks = element_line(size = size_line),
          plot.margin = margin(1, 0.5, 1, 1, 'line'))
  
  
}