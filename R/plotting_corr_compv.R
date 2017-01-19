plotting_corr_compv <- function(d) {
  d$vertical <- factor(d$vertical, levels = c(TRUE, FALSE), 
                       labels = c('Vertical', 'Horizontal'))
  ggplot(data = d )+
    geom_abline(slope = 1, lty =2, size  = size_line) +
    geom_point(size = 1.5, aes(x = thre,y = par, 
                             color = vertical, shape = vertical))+
    labs(x = 'PMR asym. task (deg)', 
         y = 'Bias sym. task (deg)') +
    scale_x_continuous(breaks =seq(-1,1,.5),
                       labels = c('-1','-0.5','0','0.5','1')) +
    scale_y_continuous(breaks =seq(-1,1,.5),
                       labels = c('-1','-0.5','0','0.5','1')) +
    scale_color_manual(values = c('#ff7f00','#a65628')) +
    coord_equal(xlim = c(-1.25, 1), ylim = c(-1.25, 1)) +
    theme(legend.position = c(.75,.2),
          legend.title = element_blank(),
          legend.box.background = element_rect(),
          axis.line = element_line(size = size_line),
          axis.ticks= element_line(size = size_line),
          legend.margin = margin(-.3, 0.2, 0, 0, 'line'), 
          plot.margin = margin(1, 0.5, 1, 1, 'line'))
}