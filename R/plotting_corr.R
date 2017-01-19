plotting_corr <- function(d, x, y) {
  corr <- cor.test(d[[x]], d[[y]]) 
  r <- corr$estimate %>% round(2)
  p <- corr$p.value %>% signif(1)
  
  ggplot()+
    geom_abline(slope = 1, lty =2, size  = size_line)+
    geom_abline(slope = -1, lty =2, size  = size_line)+
    geom_point(data = d, aes_string(x = x, y = y), size = .6) +
    guides(shape = FALSE) +
    scale_shape_discrete(solid=F) +
    scale_color_manual(values = c('#ff7f00','#999999')) +
    labs(x = paste(x, '(deg)'), y = paste(y, '(deg)')) +
    ggtitle(paste0('r=',r,', P=',p)) +
    theme(legend.title = element_blank()) +
    scale_x_continuous(limits = c(-1.3, 1.3),
                       breaks =seq(-1,1,1)) +
                      # labels = c('-1','-0.5','0','0.5','1')) +
    scale_y_continuous(limits =c(-1.3, 1.3),
                       breaks =seq(-1,1,1)) +
                      # labels = c('-1','-0.5','0','0.5','1')) +
    coord_equal() +
    theme(axis.line = element_line(size = size_line),
          axis.ticks= element_line(size = size_line),
         # legend.margin = margin(-.52, -.52, -.52, -.52, 'line'), 
          plot.title = element_text(size = 8))
}