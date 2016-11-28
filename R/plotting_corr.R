plotting_corr <- function(d) {
  ggplot(data = d)+
    geom_abline(slope = 1, lty =2, size  = size_line)+
    geom_abline(slope = -1, lty =2, size  = size_line)+
    geom_point(aes(x=Top,y=Bottom, color ='Top-Bottom')) +
    geom_point(aes(x=Right,y=Left, color='Right-Left')) +
    guides(shape = FALSE) +
    scale_shape_discrete(solid=F) +
    scale_color_manual(values = c('#ff7f00','#999999')) +
    labs(x = 'PND (deg)', y = 'PND (deg)') +
    theme(legend.title = element_blank()) +
    scale_x_continuous(breaks =seq(-1,1,.5),
                       labels = c('-1','-0.5','0','0.5','1')) +
    scale_y_continuous(breaks =seq(-1,1,.5),
                       labels = c('-1','-0.5','0','0.5','1')) +
    coord_equal(xlim = c(-1.25, 1), ylim = c(-1.25, 1)) +
    theme(axis.line = element_line(size = size_line),
          axis.ticks= element_line(size = size_line))
}