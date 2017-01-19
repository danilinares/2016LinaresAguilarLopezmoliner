plotting_asym <- function(d, dsub1, flagVertical, flagOrder) {
  ### max
  maxi <- d$curves %>% summarise(maxi = approx(x = y, y = x, xout = max(y))[[1]])
  d$par <- d$par %>% left_join(maxi)
  
  colorcurves1 <- ifelse(flagVertical,'#377eb8','#984ea3')
  colorcurves2 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  colorchoicebiascurves <- ifelse(flagVertical,'#377eb8','#984ea3')
  ggplot(d$averages %>% filter(vertical==flagVertical)) +
    facet_wrap(~subject,scales = 'free_x', ncol = 6) +
    geom_vline(xintercept = 0, lty = 2, size  = size_line)+
    geom_point(aes(x = orSmall, y = prob, color = orLarge, shape=orLarge),
               size = size_point) +
    geom_line(data = d$curves %>% filter(vertical==flagVertical),
              aes(x = x, y = y, color = orLarge), size  = size_line) +
    geom_segment(data = d$par %>% filter(parn =='p1', vertical==flagVertical),
                aes(x = par, xend = par, y = 0, yend = maxi, color = orLarge), 
                size  = size_line) +
    geom_segment(data = d$par %>% filter(parn =='p1', vertical==flagVertical),
                 aes(x=parinf,xend = parsup, y = .15, yend = 0.15,
                     color=orLarge), size  = size_line) +
    geom_text(data = dsub1 %>% filter(vertical == flagVertical),
              x = -1.8, y = .8, label = '*', size = 4) +
    scale_color_manual(values = c(colorcurves1, colorcurves2)) +
    guides(color = guide_legend(reverse=flagOrder),
           shape = guide_legend(reverse=flagOrder)) +
    labs(x = text_orientation, y = text_prob_asym,
         color = text_reference, shape = text_reference) +
    scale_y_continuous(breaks = c(0, .5, 1), limits = c(0,1)) +
    scale_x_continuous(breaks = -2:2, labels = c('','-1','0','1',''), 
                       limits = c(-2.1, 2.1)) +
    theme(legend.position = c(.6,.1),
          legend.box.background = element_rect(),
          legend.margin = margin(.2, .2, .2, .2, 'line'), 
          strip.background = element_blank(),
          axis.title.x = element_text(hjust = .2),
          axis.line = element_line(size = size_line),
          axis.ticks= element_line(size = size_line))
  
  
}

