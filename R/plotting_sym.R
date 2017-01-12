plotting_sym <- function(d, dsub1, dsub2, dsub3, flagVertical, flagOrder) {
  ### choice bias prediction
  choicebiascurves <- d$curves %>%
    filter(orLarge=='Top' | orLarge=='Right' ) %>%
    left_join(d$par %>% filter(parn == 'p1')) %>%
    mutate(x = x - 2*par) 
  
  colorcurves1 <- ifelse(flagVertical,'#377eb8','#984ea3')
  colorcurves2 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  colorchoicebiascurves <- ifelse(flagVertical,'#377eb8','#984ea3')
  ggplot(d$averages %>% filter(vertical==flagVertical)) +
    facet_wrap(~subject,scales = 'free_x', ncol = 6) +
    geom_vline(xintercept = 0, lty = 2, size  = size_line)+
    geom_point(size = size_point,
               aes(x = orSmall, y = prob, color = orLarge, shape=orLarge)) +
    geom_line(data = d$curves %>% filter(vertical==flagVertical),
              aes(x = x, y = y, color = orLarge), size  = size_line) +
    geom_line(data = choicebiascurves %>% filter(vertical==flagVertical),
              aes(x = x, y = y), lty =2, size  = size_line, 
              color = colorchoicebiascurves) +
    geom_segment(data = d$thresholds %>%
                   filter(vertical==flagVertical),
                 aes(x=threinf,xend = thresup, y = .5, yend = 0.5,
                     color=orLarge), size  = size_line) +
    geom_text(data = dsub1 %>% filter(vertical == flagVertical),
             x = -1.5, y = .9, label = 'S', size = 2) +
    geom_text(data = dsub2 %>% filter(vertical == flagVertical),
              x = -1.5, y = .9, label = 'C', size = 2) +
    geom_text(data = dsub3 %>% filter(vertical == flagVertical),
              x = -1.5, y = .9, label = 'Sc', size = 2) +
    scale_color_manual(values = c(colorcurves1, colorcurves2)) +
    guides(color = guide_legend(reverse=flagOrder),
           shape = guide_legend(reverse=flagOrder)) +
    labs(x = text_orientation, y = text_prob_sym,
         color = text_reference, shape = text_reference) +
    scale_y_continuous(breaks = c(0, .5, 1), limits = c(0,1)) +
    scale_x_continuous(breaks = -2:2, labels = c('','-1','0','1',''), 
                       limits = c(-2.1, 2.1)) +
    theme(legend.position = c(.926,.1),
          legend.box.background = element_rect(),
          legend.margin = margin(.2, .2, .2, .2, 'line'), 
          strip.background = element_blank(),
          axis.line = element_line(size = size_line),
          axis.ticks= element_line(size = size_line))
  
}

