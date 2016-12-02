plotting_sym2 <- function(d, flagMix, flagOrder) {
  ### choice bias prediction
  choicebiascurves <- d$curves %>%
    filter(orLarge=='Top' | orLarge=='Right' ) %>%
    merge(d$par %>% filter(parn == 'p1')) %>%
    mutate(x = x - 2*par) 
  
  colorcurves1 <- ifelse(flagMix,'#e41a1c','#4daf4a')
  colorcurves2 <- ifelse(flagMix,'#377eb8','#984ea3')
  colorchoicebiascurves <- ifelse(flagMix,'#e41a1c','#4daf4a')
  ggplot(d$averages %>% filter(mix==flagMix),
         aes(x = orSmall, y = prob, color = orLarge, shape=orLarge)) +
    facet_wrap(~subject,scales = 'free_x', ncol = 4) +
    geom_vline(xintercept = 0, lty = 2, size  = size_line)+
    geom_point(size = size_point) +
    geom_line(data = d$curves %>% filter(mix==flagMix),
              aes(x = x, y = y), size  = size_line) +
    geom_line(data = choicebiascurves %>% filter(mix==flagMix),
              aes(x = x, y = y), lty =2, size  = size_line, 
              color = colorchoicebiascurves) +
    geom_segment(data = d$thresholds %>%
                   filter(mix==flagMix),
                 aes(x=threinf,xend = thresup, y = .5, yend = 0.5,
                     color=orLarge), size  = size_line) +
    scale_color_manual(values = c(colorcurves1, colorcurves2)) +
    guides(color = guide_legend(reverse=flagOrder),
           shape = guide_legend(reverse=flagOrder)) +
    labs(x = text_orientation, y = text_prob_sym,
         color = text_reference, shape = text_reference) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    coord_cartesian(xlim=c(-2.1, 2.1),ylim=c(0,1)) +
    theme(legend.position = c(.9,.07),
          strip.background = element_blank(),
          axis.line = element_line(size = size_line),
          axis.ticks= element_line(size = size_line))
  
}

