plotting_asym <- function(d, flagVertical, flagOrder) {
  ### max
  maxi <- d$curves %>% summarise(maxi = approx(x = y, y = x, xout = max(y))[[1]])
  d$par <- d$par %>% left_join(maxi)
  
  print(d$par)
  colorcurves1 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  colorcurves2 <- ifelse(flagVertical,'#377eb8','#984ea3')
  colorchoicebiascurves <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  ggplot(d$averages %>% filter(vertical==flagVertical),
         aes(x = orSmall, y = prob, color = orLarge, shape=orLarge)) +
    facet_wrap(~subject,scales = 'free_x', ncol = 4) +
    geom_vline(xintercept = 0, lty = 2, size  = size_line)+
    geom_point(size = size_point) +
    geom_line(data = d$curves %>% filter(vertical==flagVertical),
              aes(x = x, y = y), size  = size_line) +
    geom_segment(data = d$par %>% filter(parn =='p1') %>% 
                   filter(vertical==flagVertical),
                aes(x = par, xend = par, y = 0, yend = maxi, color = orLarge), 
                size  = size_line) +
    scale_color_manual(values = c(colorcurves1, colorcurves2)) +
    guides(color = guide_legend(reverse=flagOrder),
           shape = guide_legend(reverse=flagOrder)) +
    labs(x = text_orientation, y = text_prob_asym,
         color = text_reference, shape = text_reference) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    coord_cartesian(xlim=c(-2.1, 2.1),ylim=c(0,1)) +
    theme(legend.position = c(.9,.07),
          strip.background = element_blank(),
          axis.line = element_line(size = size_line),
          axis.ticks= element_line(size = size_line))
  
  
  # colorcurves1 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  # colorcurves2 <- ifelse(flagVertical,'#377eb8','#984ea3')
  # colorchoicebiascurves <- ifelse(flagVertical,'#377eb8','#984ea3')
  # ggplot() +
  #   facet_wrap(~subject,scales = 'free_x') +
  #   geom_vline(xintercept = 0, lty = 2, size  = sizeLine1)+
  #   geom_rect(data = fitcomp$thresholds %>%
  #               filter(vertical==flagVertical),
  #             aes(xmin=threinf,xmax=thresup,ymin=0, ymax=1, fill=orLarge),
  #             show.legend = FALSE,alpha = .25) +
  #   #     geom_segment(data = fitcomp$thresholds %>%
  #   #                    filter(vertical==flagVertical),
  #   #                  aes(x=threinf,xend = thresup, y = 0, yend = 0,
  #   #                      color=orLarge), show.legend = FALSE,
  #   #                  alpha = .25, size = 2.25) +
  #   geom_point(data=fitequcumnorm$averages %>% filter(vertical==flagVertical),
  #              aes(x = orSmall, y = prob, color = orLarge, shape=orLarge),
  #              size = sizePoint1) +
    # geom_segment(data = pse %>% filter(vertical==flagVertical),
    #              aes(x = parinf, xend = parsup, y = 0, yend = 0,
    #                  color = orLarge),size  = sizeLine1) +
  #   geom_segment(data = pse %>% filter(vertical==flagVertical),
  #                aes(x = par, xend = par, y = 0, yend = maxi,
  #                    color = orLarge),size  = sizeLine1) +
  #   geom_line(data = fitequcumnorm$curves %>% filter(vertical==flagVertical),
  #             aes(x = x, y = y, color = orLarge),
  #             size  = sizeLine1) +
  #   scale_color_manual(values = c(colorcurves1,colorcurves2)) +
  #   guides(color = guide_legend(reverse=flagOrder),
  #          shape = guide_legend(reverse=flagOrder)) +
  #   labs(x = 'Orientation (deg)', y = textProb2,
  #        color = textReference, shape = textReference) +
  #   scale_y_continuous(breaks = c(0,.5,1)) +
  #   coord_cartesian(xlim=c(-2.1, 2.1),ylim=c(0,1)) +
  #   theme(strip.background = element_blank())
  
}

