plotting_bars <- function(d, flagVertical) {
  
  thre_long_pred_bottom_pred <- d %>% filter(orLarge == 'Top') %>% 
    mutate(orLarge = 'Bottom prediction', 
           thre = -thre, threinf = -threinf, thresup = -thresup, pred = TRUE)
  thre_long_pred_left_pred <- d %>% filter(orLarge == 'Right') %>% 
    mutate(orLarge = 'Left prediction', 
           thre = -thre, threinf = -threinf, thresup = -thresup, pred = TRUE)
  
  d <- rbind(d, thre_long_pred_bottom_pred, thre_long_pred_left_pred) %>% 
    arrange(subject) %>% mutate(subject = as.factor(subject)) 
  
  d$orLarge <- factor(d$orLarge,
                      levels = c('Top', 'Bottom', 'Bottom prediction',
                                 'Right', 'Left', 'Left prediction'))
  
  fillbar1 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  fillbar2 <- ifelse(flagVertical,'#377eb8','#984ea3')
  fillbar3 <- 'white'
  
  psymbias <- ggplot(d %>% filter(vertical == flagVertical)) + 
    geom_col(aes(x = subject, y = thre, fill = orLarge, color = orLarge), 
             width=0.7, position = position_dodge(0.75)) +
    geom_linerange(aes(x = subject, ymin = threinf, ymax = thresup, 
                       group = orLarge, lty = pred), size = size_line, 
                   position = position_dodge(0.75)) +
    scale_fill_manual(values = c(fillbar2, fillbar1, fillbar3)) +
    scale_color_manual(values = c(fillbar2, fillbar1, fillbar1)) +
    guides(lty = FALSE) +
    labs(x = 'Participant',y = 'PND (deg)', 
         color = text_reference, fill = text_reference) +
    theme(legend.position = 'top',
          axis.line = element_line(size = size_line),
          axis.ticks= element_line(size = size_line))
  
}