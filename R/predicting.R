predicting <- function(d) {
  d <- d %>% mutate(pred = FALSE) 
  
  thre_long_pred_bottom_pred <- d %>% filter(orLarge == 'Top') %>% 
    mutate(orLarge = 'Bottom prediction', 
           thre = -thre, threinf = -threinf, thresup = -thresup, pred = TRUE)
  
  thre_long_pred_left_pred <- d %>% filter(orLarge == 'Right') %>% 
    mutate(orLarge = 'Left prediction', 
           thre = -thre, threinf = -threinf, thresup = -thresup, pred = TRUE)
  
  rbind(d, thre_long_pred_bottom_pred, thre_long_pred_left_pred) %>% 
    arrange(subject) %>% mutate(subject = as.factor(subject)) 
}
