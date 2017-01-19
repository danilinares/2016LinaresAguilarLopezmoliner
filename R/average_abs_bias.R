average_abs_bias <- function(d) {
  d %>% 
    group_by(orLarge, vertical) %>% 
    summarise(model = list(t.test(thre))) %>% 
    mutate(tidy = map(model, tidy)) %>% 
    unnest(tidy, .drop = TRUE) %>% 
    rename(thre = estimate, threinf = conf.low, thresup = conf.high) %>% 
    ungroup() %>% 
    mutate(subject = 'All participants')
}