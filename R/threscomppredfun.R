threscomppredfun <- function(d) {
  d %>% 
    group_by(subject, vertical, sample) %>% 
    do(tibble(dif = diff(.$thre))) %>% group_by(subject, vertical) %>% 
    do({
      ci <- quantile(.$dif, c(.025, .975))
      dif <- mean(.$dif)
      difinf <- ci[[1]]
      difsup <- ci[[2]]
      signif <- if_else(difinf * difsup > 0, '*', ' ')
      tibble(dif, difinf, difsup, signif)
    })
}