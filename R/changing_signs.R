changing_signs <- function(d) {
  if('Top' %in% d$orLarge) {
    if (d$thre[d$orLarge == 'Top'] < 0) k <- -1
    else k <- 1
  }
  if('Right' %in% d$orLarge) {
    if (d$thre[d$orLarge == 'Right'] < 0) k <- -1
    else k <- 1
  }
  d$thre <- k * d$thre
  d$threinf <- k * d$threinf
  d$thresup <- k * d$thresup
  
  return(d)
}

