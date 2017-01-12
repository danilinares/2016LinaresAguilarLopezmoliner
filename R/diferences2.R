diferences2 <- function(d) {
  dif <- abs(d$thre[d$orLarge == 'Top'] - d$thre[d$orLarge == 'Bottom'])
  difpred <- abs(d$thre[d$orLarge == 'Top'] - 
                   d$thre[d$orLarge == 'Bottom prediction'])
  tibble(dif, difpred)


}