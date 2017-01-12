diferences <- function(d) {
  if(d$vertical %>% unique() ) {
    dif <- abs(d$thre[d$orLarge == 'Top'] - d$thre[d$orLarge == 'Bottom'])
    difpred <- abs(d$thre[d$orLarge == 'Bottom'] - 
                     d$thre[d$orLarge == 'Bottom prediction'])
    tibble(dif, difpred)
  }
  else {
    dif <- abs(d$thre[d$orLarge == 'Right'] - d$thre[d$orLarge == 'Left'])
    difpred <- abs(d$thre[d$orLarge == 'Left'] - 
                     d$thre[d$orLarge == 'Left prediction'])
    print(d$subject)
    print('right')
    print(d$thre[d$orLarge == 'Right'])
    print('left prediction')
    print(d$thre[d$orLarge == 'Left prediction'])
    tibble(dif, difpred)      
  }
}