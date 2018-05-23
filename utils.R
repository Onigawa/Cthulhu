rollDice<-function(dice=6,nbr=1,sum=F){
  res<-sample(1:dice, nbr, replace=T)
  if(sum) res <-sum(res)
  return(res)
}


