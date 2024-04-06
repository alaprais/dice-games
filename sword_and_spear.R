######################
## Sword and Spear has a simple combat mechanic of rolling 
## X dice, choosing the top 4, and then comparing those to opponent.
##
## Modifiers seem to be either 1. adding dice to X 
##                             2. adding (or subtracting) from final rolls
######################

roll_n_dice <- function(n){ return(sample(1:6,n,replace=TRUE)) }

## what is the distribution like when you roll more than 4 
## but still choose top 4
nsim <- 1000
res_1 <- c()
res_2 <- c()
res_3 <- c()
res_4 <- c()
for (i in 1:nsim){
  my_roll <- sort(roll_n_dice(5), decreasing = TRUE)[1:4]
  opp_roll <- sort(roll_n_dice(4), decreasing = TRUE)[1:4]
  res_1 <- c(res_1, my_roll[1]-opp_roll[1])
  res_2 <- c(res_2,my_roll[2]-opp_roll[2])
  res_3 <- c(res_3,my_roll[3]-opp_roll[3])
  res_4 <- c(res_4,my_roll[4]-opp_roll[4])
}

barplot(table(res_1), main='res1')
barplot(table(res_2), main='res2')
barplot(table(res_3), main='res3')
barplot(table(res_4), main='res4')

library(ggplot2)

## win/loss estimates

win_loss_distribution <- function(nmydice=4,noppdice=4,nsim=10000){
wins <- c()
losses <- c()
for (i in 1:nsim){
  my_roll <- sort(roll_n_dice(nmydice), decreasing = TRUE)[1:4]
  opp_roll <- sort(roll_n_dice(noppdice), decreasing = TRUE)[1:4]
  res <- my_roll - opp_roll
  wins <- c(wins,sum(res>0))
  losses <- c(losses, sum(res<0))
}

combat_data <- data.frame(wins,losses)
ggplot(combat_data, aes(x=wins, y=losses)) +
  stat_bin2d(bins = 9,aes(fill=after_stat(density))) +
  scale_fill_continuous() +
  theme_bw()
}

win_loss_distribution(7,4,nsim=10000)

win_differential <- function(nmydice=4,noppdice=4,nsim=10000, modifier=0){
  wins <- c()
  losses <- c()
  for (i in 1:nsim){
    my_roll <- sort(roll_n_dice(nmydice), decreasing = TRUE)[1:4] 
    opp_roll <- sort(roll_n_dice(noppdice), decreasing = TRUE)[1:4] - modifier
    res <- my_roll - opp_roll
    wins <- c(wins,sum(res>0))
    losses <- c(losses, sum(res<0))
  }
  diffs <- wins - losses
  barplot(table(diffs)/length(diffs),main=paste("My",nmydice,
                                                "V Opp",noppdice))
  print(table(diffs)/length(diffs))
}

win_differential(4,4)



get_wins_and_losses <- function(nmydice=4,noppdice=4,nsim=10000, modifier=0, win_match=FALSE){
    wins <- c()
    losses <- c()
    for (i in 1:nsim){
      my_roll <- sort(roll_n_dice(nmydice), decreasing = TRUE)[1:4] 
      opp_roll <- sort(roll_n_dice(noppdice), decreasing = TRUE)[1:4] - modifier
      res <- my_roll - opp_roll
      wins <- c(wins,sum(res>0)+ ifelse(win_match,sum(res==0),0))
      losses <- c(losses, sum(res<0))
    }
    temp <- rbind(table(wins),table(losses))/length(wins)
    barplot(temp,beside=T, col=c("darkblue","red"),
            main=paste("My",nmydice,"V Opp",noppdice, "matching",win_match))
}

get_wins_and_losses(4,4,win_match=FALSE)


########## What if we mixed in different die types
roll_n_dx <- function(n,x){
  if (n>0){out <- sample(1:x,n,replace=TRUE)}
  else {out <- 0}
  return(out) 
  }

#d4,d6,d8,d10,d12,d20
get_wins_and_losses_dx <- function(mydice=c(0,4,0,0,0,0), oppdice=c(0,4,0,0,0,0),
                                nsim=10000, modifier=0){
  dice_order <- c(4,6,8,10,12,20)
  wins <- c()
  losses <- c()
  for (i in 1:nsim){
    my_roll <- c()
    for (i in 1:length(mydice)){my_roll <- c(my_roll, roll_n_dx(mydice[i],dice_order[i]))}
    my_roll <- sort(my_roll, decreasing = TRUE)[1:4] 
    
    opp_roll <- c()
    for (i in 1:length(oppdice)){opp_roll <- c(opp_roll, roll_n_dx(oppdice[i],dice_order[i]))}
    opp_roll <- sort(opp_roll, decreasing = TRUE)[1:4] 
    
    res <- my_roll - opp_roll # single fight result
    wins <- c(wins,sum(res>0))
    losses <- c(losses, sum(res<0))
  }

  temp <- rbind(table(wins),table(losses))/nsim
  barplot(temp,beside=T, col=c("darkblue","red"),
          main=paste('temp'))
}
#d4,d6,d8,d10,d12,d20
get_wins_and_losses_dx(mydice =c(0,0,0,5,0,0),
                       oppdice=c(0,0,0,4,0,0))


################
win_differential_dx <- function(mydice=c(0,3,0,0,0,0),
                             oppdice=c(0,3,0,0,0,0),
                             nsim=10000, modifier=0){
  dice_order <- c(4,6,8,10,12,20)
  wins <- c()
  losses <- c()
  for (i in 1:nsim){
    my_roll <- c()
    for (i in 1:length(mydice)){my_roll <- c(my_roll, roll_n_dx(mydice[i],dice_order[i]))}
    my_roll <- sort(my_roll, decreasing = TRUE)[1:3] 
    
    opp_roll <- c()
    for (i in 1:length(oppdice)){opp_roll <- c(opp_roll, roll_n_dx(oppdice[i],dice_order[i]))}
    opp_roll <- sort(opp_roll, decreasing = TRUE)[1:3] 
    
    res <- my_roll - opp_roll #result of single fight
    wins <- c(wins,sum(res>0))
    losses <- c(losses, sum(res<0))
  }
  diffs <- wins - losses
  barplot(table(diffs)/nsim,main=paste("My","v Opp"))
  print(table(diffs)/length(diffs))
  boxplot(diffs, horizontal=TRUE)
  print(paste("Comeback",ecdf(diffs)(-1))) # prob of disadvantaged side "winning"
  print(paste("Clinical",1-ecdf(diffs)(0)) ) #prob of advantaged side "winning"
}
#d4,d6,d8,d10,d12,d20
win_differential_dx(mydice=c(0,0,0,0,3,0),oppdice=c(0,0,0,0,3,0))
win_differential_dx(mydice=c(0,0,0,0,4,0),oppdice=c(0,0,0,0,3,0))
win_differential_dx(mydice=c(0,2,0,0,3,0),oppdice=c(0,0,0,0,3,0))

win_differential_dx(mydice=c(0,3,0,0,0,0),oppdice=c(0,3,0,0,0,0))
win_differential_dx(mydice=c(0,4,0,0,0,0),oppdice=c(0,3,0,0,0,0))
win_differential_dx(mydice=c(0,5,0,0,0,0),oppdice=c(0,3,0,0,0,0))
win_differential_dx(mydice=c(0,7,0,0,0,0),oppdice=c(0,3,0,0,0,0))

win_differential_dx(mydice=c(0,2,0,0,0,0),oppdice=c(0,2,0,0,0,0))
win_differential_dx(mydice=c(0,5,0,0,0,0),oppdice=c(0,2,0,0,0,0))

win_differential_dx(mydice=c(0,3,0,0,0,0),oppdice=c(0,3,0,0,0,0))
win_differential_dx(mydice=c(0,4,0,0,0,0),oppdice=c(0,4,0,0,0,0))

win_differential_dx(mydice=c(0,4,0,0,0,0),oppdice=c(0,3,0,0,0,0))
win_differential_dx(mydice=c(0,5,0,0,0,0),oppdice=c(0,4,0,0,0,0))
win_differential_dx(mydice=c(0,10,0,0,0,0),oppdice=c(0,9,0,0,0,0))






