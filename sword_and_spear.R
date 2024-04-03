######################
## Sword and Spear has a simple combat mechanic of rolling 
## X dice, choosing the top 4, and then comparing those to opponent.
##
## Modifiers seem to be either 1. adding dice to X 
##                             2. adding (or subtracting) from final rolls
######################

roll_n_dice <- function(n){ return(sample(1:20,n,replace=TRUE)) }

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

get_wins_and_losses(5,4,win_match = FALSE)
