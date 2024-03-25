for (i in 2:6){
  print(i)
}


set.seed(1234)

# rolling a dice
dice <- 1:6
sample(dice,1)


# rolling many dice
results <- c()
nsim <- 1000
for (i in 1:nsim){
  results <- c(results,sample(dice,1))
}
length(results)

barplot(table(results)/sum(table(results)))


# function that rolls n dice
roll_n_dice <- function(n){ return(sample(1:6,n,replace=TRUE)) }

# function that gives empirical distribution of observed rv
show_emp_dist <- function(vec, main='title'){
  print(table(vec)/sum(table(vec)))
  barplot(table(vec)/sum(table(vec)), main=main)
}
show_emp_dist(roll_n_dice(30))

#function that creates empirical dist from data generating func
create_emp_dist <- function(FUN,...,nsim=1000){
  results <- c()
  for (i in 1:nsim){results <- c(results,FUN(...))}
  return(results)
}
table(create_emp_dist(roll_n_dice, 4))

# function that gives sum of successes
# (greater than or equal to threshold)
# 1 always fails, 6 always succeeds, regardless of threshold
count_success <- function(roll, threshold){ 
  return (sum( (roll>=max(threshold,2) | roll==6) ) )
  }
count_success(roll_n_dice((10)), 4)

library(stringr)

# function to get nwounds caused by 1 model attacking
keywords <- list(ap=2, 
                 blast=c(3,20),
                 furious=1,
                 lance=1,
                 regeneration=1,
                 rending=1,
                 poison=1)
count_melee_wounds <- function(attacks,quality,defense,keywords=list()){
  quality_roll <- roll_n_dice(max(attacks,0))
  nhits <- count_success(quality_roll,quality) +
            ifelse(is.null(keywords$furious),0,sum(quality_roll==6))
  if('blast' %in% names(keywords)){
    nhits <- min(keywords$blast[2], nhits * keywords$blast[1])
  }
  #handle rending hits separately
  nhits <- nhits - ifelse(is.null(keywords$rending),0,sum(quality_roll==6))
  defense_roll <- roll_n_dice(max(nhits,0))
  defense <- defense + 
              ifelse(is.null(keywords$ap),0,keywords$ap) +
              ifelse(is.null(keywords$lance),0,2)
  
  nwounds <-  nhits - count_success(defense_roll,defense)
  
  if('poison' %in% names(keywords)){
    nwounds <- nwounds + sum(defense_roll==6) -
      count_success(sum(defense_roll),defense)  
  }
  
  #rending wounds
  if('rending' %in% names(keywords)){
    nrends <- sum(quality_roll==6)
    rend_def_roll <- roll_n_dice(nrends)
    rendwounds <- nrends - count_success(rend_def_roll,defense+4)
    if('poison' %in% names(keywords)){
      rend_pois_def_roll <- roll_n_dice(rend_def_roll==6)
      rendwounds <- rendwounds + sum(rend_def_roll==6) -
        count_success(rend_pois_def_roll,defense+4)  
    }
  }
  
  nwounds <- nwounds + ifelse(is.null(keywords$rending),0,rendwounds) - 
                      ifelse(is.null(keywords$regeneration) | 
                                !is.null(keywords$rending) |
                                !is.null(keywords$poison),
                              0,
                              count_success(nwounds,5))
  return (max(nwounds,0))
}

count_melee_wounds(3,4,3)



