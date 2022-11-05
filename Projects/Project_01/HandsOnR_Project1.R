library(ggplot2)

# function to roll multiple 6-sided die
# num = number of die
dice.roll <- function(num=2){
  die<-(1:6)
  roll<- sample(x=die,size=num, replace=TRUE)
  s<- sum(roll)
  return(s)
}

# run dice.roll function
dice.roll()

#run dice roll 10 thousand times and store results in rolls
rolls <- replicate(10000, dice.roll())

#histogram of rolls
qplot(rolls, binwidth=1)

#create vector for weights
weights<-c(1/8,1/8,1/8,1/8,1/8,3/8)

# weighted dice roll
# w = weight
weighted.dice.roll <- function(num=2,w){
  die<-(1:6)
  roll<- sample(x=die,size=num, replace=TRUE, prob=w)
  s<- sum(roll)
  return(s)
}
# run weighted.dice.roll
weighted.dice.roll(w=weights)

# 10,000 weighted rolls
w.rolls <- replicate(10000, weighted.dice.roll(w=weights))

# new histogram to see if weighted
qplot(w.rolls, binwidth=1)
