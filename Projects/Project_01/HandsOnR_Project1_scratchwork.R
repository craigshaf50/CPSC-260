# CPSC 260 - 1/13/22
# Hands on R Book
# Project One: Weighted Dice

# create an object containing numbers 1 to 6
# c() is the combine command
die <- c(1:6)

# R built in functions
# # r function mean()
# mean(die)
# # r funciton sum()
# sum(die)
# # sample(x,size)
# sample(die,1)

# roll one dice
sample(x=die,size=1)

# roll two dice - PROBLEM: samples without replacing (cannot have same #)
sample(x=die,size=2)

# roll two dice - SOLVED: add argument replace=TRUE
sample(x=die,size=2, replace=TRUE)

# store two rolls into roll
roll <- sample(x=die,size=2, replace=TRUE)

# sum of two dice rolled
sum(roll)

# does sum exceed 8
sum(roll) >8

# function to roll multiple 6-sided die
# num = number of die
dice.roll <- function(num=2){
  die<-(1:6)
  roll<- sample(x=die,size=num, replace=TRUE)
  s<- sum(roll)
  return(s)
}
# function to roll multiple die
# dice = die used, num = number of die
dice.roll.2 <- function(dice,num){
  roll<- sample(x=dice,size=num, replace=TRUE)
  s<- sum(roll)
  return(s)
}

# call function
dice.roll(num=5)

# new 20 side die
die20 <- c(1:20)

# call it with new die
dice.roll.2(dice=die20,num=5)

library(ggplot2)

# create two vectors of the same length
x<-c(0:10)
y<-c(3,5,4,7,8,9,12,1,4,2,5)

qplot(x,y)

# create a vector for histogram
p<-c(1,2,5,6,3,1,2,3,6,7,5,3,2,1)

qplot(p, binwidth=2)


#run dice roll 10 thousand times and store results in rolls
rolls <- replicate(10000, dice.roll())

#histogram of rolls
qplot(rolls, binwidth=1)
