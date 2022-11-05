cars.altered<-read.table(file="cars altered.txt", sep=',', header=TRUE)
head(cars.altered,n=10)
str(cars.altered)

#used 0 and "missing for replacements
cars1<-cars.altered
cars1[2,3]=0
cars1[4,8]="missing"

#used mean and mode for replacements
cars2<-cars.altered
mean(cars2$cubicinches,na.rm=TRUE) #tells us mean for replacement - ~202
median(cars2$cubicinches,na.rm=TRUE) #or median for replacement  - ~160
cars2[2,3]=202
table(cars2$brand) #tells us mode to use for replacement
cars2[4,8]=" US"
#change USA to US and France to Europe
cars2[6,8]=" US"
which(cars2$brand==" France") #on row 52
cars2[52,8]=" Europe"

#table(cars2$brand)

#used random generated from data set for replacement
cars3<-cars.altered
sample(na.omit(cars3$cubicinches),1)
cars3[2,3]=198
sample(na.omit(cars3$brand),1)
cars[4,8]=" Japan"

#using imputation
cars4<-cars.altered

#############
hist(cars.altered$cubicinches) #shows skew of cubic inches
hist(cars.altered$weightlbs) #shows outlier
which(cars.altered$weightlbs < 1000)
cars.altered[262,] #car only weighs 192 pounds TYPO
cars.altered[262,5]=1925

plot(cars.altered$weightlbs,cars.altered$mpg)
which(cars.altered$mpg>400)
cars.altered[263,1]=52.7
#############

#############
#  3-24-22  #
#############
cars.altered<-read.table(file="cars altered.txt", sep=',', header=TRUE)
head(cars.altered,n=10)

hist(cars.altered$cubicinches)

#check if data is normally distributed
#Is histogram symmetric and bell shaped?
#can also check qqplot i.e. normal probability plot

qqnorm(cars.altered$cubicinches)
qqline(cars.altered$cubicinches)

#data transformation
#log, sq root, 1/log, 1/(sq root)
#log
hist(log(cars.altered$cubicinches))
qqnorm(log(cars.altered$cubicinches))
qqline(log(cars.altered$cubicinches))
#sqrt
hist(sqrt(cars.altered$cubicinches))
qqnorm(sqrt(cars.altered$cubicinches))
qqline(sqrt(cars.altered$cubicinches))
#1/log - looks the most symmetric and bell shaped of the 4
hist(1/log(cars.altered$cubicinches))
qqnorm(1/log(cars.altered$cubicinches))
qqline(1/log(cars.altered$cubicinches))
#1/sqrt
hist(1/sqrt(cars.altered$cubicinches))
qqnorm(1/sqrt(cars.altered$cubicinches))
qqline(1/sqrt(cars.altered$cubicinches))

#Min-Max Normalization Transformation
#use when variables have ranges that vary greatly
summary(cars.altered$weightlbs[-262]) #removing the super outlier
min(cars.altered$weightlbs[-262])
max(cars.altered$weightlbs[-262])
#min 1613 max 4997 range = 3384
#(weight-min)/(max-min)
min.max.weights<-(cars.altered$weightlbs-1613)/3384
min.max.weights
#all values now between 0 and 1

#z score standardization
# (weight-mean)/StanDev
sd(cars.altered$weightlbs[-262]) #851.5834
#mean is 3004
z.weights<-(cars.altered$weightlbs-3004)/851.5834
z.weights
hist(z.weights)
#all values will be roughly between -2 and 2

#example of not tidy format data
HIV<-read.csv(file="hiv_percent.csv")

library(tidyverse)
hiv.subset<-filter(HIV, country %in% c('Cambodia','El Salvador', "Indonesia", 'United States', "Zambia")) %>%
  select(country, X1990, X1993, X1996, X1999, X2002, X2005, X2008,X2011)

#gather()
hiv_long<-gather(hiv.subset, key=Year, value= Percent, -country)
hiv_long

#spread() - basically undoes the gather
hiv_spread<-spread(hiv_long, key=Year, value=Percent)
hiv_spread

library(babynames)
babynames
baby.sub<-babynames[sample(1:nrow(babynames),1000),]
#use spread() to spread out the number of babies born with a specific 
#name across multiple columns
baby.spread<-spread(baby.sub, key=year, value=n)
