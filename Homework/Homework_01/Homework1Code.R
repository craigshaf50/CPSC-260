# Homework 1 code
# copy code entered and console output into word doc
# Also change color of answers to easily tell difference
# check 30 min in zoom is confused 1-25-22
my.list

vec.short<-c(1:5)
vec.long<-c(1:8)

vec.df<-data.frame(vec.short,vec.long)

vec.short<-c(vec.short,NA,NA,NA)
typeof(vec.short)

vec.df<-data.frame(vec.short,vec.long)
vec.df

str(my.list)

test.list = list(data7, habitats, fw, bird)
test.list
str(test.list)

names(test.list) = c("data7", "habitats" ,"fw", "bird")
test.list

test.list[3]

test.list[[3]]

test.list$fw

class(test.list[3])

class(test.list[[3]])

class(test.list$fw)

test.list$fw[,1] 
