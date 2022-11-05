#install.packages("babynames")
library(tidyverse)
library(babynames)

head(babynames)
class(babynames)

#1 most popular baby names?
names.MostPop<-babynames %>%
  group_by(name) %>%
  summarize(total_n=sum(n)) %>%
  arrange(-total_n)
names.MostPop
#since 1990?
names.MostPop_1990<-babynames %>%
  filter(year>=1990) %>%
  group_by(name) %>%
  summarize(total_n=sum(n)) %>%
  arrange(-total_n)
names.MostPop_1990

#differentiate most popular since 1990 by gender
#female since 1990
names.MostPop_1990<-babynames %>%
  filter(year>=1990,sex=='F') %>%
  group_by(name,sex) %>%
  summarize(total_n=sum(n)) %>%
  arrange(-total_n)
names.MostPop_1990
#male since 1990
names.MostPop_1990<-babynames %>%
  filter(year>=1990,sex=='M') %>%
  group_by(name,sex) %>%
  summarize(total_n=sum(n)) %>%
  arrange(-total_n)
names.MostPop_1990


#3 given my name, Craig
names.craig<-babynames %>%
  filter(name=="Craig") %>%
  summarize(total_n=sum(n))
names.craig
#name and gender
names.craig<-babynames %>%
  filter(name=="Craig",sex=='M') %>%
  summarize(total_n=sum(n))
names.craig
#number of craig's since 2001
names.craig<-babynames %>%
  filter(name=="Craig",sex=='M',year>=2001) %>%
  summarize(total_n=sum(n))
names.craig

#4 graph of name over time
names.craig<-babynames %>%
  filter(name=="Craig")

ggplot(data=names.craig)+
  geom_point(mapping=aes(x=year,y=n,color=sex))+
  labs(x="Year",y="Number of Babies Named Craig", 
       title = "Number of Babies Named Craig since 1880",
       subtitle = "(Only includes years where more than 5 babies are named Craig)")
#ggsave('babies-named-craig.png', width = 14, height = 10, dpi = "retina")

#5 number of kids named Abcde
names.Abcde<-babynames %>%
  filter(name=="Abcde") %>%
  summarize(total_n=sum(n))
names.Abcde              
