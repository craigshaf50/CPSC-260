#install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

head(flights)
help(flights)
view(flights)
view(airlines)

flights.1<-flights %>%
  filter(arr_delay>=120)

flights.2<-flights %>%
  filter((carrier=='UA')|(carrier=='AA')|(carrier=='DL'))

flights.3<-flights %>%
  filter(dep_delay<=0,arr_delay>=120)

help(between)

flights.4<-flights %>%
  filter(between(dep_time,0000,0600))
  
flights.5<-flights %>%
  filter(is.na(dep_time))
nrow(flights.5)

flights.6<-flights %>%
  arrange(-dep_delay)

flights.7<-flights %>%
  arrange(dep_delay)
flights.8<-flights %>%
  arrange(-distance)
flights.9<-flights %>%
  arrange(distance)
flights.10<-flights %>%
  select(dep_time,dep_time,dep_time)

help(contains)
select(flights, contains("TIME"))

flights.11<-flights %>%
  mutate(sched_dep_time_min=((sched_dep_time%/%100)*60+(sched_dep_time%%100)))

flights.12<-flights %>%
  mutate(arr_dep_diff=arr_time-dep_time) %>%
  select(arr_dep_diff,air_time)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(dest) %>%
  summarize(n=n())

not_cancelled %>%
  group_by(tailnum) %>%
  summarize(n=sum(distance))

flights_canceled<-flights %>%
  filter(is.na(arr_time),is.na(dep_time)) %>%
  group_by(year, month, day) %>% 
  summarize(canceled_per_day=mean(n()))
avg_delay<-flights %>%
  filter(!is.na(arr_time),!is.na(dep_time)) %>%
  group_by(year, month, day) %>% 
  summarize(avg_dep_delay=mean(dep_delay))
            
cancel_vs_delay<-left_join(flights_canceled,avg_delay, by=c("year","month","day"))

ggplot(cancel_vs_delay)+
  geom_point(mapping=aes(canceled_per_day,avg_dep_delay))

