library(tidyverse)
library(Lahman)

#Question 1 - Can a player with a low batting average still hit a lot of HR?
#answer - Yes. By looking at the data, both Adam Dunn and Dave Kingman hit over
#         400 homeruns with a batting average less than .24
view(Batting)
view(Master)

bat.1<-Batting %>%
  group_by(playerID) %>%
  summarise(total_atbats=sum(AB),total_hits=sum(H),total_hr=sum(HR)) %>%
  mutate(batting_avg=total_hits/total_atbats)

career_stats<-Master %>%
  select(playerID,nameFirst,nameLast) %>%
  left_join(bat.1,by="playerID") %>%
  filter(!is.na(batting_avg),batting_avg<=.24) %>%
  select(-total_hits,-total_atbats,-playerID) %>%
  arrange(-total_hr)
#proof of answer
career_stats
ggplot(data=career_stats,mapping=aes(x=batting_avg,y=total_hr))+
  geom_point()

#Question 2 - Which airline should you choose for NEWARK to OHARE?
#answer - You would want to be on UA, United Airlines, because they make up roughly
#         6 minutes of air time per flight from Newark(EWR) to O'Hare(ORD) 
library(nycflights13)
view(flights)
view(airlines)
EWR.to.ORD<-flights %>%
  filter(origin=="EWR",dest=="ORD", dep_delay>0,!is.na(dep_delay),!is.na(arr_delay)) %>%
  mutate(time_gained=dep_delay-arr_delay) %>%
  group_by(carrier) %>%
  summarise(avg_time_gained=mean(time_gained)) %>%
  arrange(-avg_time_gained) %>%
  left_join(airlines,by="carrier")
#proof of answer
EWR.to.ORD

#EXTRA CREDIT:
#a) Newark offers the most flights
nyc.flights<-flights %>%
  filter(!is.na(dep_time))%>%
  group_by(origin) %>%
  summarise(total_flights=n())
#proof that Newark offers more flights
nyc.flights

#b) Flights out of Newark are more likely to leave late
nyc.flights.2<-flights %>%
  filter(!is.na(dep_time),dep_delay>0)%>%
  group_by(origin) %>%
  summarise(flights_delayed=n())
flight.delay_pct<-left_join(nyc.flights,nyc.flights.2,by="origin") %>%
  mutate(delay_pct=(flights_delayed/total_flights)) %>%
  select(origin,delay_pct)
#proof that Newark is more likely to have flights leave late
flight.delay_pct





