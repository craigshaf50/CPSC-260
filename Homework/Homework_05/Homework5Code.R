install.packages("fueleconomy")
install.packages("maps")
library(tidyverse)
library(nycflights13)
library(Lahman)
library(babynames)
library(fueleconomy)
library(maps)

view(Batting)
Batting %>%
  count(playerID,yearID,stint,teamID,lgID) %>%
  filter(n>1)

view(babynames)
babynames %>%
  count(name,year,sex) %>%
  filter(n>1)

view(vehicles)
vehicles %>%
  count(id) %>%
  filter(n>1)

view(diamonds)
diamonds2<-mutate(diamonds,id=row_number())
diamonds2

view(People)
view(Salaries)

view(flights)
view(airports)
view(planes)

flights.1<-flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  mutate(total_delay=(dep_delay+arr_delay)) %>%
  group_by(dest) %>%
  summarize(avg_delay=mean(total_delay))

avg_dest_delays<-airports %>%
  inner_join(flights.1,by=c("faa"="dest"))

avg_dest_delays %>%   
  ggplot(aes(lon, lat, colour = avg_delay)) +   
  borders("state") +   
  geom_point() +  
  coord_quickmap()

flights.2<-flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  mutate(total_delay=(dep_delay+arr_delay)) %>%
  group_by(tailnum) %>%
  summarize(avg_delay=mean(total_delay))

age_vs_delays<-planes %>%
  inner_join(flights.2,by="tailnum") %>%
  filter(!is.na(year)) %>%
  mutate(age=(2013-year))

ggplot(age_vs_delays,mapping=aes(x=age,y=avg_delay))+
  geom_point()+
  geom_smooth(se=F)

view(weather)  
flights.3<-flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  mutate(total_delay=(dep_delay+arr_delay)) %>%
  group_by(day, month, year, hour, origin) %>%
  summarize(avg_delay=mean(total_delay))

delays_vs_weather<-weather %>%
  inner_join(flights.3)

ggplot(delays_vs_weather,mapping=aes(x=precip,y=avg_delay))+
  geom_point()+
  geom_smooth(se=F)

flights_100<-flights %>%
  filter(!is.na(tailnum)) %>%
  count(tailnum, sort=T) %>%
  filter(n>=100)

ec.1<-flights %>% 
  semi_join(flights_100)

ec.2<-flights%>%
  group_by(tailnum) %>%
  mutate(f.100=ifelse(tailnum %in% flights_100$tailnum,1,0)) %>%
  filter(f.100==1)
