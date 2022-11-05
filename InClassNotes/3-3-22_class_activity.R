library(tidyverse)
library(babynames)
babynames
lifetables
help("lifetables")

#filter out correct time period 1900-2014
#need column that has person's age called x
#divide counts by 1000
name.df<-babynames %>%
  filter(year>=1900) %>%
  filter(year<=2014) %>%
  mutate(x=2014-year,n_1000=n/1000) %>%
  mutate(decade=ifelse(year%in%c(1900:1909),1900,
                       ifelse(year%in%c(1910:1919),1910,
                       ifelse(year%in%c(1920:1929),1920,
                       ifelse(year%in%c(1930:1939),1930,
                       ifelse(year%in%c(1940:1949),1940,
                       ifelse(year%in%c(1950:1959),1950,
                       ifelse(year%in%c(1950:1959),1950,
                       ifelse(year%in%c(1960:1969),1960,
                       ifelse(year%in%c(1970:1979),1970,
                       ifelse(year%in%c(1980:1989),1980,
                       ifelse(year%in%c(1990:1999),1990,
                       ifelse(year%in%c(2000:2009),2000,
                       2010)))))))))))))

#join practice - join by one column - year
#left_join() or right_join()
#join1<-left_join(name.df,lifetables,by="year")

#fix lifetables
lifetables.df<-lifetables %>%
  mutate(decade=year) %>%
  select(decade,x,sex,lx)

#estimated alive = count * prob person is alive
#to get prob someone is alive:
#join lifetable and babyname. need year and baby names to match up
# then we can compute the prob the person is alive
#    =lx/100000
joined.df<-left_join(name.df,lifetables.df,by= c("decade","x","sex"))
joined.df<-joined.df %>%
  mutate(prob_alive=lx/100000, est_alive=n*prob_alive,age_today=x) %>%
  select(-x)






