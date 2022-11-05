#install.packages("pscl")
library(tidyverse)
library(pscl)

head(presidentialElections)
help(presidentialElections)

### select() ###
#create a data set containing only demVote and year
#select() - give data set and list columns you have selected
select.practice1<-select(presidentialElections,demVote,year)
select.practice1
#create a data set containing only demVote and state
select.practice2<-select(presidentialElections,state, demVote)
select.practice2

### filter() ###
#filter out data for 2008, extracting only 2008's data
#filter() - give it data set
filter.practice1<-filter(presidentialElections,year==2008)
filter.practice1
#filter out states that were part of the confederacy
filter.practice2<-filter(presidentialElections,south==TRUE)
filter.practice2
#filter out data for iowa from 2000 to 2016
filter.practice3<-filter(presidentialElections,state=='Iowa',year>=2000)
filter.practice3

### filter and select ###
#filter out data for iowa from 2000 to 2016, 
#but dont include state and south columns
#used separately
filter.practice3<-filter(presidentialElections,state=='Iowa',year>=2000)
select.practice3<-select(filter.practice3,demVote,year)
select.practice3
#or nested
nested<-select(filter(presidentialElections,state=='Iowa',year>=2000),demVote,year)
nested
#or pipe notation
pipe.practice<-presidentialElections %>%
  filter(state=="Iowa",year>=2000) %>%
  select(demVote,year)
pipe.practice

### mutate() ###
#give dataframe and specify the name of column and what you want it to have
#add column to data set for % of other candidates 
#otherVote = (100-demVote)
mutate.practice1<-mutate(presidentialElections,otherVote=100-demVote)
mutate.practice1
#in pipe notation
mutate.practice1<-presidentialElections %>%
  mutate(otherVote=100-demVote)
mutate.practice1  

### arrange() ###
#give data set and tell it what column to use when arranging
#make it so 2016 is the top - descending order
arrange.practice1<-arrange(presidentialElections,-year)
arrange.practice1
#or
arrange.practice1<-arrange(presidentialElections,desc(year))
arrange.practice1

### summarize() ###
#compute average of demVote column
summarize.practice1<-summarize(presidentialElections,avgDemVote=mean(demVote))
summarize.practice1


### summarize and mutate ###
#add column for other vote and compute avg of demvote and othervote column
nested.2<-summarize(mutate(presidentialElections,otherVote=100-demVote),AvgDemVote=mean(demVote),AvgOtherVote=mean(otherVote))
nested.2
#pipe notation
nested.2<-presidentialElections %>% 
  mutate(otherVote=100-demVote) %>%
  summarize(AvgDemVote=mean(demVote),AvgOtherVote=mean(otherVote))
nested.2


### group_by() ###
group_by(presidentialElections,state) %>%
  summarize(AvgDemVote=mean(demVote)) %>%
  arrange(-AvgDemVote)


### practice using the functions ###
view(presidential)
#1. retrieve only name and party
pres.NameParty<-select(presidential,name,party)
pres.NameParty
#2. create subset of data for only dem presidents
pres.dem<-filter(presidential,party=="Democratic")
pres.dem
#3. create subset of dta for only dem presidents after 1973
pres.dem1973<-filter(presidential,party=="Democratic",start>= 1973)
pres.dem1973
#4. only the names of dems after 1973
pres.demNames1973<-presidential %>%
  filter(party=="Democratic",start>= 1973) %>%
  select(name)
pres.demNames1973


#add length of president's term
#libridate function in tidyverse makes date arithmatic easier
presidential2<-presidential %>% mutate(term.length = interval(presidential$start,presidential$end)/dyear(1))
interval(presidential$start,presidential$end)/dyear(1)
help(interval)

#how many of each party
#group_by( ) by party
#n() counts rows
presidential %>% group_by(party) %>% summarize(Num=n())


#install.packages("Lahman")
library(Lahman)
help(Lahman)

help(Teams)
view(Teams)
head(Teams)
class(Teams)
str(Teams)
names(Teams)

#5a
ben.subset<-filter(Teams, teamID=="NYN",yearID>=2004,yearID<=2012)
ben.subset<-select(ben.subset,yearID,teamID,W,L)
#5b
ben.subset1<-Teams %>% 
  filter(teamID=="NYN",yearID>=2004,yearID<=2012) %>%
  select(yearID,teamID,W,L)

#7
ben.subset2<-Teams %>% 
  filter(teamID=="NYN",yearID>=2004,yearID<=2012) %>%
  select(yearID,teamID,W,L,R,RA)

#8
ben.subset3<-Teams %>% 
  filter(teamID=="NYN",yearID>=2004,yearID<=2012) %>%
  select(yearID,teamID,W,L,R,RA) %>%
  mutate(WPct = W/(W+L))

#9
ben.subset4<-Teams %>% 
  filter(teamID=="NYN",yearID>=2004,yearID<=2012) %>%
  select(yearID,teamID,W,L,R,RA) %>%
  mutate(WPct = W/(W+L),E_WPct = 1/(1+(RA/R)^2))

#10
filter(ben.subset4, WPct>=E_WPct)

#11a
arrange(ben.subset4, WPct)
#11b
arrange(ben.subset4, WPct-E_WPct)

#12 add diff
ben.subset5<-Teams %>% 
  filter(teamID=="NYN",yearID>=2004,yearID<=2012) %>%
  select(yearID,teamID,W,L,R,RA) %>%
  mutate(WPct = W/(W+L),E_WPct = 1/(1+(RA/R)^2),diff=WPct-E_WPct)

#13 Game_diff
ben.subset6<-Teams %>% 
  filter(teamID=="NYN",yearID>=2004,yearID<=2012) %>%
  select(yearID,teamID,W,L,R,RA) %>%
  mutate(WPct = W/(W+L),E_WPct = 1/(1+(RA/R)^2),diff=WPct-E_WPct,Game_diff=(W+L)*diff)

#14
ben.subset7<-Teams %>% 
  filter(teamID=="NYN",yearID>=2004,yearID<=2012) %>%
  select(yearID,teamID,W,L,R,RA) %>%
  mutate(WPct = W/(W+L),E_WPct = 1/(1+(RA/R)^2),diff=WPct-E_WPct,Game_diff=(W+L)*diff) %>%
  rename(pct_diff=diff)

#15 avg win pct with summarize
summarize(ben.subset7, avg_WPct=mean(WPct))
#16 add num seasons, total win & losses
summarize(ben.subset7, seasons=n(),totalWins=sum(W),totalLosses=sum(L),avg_WPct=mean(WPct))

#17
ben.subset8<-Teams %>% 
  filter(teamID=="NYN",yearID>=2004,yearID<=2012) %>%
  select(yearID,teamID,W,L,R,RA) %>%
  mutate(WPct = W/(W+L),E_WPct = 1/(1+(RA/R)^2),diff=WPct-E_WPct,Game_diff=(W+L)*diff) %>%
  rename(pct_diff=diff) %>%
  mutate(gm = ifelse(yearID==2004,"Duquette",ifelse(yearID>2010, "Alderson","Minaya")))

#19
group_by(ben.subset8,gm) %>%
  summarize(seasons=n(),totalWins=sum(W),totalLosses=sum(L),avg_WPct=mean(WPct))

#21
group_by(ben.subset8,gm) %>%
  summarize(seasons=n(),avg_Wins=mean(W),avg_Losses=mean(L),avg_WPct=mean(WPct),
            avg_E_WPct=mean(E_WPct),avg_Game_diff=mean(Game_diff))



#challenge 
ben.challenge<-Teams %>% 
  filter(teamID=="NYN",yearID>=2004,yearID<=2012) %>%
  select(yearID,teamID,W,L,R,RA) %>%
  mutate(WPct = W/(W+L),E_WPct = 1/(1+(RA/R)^2),E_W=(E_WPct*(W+L))) %>%
  mutate(gm = ifelse(yearID==2004,"Duquette",ifelse(yearID>2010, "Alderson","Minaya")))%>%
  group_by(gm) %>%
  summarize(seasons=n(),avg_Wins=mean(W),avg_E_W=mean(E_W),avg_WPct=mean(WPct))%>%
  arrange(-avg_WPct)

head(ben.challenge)
