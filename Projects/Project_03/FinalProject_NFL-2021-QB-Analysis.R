library(tidyverse)
library(rvest)



### Webpage 1 - OTC: quarterback salaries table ###
OTC<-read_html('https://overthecap.com/position/quarterback/2021/')
tb<-OTC%>%html_nodes("table") %>% html_table()
qb.2021compensation<-tb[[1]]
## cleaning the data ##
# fix column names
table.col.names<-c('Player','Team.Name','Cap.Number','Cash.Spent')
colnames(qb.2021compensation)<-table.col.names
#add taysom hill row with contract details since hew was excluded from the data set(Taysom plays multiple
#positions so it is likely he was excluded from the list because he can be classified as something other than a QB.)
th.df<-c("Taysom Hill","Saints",'$7,259,000','$12,159,000')
names(th.df)<-c('Player','Team.Name','Cap.Number','Cash.Spent')
qb.2021compensation<-rbind(qb.2021compensation,th.df)
#turn character data to numeric & remove symbols like "$" and ","
cap.num<-gsub(",","",qb.2021compensation$Cap.Number)
cap.num<-str_extract(cap.num, "\\d+") %>% as.numeric()
qb.2021compensation$Cap.Number<-cap.num
cash.num<-gsub(",","",qb.2021compensation$Cash.Spent)
cash.num<-str_extract(cash.num, "\\d+") %>% as.numeric()
qb.2021compensation$Cash.Spent<-cash.num
#add total compensation column (cap.number + cash.spent)
qb.2021compensation<-mutate(qb.2021compensation, Total.Comp=Cap.Number+Cash.Spent)
#rename Gardner Minshew to Gardner Minshew II, Matt Stafford to Matthew Stafford,
#and Phillip Walker to PJ Walker for compatability with other dataframes
qb.2021compensation$Player[71]="Gardner Minshew II"
qb.2021compensation$Player[9]="Matthew Stafford"
qb.2021compensation$Player[63]="PJ Walker"
#fix team names for merging (creating a new dataframe with pairs to merge and then removing old names)
#also including conference and division info for analysis later
Team<-c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET","GNB","HOU","IND","JAX","KAN",
        "LAC","LAR","LVR","MIA","MIN","NOR","NWE","NYG","NYJ","PHI","PIT","SEA","SFO","TAM","TEN","WAS")
Mascot<-c("Cardinals","Falcons","Ravens","Bills","Panthers","Bears","Bengals","Browns","Cowboys","Broncos",
          "Lions","Packers","Texans","Colts","Jaguars","Chiefs","Chargers","Rams","Raiders","Dolphins",
          "Vikings","Saints","Patriots","Giants","Jets","Eagles","Steelers","Seahawks","49ers","Buccaneers",
          "Titans","Commanders")
teamdata<-data.frame(Team,Mascot)
teamdata<-mutate(teamdata, Conf= ifelse (Team %in% c("ARI","ATL","CAR","CHI","DAL","DET","GNB","LAR","MIN","NOR",
                                           "NYG","PHI","SEA","SFO","TAM","WAS"),'NFC','AFC'))
teamdata<-mutate(teamdata, Div = ifelse(Team %in% c("ARI","LAR","SEA","SFO"),"NFC West",
                                        ifelse(Team %in% c("ATL","CAR","NOR","TAM"),"NFC South",
                                        ifelse(Team %in% c("CHI","DET","GNB","MIN"),"NFC North",
                                        ifelse(Team %in% c("DAL","NYG","PHI","WAS"),"NFC East",
                                        ifelse(Team %in% c("DEN","KAN","LAC","LVR"),"AFC West",
                                        ifelse(Team %in% c("HOU","IND","JAX","TEN"),"AFC South",
                                        ifelse(Team %in% c("BAL","CIN","CLE","PIT"),"AFC North","AFC East"))))))))
#Merge teamdata to compensation data set
qb.2021compensation<-left_join(qb.2021compensation,teamdata,by=c("Team.Name"="Mascot"))



### Webpage 2 - PFR: Passing Stats ###
PFR.P<-read_html('https://www.pro-football-reference.com/years/2021/passing.htm')
tb2<-PFR.P%>%html_nodes("table") %>% html_table()
qb.2021passing<-tb2[[1]]
## cleaning and preprocessing the data ##
#select columns necessary for analysis (remove stats that do not relate to my questions) and renaming columns
qb.2021passing<-qb.2021passing[,-c(1,18,20,26,27,28,29)] %>%
  rename(Team=Tm,P.Att=Att, Cmp.Pct = `Cmp%`,TD.Pct =`TD%`,Int.Pct = `Int%`,P.Yds = Yds, P.TD=TD,
         P.First.Downs=`1D`,P.YdsPerAtt=`Y/A`,P.YdsPerCmp=`Y/C`,P.YdsPerGame=`Y/G`,Q4.Comebacks=`4QC`)
#Separate QBrec into Wins, Losses, Ties
qb.2021passing<-separate(qb.2021passing, QBrec, c("Wins","Losses","Ties"), sep="-")
#change character data that should be numeric to numeric (age,games,games started,yards,etc)
qb.2021passing$Age=as.numeric(qb.2021passing$Age)
qb.2021passing$G=as.numeric(qb.2021passing$G)
qb.2021passing$GS=as.numeric(qb.2021passing$GS)
qb.2021passing$Wins=as.numeric(qb.2021passing$Wins)
qb.2021passing$Losses=as.numeric(qb.2021passing$Losses)
qb.2021passing$Ties=as.numeric(qb.2021passing$Ties)
qb.2021passing$Cmp=as.numeric(qb.2021passing$Cmp)
qb.2021passing$P.Att=as.numeric(qb.2021passing$P.Att)
qb.2021passing$Cmp.Pct=as.numeric(qb.2021passing$Cmp.Pct)
qb.2021passing$P.Yds=as.numeric(qb.2021passing$P.Yds)
qb.2021passing$P.TD=as.numeric(qb.2021passing$P.TD)
qb.2021passing$TD.Pct=as.numeric(qb.2021passing$TD.Pct)
qb.2021passing$Int=as.numeric(qb.2021passing$Int)
qb.2021passing$Int.Pct=as.numeric(qb.2021passing$Int.Pct)
qb.2021passing$P.First.Downs=as.numeric(qb.2021passing$P.First.Downs)
qb.2021passing$P.YdsPerAtt=as.numeric(qb.2021passing$P.YdsPerAtt)
qb.2021passing$P.YdsPerCmp=as.numeric(qb.2021passing$P.YdsPerCmp)
qb.2021passing$P.YdsPerGame=as.numeric(qb.2021passing$P.YdsPerGame)
qb.2021passing$Rate=as.numeric(qb.2021passing$Rate)
qb.2021passing$QBR=as.numeric(qb.2021passing$QBR)
qb.2021passing$Sk=as.numeric(qb.2021passing$Sk)
qb.2021passing$Q4.Comebacks=as.numeric(qb.2021passing$Q4.Comebacks)
qb.2021passing$GWD=as.numeric(qb.2021passing$GWD)
#get rid of rows that aren't Quarterbacks (other positions, filler rows, etc)
ii<-((qb.2021passing$Pos == 'QB')|(qb.2021passing$Pos == 'qb')|(qb.2021passing$Pos == ''))
qb.2021passing<-qb.2021passing[ii,]
#some of the empty Pos columns are not quarterbacks, so we have to remove them (had to find them manually)
qb.2021passing<-qb.2021passing[-c(82,81,80,79,75,73,71,65),]
#all players left are QBs, but they have 'QB','qb', or ''. So we need to make them all the same thing
qb.2021passing$Pos='QB'
#remove symbols by player names
pname<-gsub("[^[:alnum:] ]","",qb.2021passing$Player)
qb.2021passing$Player<-pname
#remove quarterbacks that did not start a game (these quarterbacks have very small sample size)
qb.2021passing<-qb.2021passing %>% filter(GS>=1)
#-- fix NA's for columns Q4.Comebacks & GWD --#
#for fourth quarter comebacks if they are empty values, then they had 0 fourth quarter comebacks recorded
jj<-is.na(qb.2021passing$Q4.Comebacks)
qb.2021passing$Q4.Comebacks[jj]=0
#the same logic applies for GameWinningDrives (GWD)
kk<-is.na(qb.2021passing$GWD)
qb.2021passing$GWD[kk]=0



### Webpage 3 - PFR: Rushing Stats
PFR.R<-read_html('https://www.pro-football-reference.com/years/2021/rushing.htm')
tb3<-PFR.R%>%html_nodes("table") %>% html_table()
qb.2021rushing<-tb3[[1]]
#fix column names (they are in the first line and some need to be changed to meet naming conventions or for clarity)
table.col.names<-qb.2021rushing[1,]
colnames(qb.2021rushing)<-table.col.names
#select columns necessary for analysis (remove stats that do not relate to questions) and renaming columns
qb.2021rushing<-qb.2021rushing[,-c(1,12)] %>%
  rename(Team=Tm,R.Att=Att,R.Yds = Yds, R.TD=TD,R.First.Downs=`1D`,R.YdsPerAtt=`Y/A`,R.YdsPerGame=`Y/G`)
#change character data that should be numeric to numeric (age,games,games started,yards,etc)
qb.2021rushing$Age=as.numeric(qb.2021rushing$Age)
qb.2021rushing$G=as.numeric(qb.2021rushing$G)
qb.2021rushing$GS=as.numeric(qb.2021rushing$GS)
qb.2021rushing$R.Att=as.numeric(qb.2021rushing$R.Att)
qb.2021rushing$R.Yds=as.numeric(qb.2021rushing$R.Yds)
qb.2021rushing$R.TD=as.numeric(qb.2021rushing$R.TD)
qb.2021rushing$R.First.Downs=as.numeric(qb.2021rushing$R.First.Downs)
qb.2021rushing$R.YdsPerAtt=as.numeric(qb.2021rushing$R.YdsPerAtt)
qb.2021rushing$R.YdsPerGame=as.numeric(qb.2021rushing$R.YdsPerGame)
qb.2021rushing$Fmb=as.numeric(qb.2021rushing$Fmb)
#get rid of rows that aren't Quarterbacks (other positions, filler rows, etc)
hh<-((qb.2021rushing$Pos == 'QB')|(qb.2021rushing$Pos == 'qb')|(qb.2021rushing$Pos == ''))
qb.2021rushing<-qb.2021rushing[hh,]
#since our other data table only includes quarterbacks with at least 1 start, we will remove players without starts
qb.2021rushing<-qb.2021rushing %>% filter(GS>=1)
#now it is much easier to find other positions that made it through and remove them manually
qb.2021rushing<-qb.2021rushing[-c(1,2,6,10,17,19,32,60,61,66,67,68,69,73,74,75,77),]
#now every player left is a quarterback so we can make the position column all the same
qb.2021rushing$Pos='QB'
#remove symbols by player names
pname<-gsub("[^[:alnum:] ]","",qb.2021rushing$Player)
qb.2021rushing$Player<-pname
#no empty cells or NA's so this data set is done



### Merge the three tables for full Quarterback statistics & other data ###
#merge our passing and rushing dataframes
qb.2021stats<-left_join(qb.2021passing,qb.2021rushing,by=c("Player","Team","Age","Pos","G","GS"))
#looking at the dataset, two quarterbacks (row 57 and 60) had no rushing statistics (filled with NAs), so we will fill in zeros for the rushing columns
qb.2021stats[c(57,60),c(27:33)]=0
#Now we need to fix Josh Johnson's Team situation ("2TM"). Since his most recent team was Baltimore we will replace it with "BAL"
qb.2021stats[45,2]="BAL"
#Now that that is fixed, we can merge our compensation dataframe
qb.2021master<-left_join(qb.2021stats,qb.2021compensation, by=c("Player","Team"))
#data frames fully merged!



### question 1: do higher paid qbs win more games? better win pct?
subset1.q1<-qb.2021master %>% select(Player,Wins,Losses,Ties,Cap.Number,Total.Comp) %>%
  mutate(Win.Pct=Wins/(Wins+Losses))
ggplot(subset1.q1,mapping=aes(x=Cap.Number,y=Wins))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset1.q1,mapping=aes(x=Total.Comp,y=Wins))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset1.q1,mapping=aes(x=Cap.Number,y=Win.Pct))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset1.q1,mapping=aes(x=Total.Comp,y=Win.Pct))+
  geom_point()+
  geom_smooth(method=lm)
#looks more like higher paid qbs play more and have greater opportunity to win games (hence higher correlation)
#looks like data is so varied for win.pct and compensation that it is hard to say if there is a relationship.
#Could have something to do with amount of data. Exclude players < 4 games started
subset2.q1<-qb.2021master %>% filter(GS >= 4) %>%
  select(Player,Wins,Losses,Ties,Cap.Number,Total.Comp) %>%
  mutate(Win.Pct=Wins/(Wins+Losses))
ggplot(subset2.q1,mapping=aes(x=Cap.Number,y=Wins))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset2.q1,mapping=aes(x=Total.Comp,y=Wins))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset2.q1,mapping=aes(x=Cap.Number,y=Win.Pct))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset2.q1,mapping=aes(x=Total.Comp,y=Win.Pct))+
  geom_point()+
  geom_smooth(method=lm)
#more of a correlation between number of wins and cap.number and wins and total.comp.
#win.pct looks like it correlates more with cap.number and total.comp than before
#Answer: After excluding players with a smaller sample of games, the data shows that players 
#that are paid more tend to win more games and win at a higher percentage. However, 
#this feels more like a symptom of something else rather than being a direct relationship.


### Question 2: Are higher paid QBs better?
##1) score more?
subset1.q2<-qb.2021master %>% select(Player, P.TD, R.TD, Cap.Number, Total.Comp)%>%
  mutate(Total.TD=P.TD+R.TD)
view(subset1.q2)
ggplot(subset1.q2,mapping=aes(x=Cap.Number,y=Total.TD))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset1.q2,mapping=aes(x=Total.Comp,y=Total.TD))+
  geom_point()+
  geom_smooth(method=lm)
#players with less than 4 games are removed
subset2.q2<-qb.2021master %>% filter(GS >= 4) %>%
  select(Player, P.TD, R.TD, Cap.Number, Total.Comp)%>%
  mutate(Total.TD=P.TD+R.TD)
view(subset2.q2)
ggplot(subset2.q2,mapping=aes(x=Cap.Number,y=Total.TD))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset2.q2,mapping=aes(x=Total.Comp,y=Total.TD))+
  geom_point()+
  geom_smooth(method=lm)
#touchdown percentage
subset3.q2<-qb.2021master %>% filter(GS>=4) %>%
  select(Player, P.Att,R.Att,P.TD, R.TD, Cap.Number, Total.Comp)%>%
  mutate(Total.TD.Pct=(P.TD+R.TD)/(P.Att+R.Att))
view(subset3.q2)
ggplot(subset3.q2,mapping=aes(x=Cap.Number,y=Total.TD.Pct))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset3.q2,mapping=aes(x=Total.Comp,y=Total.TD.Pct))+
  geom_point()+
  geom_smooth(method=lm)
##2) less turnovers
#look at turnover percentage
subset4.q2<-qb.2021master %>% filter(GS>=4) %>%
  select(Player, P.Att,R.Att,Int,Fmb, Cap.Number, Total.Comp)%>%
  mutate(TO.Pct=(Int+Fmb)/(P.Att+R.Att))
view(subset4.q2)
ggplot(subset4.q2,mapping=aes(x=Cap.Number,y=TO.Pct))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset4.q2,mapping=aes(x=Total.Comp,y=TO.Pct))+
  geom_point()+
  geom_smooth(method=lm)
##3) passer rating
subset5.q2<-qb.2021master %>% filter(GS>=4) %>%
  select(Player, Rate, Cap.Number, Total.Comp)
ggplot(subset5.q2,mapping=aes(x=Cap.Number,y=Rate))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(subset5.q2,mapping=aes(x=Total.Comp,y=Rate))+
  geom_point()+
  geom_smooth(method=lm)
#same graph but with players on rookie deals in different color
rookie.deals<-c("Baker Mayfield","Justin Fields","Kyler Murray","Mac Jones","Joe Burrow","Daniel Jones",
               "Sam Darnold","Tua Tagovailoa","Justin Herbert","Lamar Jackson","Davis Mills",
               "Jalen Hurts", "Zach Wilson")
subset6.q2<-qb.2021master %>% filter(GS>=4) %>%
  select(Player, Rate, Cap.Number, Total.Comp) %>% 
  mutate(Rookie.Deal= ifelse(Player %in% rookie.deals,"Yes","No"))
ggplot(subset6.q2,mapping=aes(x=Total.Comp,y=Rate))+
  geom_point(mapping=aes(x=Cap.Number,y=Rate,color=Rookie.Deal))+
  geom_smooth(method=lm)+
  scale_color_brewer(palette='Set1')
ggplot(subset6.q2,mapping=aes(x=Total.Comp,y=Rate))+
  geom_point(mapping=aes(x=Total.Comp,y=Rate,color=Rookie.Deal))+
  geom_smooth(method=lm)+
  scale_color_brewer(palette='Set1')


### Question 3: Which conference spends the most on QB? Division? Do they win more?
#Conference. NFC spends the most and they win more
qb.2021master%>% select(Conf,Div,Total.Comp,Wins,Losses,Ties,Age)%>%
  group_by(Conf)%>%
  summarise(TotalSpent=sum(Total.Comp),TotalW=sum(Wins),TotalL=sum(Losses),TotalT=sum(Ties),WinPct=TotalW/(TotalW+TotalL),AvgAge=mean(Age))%>%
  arrange(by=-WinPct)
#division. Different story. 
#NFC west spends second most and wins most 
#However, next three division spend the least and win 2nd 3rd and 4th most.
qb.2021master%>% select(Conf,Div,Total.Comp,Wins,Losses,Ties,Age)%>%
  group_by(Div)%>%
  summarise(TotalSpent=sum(Total.Comp),TotalW=sum(Wins),TotalL=sum(Losses),TotalT=sum(Ties),WinPct=TotalW/(TotalW+TotalL),AvgAge=mean(Age))%>%
  arrange(by=-WinPct)



### Final Graphs for article ###
ggplot(subset2.q1,mapping=aes(x=Total.Comp,y=Wins))+
  geom_point()+
  geom_smooth(method=lm)+
  ggtitle("Quarterback's Total Compensation vs Wins")+
  labs(x='Total Compensation',
       y='Wins',subtitle = "Excluding QB's with less than 4 starts in 2021")
ggplot(subset2.q1,mapping=aes(x=Total.Comp,y=Win.Pct))+
  geom_point()+
  geom_smooth(method=lm)+
  ggtitle("Quarterback's Total Compensation vs Win Percentage")+
  labs(x='Total Compensation',
       y='Win Percentage',subtitle = "Excluding QB's with less than 4 starts in 2021")
ggplot(subset6.q2,mapping=aes(x=Total.Comp,y=Rate))+
  geom_point(mapping=aes(x=Total.Comp,y=Rate,color=Rookie.Deal))+
  geom_smooth(method=lm)+
  scale_color_brewer(palette='Set1')+
  geom_smooth(method=lm)+
  ggtitle("Quarterback's Total Compensation vs Passer Rating")+
  labs(x='Total Compensation',
       y='Passer Rating',subtitle = "Excluding QB's with less than 4 starts in 2021")
