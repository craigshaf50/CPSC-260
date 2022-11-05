library(tidyverse)

CrimeAndInc<-read.csv(file='crime_and_incarceration_by_state_altered.csv')

### 1 ###
CrimeAndInc.1<-CrimeAndInc %>%
  filter(jurisdiction %in% c('IOWA','ILLINOIS','NORTH DAKOTA','SOUTH DAKOTA','MINNESOTA','WISCONSIN',
                             'NEBRASKA','KANSAS','MISSOURI','INDIANA','MICHIGAN','OHIO')) %>%
  select(jurisdiction,year,crimes_estimated,state_population,prisoner_count,murder_manslaughter,
         violent_crime_total,robbery,agg_assault,vehicle_theft,property_crime_total)

### 2 ###
CrimeAndInc.2<-CrimeAndInc.1 %>%
  select(jurisdiction, year, prisoner_count)
#a)"wide" data
CrimeAndInc.2Wide<-spread(CrimeAndInc.2, key=year, value=prisoner_count)
view(CrimeAndInc.2Wide)
#b)"long" data
CrimeAndInc.2Long<-gather(CrimeAndInc.2Wide, key=year, value=prisoner_count,-jurisdiction)
view(CrimeAndInc.2Long)

### 3 ###
hist(CrimeAndInc.1$state_population)
hist(CrimeAndInc.1$prisoner_count)
hist(CrimeAndInc.1$murder_manslaughter)
hist(CrimeAndInc.1$violent_crime_total)
hist(CrimeAndInc.1$robbery)
hist(CrimeAndInc.1$agg_assault)
hist(CrimeAndInc.1$vehicle_theft)
hist(CrimeAndInc.1$property_crime_total) 

### 4 ###
ggplot(CrimeAndInc.1, mapping=aes(x=year, y= murder_manslaughter, color=jurisdiction))+
  geom_point()+
  geom_line()

### 5 ###
ggplot(CrimeAndInc.1, mapping=aes(x=year, y= state_population, color=jurisdiction))+
  geom_point()+
  geom_line()
ggplot(CrimeAndInc.1, mapping=aes(x=year, y= prisoner_count, color=jurisdiction))+
  geom_point()+
  geom_line()
ggplot(CrimeAndInc.1, mapping=aes(x=year, y= violent_crime_total, color=jurisdiction))+
  geom_point()+
  geom_line()
ggplot(CrimeAndInc.1, mapping=aes(x=year, y= robbery, color=jurisdiction))+
  geom_point()+
  geom_line()
ggplot(CrimeAndInc.1, mapping=aes(x=year, y= agg_assault, color=jurisdiction))+
  geom_point()+
  geom_line()
ggplot(CrimeAndInc.1, mapping=aes(x=year, y= vehicle_theft, color=jurisdiction))+
  geom_point()+
  geom_line()
ggplot(CrimeAndInc.1, mapping=aes(x=year, y= property_crime_total, color=jurisdiction))+
  geom_point()+
  geom_line()

### 6 ###
#a) fix murder manslaughter
Wisconsin.MM<-CrimeAndInc.1 %>%
  filter(CrimeAndInc.1$jurisdiction=='WISCONSIN') %>%
  select(jurisdiction,year,murder_manslaughter)
mean(Wisconsin.MM$murder_manslaughter,na.rm=TRUE) #176.4667
median(Wisconsin.MM$murder_manslaughter,na.rm=TRUE) #165

CrimeAndInc.1[96,6]=165 #replace with median

#b) fix vehicle theft
WiscMinn.Veh<-CrimeAndInc.1 %>%
  filter(jurisdiction %in% c('MINNESOTA',"WISCONSIN"))%>%
  select(jurisdiction,year,vehicle_theft) #just used to easily compare the states

CrimeAndInc.1[96,10]=CrimeAndInc.1[90,10] #replace NA with 10195

#c)fix robbery
CrimeAndInc.1[122,8] #indiana 2011 - the big jump on the graph

CrimeAndInc.1[122,8]=6977

#d) fix state population
CrimeAndInc.1[161,4] #999877
CrimeAndInc.1[161,4]=9929848

#e) fix property crime total
CrimeAndInc.1[c(8,20,32),11] #our zeros
CrimeAndInc.1[44,11] #2004 Nebraska - 61512
CrimeAndInc.1[c(8,20,32),11]=61512

### 7 ###
ggplot(CrimeAndInc.1, mapping=aes(x=prisoner_count, y=vehicle_theft))+
  geom_point()+
  geom_smooth()

### 8 ###
ggplot(CrimeAndInc.1, mapping=aes(x=prisoner_count, y=vehicle_theft, color=as.factor(year)))+
  geom_point()
ggplot(CrimeAndInc.1, mapping=aes(x=prisoner_count, y=vehicle_theft, color=as.factor(jurisdiction)))+
  geom_point()

### 9 ###
table(CrimeAndInc.1$crimes_estimated)

### 10 ###
#a) change crimes_estimated to 1 and 0 instead of true/false
str(CrimeAndInc.1) #character data for crimes_esimated
crimes<-CrimeAndInc.1 #just incase I mess up
ii<-CrimeAndInc.1$crimes_estimated=='FALSE'
jj<-CrimeAndInc.1$crimes_estimated=='TRUE'
CrimeAndInc.1[jj,3]=1
CrimeAndInc.1[ii,3]=0
CrimeAndInc.1$crimes_estimated=as.numeric(CrimeAndInc.1$crimes_estimated)

#b) replace missing value with mode
table(CrimeAndInc.1$crimes_estimated) #mode is 0 since it occurs the most
CrimeAndInc.1[190,3] #the NA
CrimeAndInc.1[190,3]=0
CrimeAndInc.1$crimes_estimated #No more NA

#c) check if it makes sense
CrimeAndInc.1Wide<-CrimeAndInc.1 %>%
  select(jurisdiction,year,crimes_estimated)%>%
  spread(key=year, value=crimes_estimated)

### 11 ###
CrimeAndInc.p100k<-CrimeAndInc.1 %>%
  mutate(VCT.p100k=(violent_crime_total/state_population)*100000,
         PCT.p100k=(property_crime_total/state_population)*100000)
CrimeAndInc.p100k

### 12 ###
ggplot(CrimeAndInc.p100k, mapping=aes(x=year, y= VCT.p100k, color=jurisdiction))+
  geom_point()+
  geom_line()
ggplot(CrimeAndInc.p100k, mapping=aes(x=year, y= violent_crime_total, color=jurisdiction))+
  geom_point()+
  geom_line()

### 13 ###
min(CrimeAndInc.p100k$prisoner_count) #1088 min
max(CrimeAndInc.p100k$prisoner_count) #52240 max
52240-1088 #51152 range
prisoner_count.MinMax<-(CrimeAndInc.p100k$prisoner_count-1088)/51152
prisoner_count.MinMax
hist(prisoner_count.MinMax)
CrimeAndInc.p100k<-mutate(CrimeAndInc.p100k,prisoner_count.MinMax=prisoner_count.MinMax)
CrimeAndInc.p100k
