library(tidyverse)
library(babynames)

bb.names<-read.csv(file=file.choose())

joseph<-bb.names %>%
  filter(name=="Joseph"&sex=="M")


d<-rep(joseph$age_today, joseph$est_alive_today_thousands)
median(d) #39
#2014-39=1975 median
#1975 data
j.1975<-joseph %>%
  filter(year==1975)

#add text - make dataframe with the text and the x and y coordinate of where it will go
text<-c("Number of Josephs\n born each year","The median\n living Joseph is 39\n years old",
        "Number of Josephs\n born each year\n estimated to be alive\n on Jan 1 2014")
xcoord<-c(1933,2000,1928)
ycoord<-c(34,35,10)
text.df<-data.frame(text,xcoord,ycoord)

#blue bars - use geom_col()
#dark blue line - median
p<-ggplot(data=joseph)+
  geom_col(mapping=aes(x=year,y=est_alive_today_thousands),
           fill="lightskyblue",color="white")+
  geom_col(data=j.1975,mapping=aes(x=year,y=est_alive_today_thousands),
           fill="dodgerblue",color="white")+
  geom_line(mapping=aes(x=year,y=count_thousands),lwd=1)+
  labs(x=NULL,y=NULL,title="Age Distribution of American Boys Named Joseph",
       subtitle="By year of birth")+
  geom_text(data=text.df,mapping=aes(label=text,x=xcoord,y=ycoord,color=text))+
  geom_curve(data=NULL,x=1990,xend=1975,y=35,yend=23,arrow=arrow(length=unit(0.3,"cm")),curvature=.65)+
  ylim(0,40)
p+scale_color_manual(guide=NULL, values=c("black","dodgerblue","gray40"))+
  theme(panel.background = element_rect(fill = "gray92"),
        plot.background = element_rect(fill = "gray92"),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold")) +
  scale_x_continuous(breaks=seq(1900, 2014, 10))
#ggsave('Joseph.Name.png', width = 14, height = 10, dpi = "retina")
#some names are commonly given to both m and f
#for example casey
#compare graphs for both m and f with name casey
casey<-bb.names %>%
  filter(name=="Casey")
ggplot(data=casey)+
  geom_col(mapping=aes(x=year,y=est_alive_today_thousands),
           fill="lightskyblue",color="white")+
  geom_line(mapping=aes(x=year,y=count_thousands),lwd=1)+
  labs(x=NULL,y=NULL,title="Age Distribution of Americans Named Casey",
       subtitle="By year of birth")+
  facet_wrap(~sex)

#compare joseph and josephine
joseph.josephine<-bb.names %>%
  filter((name=="Joseph" & sex=='M')|(name=="Josephine" & sex =='F'))
ggplot(data=joseph.josephine)+
  geom_col(mapping=aes(x=year,y=est_alive_today_thousands),
           fill="lightskyblue",color="white")+
  geom_line(mapping=aes(x=year,y=count_thousands),lwd=1)+
  labs(x=NULL,y=NULL,title="Age Distribution of Americans Named Joseph or Josephine",
       subtitle="By year of birth")+
  facet_wrap(~sex)

#compare your name with M/F or another name
#craig male vs joseph male
craig.joseph<-bb.names %>%
  filter((name=="Craig" & sex=='M')|(name=="Joseph" & sex=='M'))
ggplot(data=craig.joseph)+
  geom_col(mapping=aes(x=year,y=est_alive_today_thousands),
           fill="lightskyblue",color="white")+
  geom_line(mapping=aes(x=year,y=count_thousands),lwd=1)+
  labs(x=NULL,y=NULL,title="Age Distribution of American Boys Named Craig or Joseph",
       subtitle="By year of birth")+
  facet_wrap(~name)
#ggsave('CraigVsJoseph.png', width = 14, height = 10, dpi = "retina")  
#craig graph
craig<-bb.names %>%
  filter((name=="Craig" & sex=='M'))
#find median
d<-rep(craig$age_today, craig$est_alive_today_thousands)
median(d) #47
craig.1967<-craig %>%
  filter(year==1967)
#add text df
c.text<-c("Number of Craigs\n born each year","The median\n living Craig is 47\n years old",
        "Number of Craigs\n born each year\n estimated to be alive\n on Jan 1 2014")
c.xcoord<-c(1943,1990,1928)
c.ycoord<-c(12,12,5)
c.text.df<-data.frame(c.text,c.xcoord,c.ycoord)

ggplot(data=craig)+
  geom_col(mapping=aes(x=year,y=est_alive_today_thousands),
           fill="lightskyblue",color="white")+
  geom_col(data=craig.1967,mapping=aes(x=year,y=est_alive_today_thousands),
           fill="dodgerblue",color="white")+
  geom_line(mapping=aes(x=year,y=count_thousands),lwd=1)+
  geom_curve(data=NULL,x=1981,xend=1967,y=12,yend=8,arrow=arrow(length=unit(0.3,"cm")),curvature=.5)+
  ylim(0,15)+
  geom_text(data=c.text.df,mapping=aes(label=c.text,x=c.xcoord,y=c.ycoord,color=c.text))+
  labs(x=NULL,y=NULL,title="Age Distribution of American Boys Named Craig",
       subtitle="By year of birth")+
  scale_color_manual(guide=NULL, values=c("black","dodgerblue","gray40"))+
  theme(panel.background = element_rect(fill = "gray92"),
        plot.background = element_rect(fill = "gray92"),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold")) +
  scale_x_continuous(breaks=seq(1900, 2014, 10))
#ggsave('Craig.Name.png', width = 14, height = 10, dpi = "retina")
