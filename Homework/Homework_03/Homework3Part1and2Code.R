library(tidyverse)
### PART 1 ###
##Q1 - load CSV
HLValues<-read.csv(file='house-and-land-values.csv')
head(HLValues)

##Q2 - 5 subsets
#just iowa
HLValues.IA<-HLValues[(HLValues$State)=="IA",]
head(HLValues.IA)
tail(HLValues.IA)

#just california from 1985
ii<-(HLValues$State == "CA") & (HLValues$Date >= 1985)
HLValues.CA1985<-HLValues[ii,]
head(HLValues.CA1985)
tail(HLValues.CA1985)

#just Iowa, California, Massachusetts, and Texas
states<-c("IA","CA","MA","TX")
jj<-HLValues$State %in% states
HLValues.ICMT<-HLValues[jj,]
head(HLValues.ICMT)
tail(HLValues.ICMT)

#just data with specified region (not NA)
remove.ii<-which(is.na(HLValues$region))
HLValues.noNA<-HLValues[-remove.ii,]
head(HLValues.noNA)
tail(HLValues.noNA)

#just data for 2013 first quarter (most recent data in the set -> 2013.25)
HLValues.2013.25<-HLValues[(HLValues$Date == 2013.25),]
head(HLValues.2013.25)
tail(HLValues.2013.25)

##Q3 - histogram of iowa home values 
ggplot(data=HLValues.IA)+
  geom_histogram(mapping=aes(x=Home.Value),color='green',binwidth = 50000)

#ggsave('iowaHLValueHistogram.png', width = 14, height = 10, dpi = "retina")

##Q4 - scatterplot of iowa home values from 1975 to 2013
ggplot(data = HLValues.IA)+
  geom_point(mapping=aes(x=Date,y=Home.Value),color='green')+
  labs(x="Date (in years)",y="Home Value (in US Dollars)", 
       title="Home Value in Iowa from 1975 to 2013")

#ggsave('iowaHomeValue1975-2013.png', width = 14, height = 10, dpi = "retina")

##Q5 - add land values to graph from Q4 and change labels
ggplot(data = HLValues.IA)+
  geom_point(mapping=aes(x=Date,y=Home.Value),color='green')+
  geom_point(mapping=aes(x=Date,y=Land.Value),color='blue')+
  labs(x="Date (in years)",y="Home and Land Values (in US Dollars)", 
       title="Home and Land Values in Iowa from 1975 to 2013")

#ggsave('iowaHomeLandValue1975-2013.png', width = 14, height = 10, dpi = "retina")

##Q6 - add legend to graph
Graph.Colors<-c('green','blue')

ggplot(data = HLValues.IA)+
  geom_point(mapping=aes(x=Date,y=Home.Value,color='Home.Value'))+
  geom_point(mapping=aes(x=Date,y=Land.Value,color='Land.Value'))+
  labs(x="Date (in years)",y="Home and Land Values (in US Dollars)", 
       title="Home and Land Values in Iowa from 1975 to 2013", color="Legend")+
  scale_color_manual(values=Graph.Colors)

#ggsave('iowaHLVwithLegend1975-2013.png', width = 14, height = 10, dpi = "retina")

##Q7 - add structure cost to graph
Graph.Colors2<-c('green','blue','red')
ggplot(data = HLValues.IA)+
  geom_point(mapping=aes(x=Date,y=Home.Value,color='Home.Value'))+
  geom_point(mapping=aes(x=Date,y=Land.Value,color='Land.Value'))+
  geom_point(mapping=aes(x=Date,y=Structure.Cost,color='Structure.Cost'))+
  labs(x="Date (in years)",y="Structure Cost, Home, and Land Values (in US Dollars)", 
       title="Structure Cost, Home, and Land Values in Iowa from 1975 to 2013", color="Legend")+
  scale_color_manual(values=Graph.Colors2)

#ggsave('iowaSHLV1975-2013.png', width = 14, height = 10, dpi = "retina")

##Q8 - Q7's graph with California's data since 1985
ggplot(data = HLValues.CA1985)+
  geom_point(mapping=aes(x=Date,y=Home.Value,color='Home.Value'))+
  geom_point(mapping=aes(x=Date,y=Land.Value,color='Land.Value'))+
  geom_point(mapping=aes(x=Date,y=Structure.Cost,color='Structure.Cost'))+
  labs(x="Date (in years)",y="Structure Cost, Home, and Land Values (in US Dollars)", 
       title="Structure Cost, Home, and Land Values in California from 1985 to 2013", color="Legend")+
  scale_color_manual(values=Graph.Colors2)

#ggsave('californiaSHLV1975-2013.png', width = 14, height = 10, dpi = "retina")






### PART 2 ###
##Q9 - box plot for home values of 4 states, all states different colors
ggplot(data=HLValues.ICMT)+
  geom_boxplot(mapping=aes(x=State,y=Home.Value, color=State),show.legend = FALSE)

#ggsave('ICMT-HLV.png', width = 14, height = 10, dpi = "retina")

##Q10 - box plot for structure cost
ggplot(data=HLValues.ICMT)+
  geom_boxplot(mapping=aes(x=State,y=Structure.Cost), color="blue", 
               fill="turquoise", alpha=.5, notch=TRUE)

#ggsave('ICMT-Struct-HLV.png', width = 14, height = 10, dpi = "retina")

##Q11 - violin plot for land value
ggplot(data=HLValues.ICMT)+
  geom_violin(mapping=aes(x=State,y=Land.Value,fill=State))

#ggsave('ICMT-Land-HLV.png', width = 14, height = 10, dpi = "retina")

##Q12 - home values based on region
ggplot(data=HLValues)+
  geom_freqpoly(mapping=aes(x=Home.Value,color=region),binwidth=15000)

#ggsave('HLV-freqpoly.png', width = 14, height = 10, dpi = "retina")

##Q13 - fill in with geom_area
ggplot(data=HLValues)+
  geom_area(mapping=aes(x=Home.Value,fill=region),stat='bin',binwidth=15000)

#ggsave('HLV-geomarea.png', width = 14, height = 10, dpi = "retina")

##Q14 - Q13 without NA & black lines added
ggplot(data=HLValues.noNA)+
  geom_area(mapping=aes(x=Home.Value,fill=region),stat='bin',binwidth=15000,color='black')

#ggsave('HLV-NoNA-geomarea.png', width = 14, height = 10, dpi = "retina")

##Q15 - ridgeline chart
library(ggridges)
ggplot(data=HLValues.noNA,mapping=aes(x=Home.Value,y=region,fill=region))+
  geom_density_ridges()

#ggsave('HLV-NoNA-ridgeline.png', width = 14, height = 10, dpi = "retina")

##Q16 - scatterplot
ggplot(data=HLValues.2013.25)+
  geom_point(mapping=aes(x=Land.Value,y=Structure.Cost,
                         size=Land.Share..Pct.,color=Home.Price.Index),alpha=.9)+
  labs(x="Land Value (in US Dollars)",y="Structure Cost (in US Dollars)", 
      title="Land Value vs Structure Cost for the First Quarter of 2013")

#ggsave('HLV-2013-25.png', width = 14, height = 10, dpi = "retina")

##Q17 - Q16 with changes
library(ggrepel)
ggplot(data=HLValues.2013.25,mapping=aes(x=log(Land.Value),y=Structure.Cost))+
  geom_point(mapping=aes(size=Land.Share..Pct.,color=Home.Price.Index),alpha=.9)+
  geom_smooth(mapping=aes(linetype=region),color='red',se=FALSE)+
  geom_text_repel(mapping=aes(label=State))+
  labs(x="Log of Land Value (in US Dollars)",y="Structure Cost (in US Dollars)", 
       title="Log of Land Value vs Structure Cost for the First Quarter of 2013")

#ggsave('HLV-2013-25-part2.png', width = 14, height = 10, dpi = "retina")



