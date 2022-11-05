library(tidyverse)
library(ggrepel)
econ<-read.csv(file='EconomistData.csv')

head(econ)
str(econ)
#HDI human development index
#CPI curruption perception index

#region->color,size->HDI.Rank
ggplot(data=econ)+
  geom_point(mapping=aes(x=CPI,y=HDI,color=Region,size=HDI.Rank))

pointsToLabel<-c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                 "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                 "India", "Italy", "China", "South Africa", "Spain",
                 "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                 "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                 "New Zealand", "Singapore")
colors<-c('#24576D','#099DD7','#28AADC','#248E84','#F2583F','#96503F')
econ2<-econ
econ2$Region <- factor(econ2$Region,
                      levels = c("EU W. Europe",
                                 "Americas",
                                 "Asia Pacific",
                                 "East EU Cemt Asia",
                                 "MENA",
                                 "SSA"),
                      labels = c("OECD",
                                 "Americas",
                                 "Asia &\nOceania",
                                 "Central &\nEastern Europe",
                                 "Middle East &\nnorth Africa",
                                 "Sub-Saharan\nAfrica"))

ii<-econ$Country %in% pointsToLabel
econ.subset<-econ[ii,]
#all points size 3
#add smooth line over
#remove confidence interval, make line red
#shape 1 is donut, stroke is width of lines
#add correct labels, colors
#used ggrepel to move labels off their points
ggplot(data=econ,mapping=aes(x=CPI,y=HDI))+
  geom_point(mapping=aes(color=Region), size=3, shape=1, stroke=1.25)+
  geom_smooth(method='lm',color='red',se=FALSE)+
  labs(x='Corruption Perception Index 2011 (10 = least corrupt)',
       y='Human Development Index 2011 (1 = best)')+
  scale_color_manual(values=colors)+
  geom_text_repel(data=econ.subset,mapping=aes(x=CPI,y=HDI,label=Country))

#fix region labels by replacing econ w/ econ2 for data
#store in p
#add in formula for curve
p<-ggplot(data=econ2,mapping=aes(x=CPI,y=HDI))+
    geom_point(mapping=aes(color=Region), size=3, shape=1, stroke=1.25)+
    geom_smooth(method='lm',formula=y~x+log(x),color='red',se=FALSE)+
    labs(x='Corruption Perception Index 2011 (10 = least corrupt)',
       y='Human Development Index 2011 (1 = best)')+
    scale_color_manual(values=colors)+
    geom_text_repel(data=econ.subset,mapping=aes(x=CPI,y=HDI,label=Country))
#modify the plot's axis
p+scale_x_continuous(limits=c(1,10),breaks=1:10)+
  scale_y_continuous(limits=c(0.2,1),breaks=seq(0.2,1, by=0.1))+
  ggtitle("Corruption and human development")+
  theme_minimal()+
  theme(legend.position = 'top',
        legend.text = element_text(size=11,color='gray20'),
        axis.text = element_text(color='gray40'),
        axis.title.x = element_text(size=8,color='gray40',face='italic'),
        axis.title.y = element_text(size=8,color='gray40',face='italic'),
        axis.line.x = element_line(size=.5,color='gray40'),
        panel.grid.major.y = element_line(size=.5,color='gray40'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())+
  guides(color=guide_legend(nrow=1))

ggsave('econGraph.png', width = 14, height = 10, dpi = "retina")  

