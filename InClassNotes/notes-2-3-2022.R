library(ggplot2)


ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=class))+
  geom_smooth(mapping=aes(x=displ,y=hwy),color='red')

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy,color=class))

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy))+
  facet_wrap(~class)

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy,linetype=class))

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy,group=class))


str(mpg)
head(mpg)

#change cyl from int to factor
mpg$cyl<-as.factor(mpg$cyl)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=cyl))

#find the 5 cylinder cars
ii<-mpg$cyl == '5'
mpg[ii,]


## Bar Chart
head(diamonds)
View(diamonds)

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut),fill="blue")

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut,fill=cut))

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut,fill=clarity))

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut))+
  facet_wrap(~clarity)

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut,fill=cut))+
  coord_flip()

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut,fill=cut))+
  coord_polar()+
  labs(x=NULL,y=NULL, title = "Coxcomb Chart")


##boxplot
ggplot(data=diamonds)+
  geom_boxplot(mapping=aes(x='',y=price))

ggplot(data=diamonds)+
  geom_boxplot(mapping=aes(x=cut,y=price,color=cut))+
  coord_flip()

##histogram
ggplot(data=diamonds)+
  geom_histogram(mapping=aes(x=carat),binwidth = .5)+
  coord_cartesian(xlim=c(0,3))

#frequency polygon
ggplot(data=diamonds)+
  geom_freqpoly(mapping=aes(x=carat,color=cut),binwidth = .1)+
  coord_cartesian(xlim=c(0,3))

#add text to graph
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  geom_text(mapping=aes(x=displ,y=hwy,label=model))


ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  geom_text(data=mpg[c(12,54,176,100),],
            mapping=aes(x=displ,y=hwy,label=model),
            color='red',vjust='top',hjust="left")

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  geom_text(mapping=aes(x=displ,y=hwy,label=model),alpha=.5)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  geom_text(data=mpg[c(12,54,176,100),],
            mapping=aes(x=displ,y=hwy,label=model),
            color='red',alpha=.5)
#geom_label()
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  geom_label(data=mpg[c(12,54,176,100),],
            mapping=aes(x=displ,y=hwy-0.5,label=model),
            color='red',vjust='top',hjust="left")

##colors/themes
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=cyl))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=cyl))+
  scale_color_brewer(palette='Spectral')

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=cyl))+
  scale_color_brewer(palette='Set1')

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=cyl))+
  theme_bw()

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=cyl))+
  theme_dark()

#library
#install.packages('ggridges')
library(ggridges)

ggplot(data=diamonds,mapping=aes(x=price,y=cut,fill=cut))+
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


#more color stuff
ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut),fill="violetred2")
#pulls up list of colors
colors()
  