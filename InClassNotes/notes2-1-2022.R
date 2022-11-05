library(tidyverse)

head(mpg)

view(mpg)


ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=class))
  
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,shape=class))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,size=class))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,alpha=class))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=class,shape=class))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy),color="blue")

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy),color="blue")

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy),color="blue",se=FALSE)

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy),se=FALSE)+
  geom_point(mapping=aes(x=displ,y=hwy,color=class))

ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_smooth(se=FALSE)+
  geom_point()
