# Buff tail: 10  1  37  5  12
# Garden bee: 8  3  19  6  4
# Red tail: 18  9  1  2  4
# Honeybee: 12, 13, 16, 9, 10
# Carder bee: 8, 27, 6, 32, 23

# Q1
buff.tail<-c(10,1,37,5,12)
garden.bee<-c(8,3,19,6,4)
red.tail<-c(18,9,1,2,4)
honey.bee<-c(12,13,16,9,10)
carder.bee<-c(8,27,6,32,23)

bees<-cbind(buff.tail,garden.bee,red.tail,honey.bee,carder.bee)
bees

bees.df<-as.data.frame(bees)
bees.df

write.csv(bees.df,file = "bees",row.names = FALSE)
read.csv("bees")

# Q2
bees.df2<-data.frame(buff.tail,garden.bee,red.tail,honey.bee,carder.bee)
bees.df2


bees.matrix2<-as.matrix(bees.df2)
bees.matrix2

# Thistle, Vipers bugloss, Golden rain, Yellow alfalfa, and Blackberry
plant.names<-c("thistle", "vipers bugloss", "golden rain", "yellow alfalfa", "blackberry")
plant.names

row.names(bees.matrix2)<-plant.names
bees.matrix2

bees.df3<-as.data.frame(bees.matrix2)

# Q3
bees.df3[5,]

bees.df3[c(3,4),]

bees.df3[3,1]
bees.df3$buff.tail[3]

bees.df3[,3]
bees.df3$red.tail

bees.df3[,c(1,2,4,5)]
bees.df3[,-3]

jj<-bees.df3[,4]>=10
bees.df3[jj,]
ii<-bees.df3$honey.bee>=10
bees.df3[ii,]

jj<-bees.df3[,c(1,4)]>=10
bees.df3[jj,]
ii<-bees.df3$honey.bee>=10 & bees.df3$buff.tail>=10
bees.df3[ii,]

# Q4
card.deck<-read.csv("deck")

#function to shuffle deck
shuffle<-function(d){
  row.order<-sample(1:52,52)
  return(d[row.order,])
}

deal.5<-function(d){
  return(d[c(1:5),])
}
card.deck<-shuffle(card.deck)
deal.5(card.deck)
card.deck<-shuffle(card.deck)
deal.5(card.deck)
card.deck<-shuffle(card.deck)
deal.5(card.deck)


#deal 5 cards to each 3 players. 1 card at a time
deal3p.5<-function(d){
  temp.deck<-d[,c(1,2)]
  i=0
  while (i<5){
    if (i==0){
    p1<-temp.deck[1,]
    temp.deck<-temp.deck[-1,]
    p2<-temp.deck[1,]
    temp.deck<-temp.deck[-1,]
    p3<-temp.deck[1,]
    temp.deck<-temp.deck[-1,]
    }
    else{
      p1<-rbind(p1,temp.deck[1,])
      temp.deck<-temp.deck[-1,]
      p2<-rbind(p2,temp.deck[1,])
      temp.deck<-temp.deck[-1,]
      p3<-rbind(p3,temp.deck[1,])
      temp.deck<-temp.deck[-1,]
    }
    i=i+1
  }
  l<-list(p1,p2,p3)
  names(l)=c("Player 1 Hand","Player 2 Hand","Player 3 Hand")
  return(l)
}
deal3p.5(card.deck)
card.deck<-shuffle(card.deck)
