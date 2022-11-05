# Project 2: Playing Cards

# part one: build deck
#build card face, suit, value
v<-(13:1)
f<-c('king','queen','jack','ten','nine','eight','seven','six','five','four','three','two','one')
s.heart<-rep('heart',13)
s.diamond<-rep('diamond',13)
s.spade<-rep('spade',13)
s.club<-rep('club',13)

#build each column for data frame w/ repetition function
card.face<-rep(f,4)
card.suit<-c(s.heart,s.diamond,s.spade,s.club)
card.value<-rep(v,4)

#build the data frame for the deck of cards
deck<-data.frame(card.face,card.suit,card.value)

# save our created deck to our computer
#write.table(deck,file = "deck", sep = ",", row.names = FALSE)
#load a prewritten file for deck
#card.deck<-read.table(file=file.choose(), sep = ',', header=TRUE)

#function to shuffle deck
shuffle<-function(d){
  row.order<-sample(1:52,52)
  return(d[row.order,])
}

### GAME - WAR ###
# aces = 14 points
# all other values are ok
deck.war<-deck
deck.war[deck.war$card.face == "ace",]<-14

### Game - Hearts ###
# card value = 0
# hearts = 1
# queen of spades = 13
deck.hearts<-deck
#change all values to 0
deck.hearts$card.value<-0
#make hearts = 1
ii<-deck.hearts$card.suit== "heart"
deck.hearts[ii,3]<-1
deck.hearts
#make queen of spades = 13
jj<- deck.hearts$card.suit == "spade" & deck.hearts$card.face == "queen"
deck.hearts[jj,3]<-13

### Game - Black Jack ###
# king queen and jack = 10
# num cards  = their value
# ace = 1 or 11
deck.BlackJack<-card.deck
deck.BlackJack<-shuffle(d=deck.BlackJack)

#set king queen and jack to 10
ii<-deck.BlackJack$card.face=="king"|deck.BlackJack$card.face=="queen"|deck.BlackJack$card.face=="jack"
deck.BlackJack[ii,3]<-10
#or you can use
deck.BlackJack$card.face %in% c("king","queen","jack")
#set ace to NA
deck.BlackJack[deck.BlackJack$card.face=="one",3]<-NA


# new deal and shuffles
card.deck<-read.table(file="deck", sep = ',', header=TRUE)
#imput deck, top row card, return first row
deal<-function(d){
  card<-d[1,]
  assign('deck', d[-1,],envir=globalenv())
  return(card)
}

deal(d=card.deck)
card.deck
deck                      

shuffle<-function(d){
  row.order<-sample(1:52,52)
  assign("card.deck",d[row.order,],envir=globalenv())
}
shuffle(d=card.deck)
card.deck

deal(d=card.deck)
deal(d=card.deck)
deal(d=card.deck)
