# Project 2: Playing Cards - scratch work

#practicing typeof and coercion of vectors and vector types
d<-c(1,2,3,4,5,6)
typeof(d)

char.vector<-c('hello','world')
char.vector
typeof(char.vector)

char.vector<-c('hello','world',1)
char.vector
typeof(char.vector)

char.vector2<-c('1','2','5')
char.vector2
typeof(char.vector2)

num.vec<-as.numeric(char.vector2)
typeof(num.vec)

logical.vec<-c(FALSE,FALSE,TRUE)
logival.vec2<-c(TRUE,FALSE,TRUE,3,5)

logical.vec
logival.vec2





#matrix practice
d<-c(1:6)
# nrow=2
matrix(d,nrow=2)
# ncol=2
matrix(d,ncol=2)

#add byrow=TRUE
#changes the way that numbers are put into matrix
matrix(d,nrow=2,byrow=TRUE)

#new vector of len 7
d7<-c(d,7)
d7
#Error: data length not suitable so it repeats to fill
matrix(d7,nrow=2)

#poker hand containing royal flush of spades
#royal flush = ace, king, queen, jack, ten
#suit and face in different columns
card.face<-c('ace','king','queen','jack','ten')
card.suit<-c('spade','spade','spade','spade','spade')
#cbind binds the vectors into separate columns
rflush.spade<-cbind(card.face,card.suit)
rflush.spade




#dataframes
#two new vectors
p<-c(1,2,3,4,5)
l<-c(F,T,F,T,T)
#build data frame
df<-data.frame(card.face,card.suit,p,l)
#structure, gives types of vectors
str(df)

#class gives type of data (matrix,dataframe,array,etc)
class(df)
class(rflush.spade)

#convert between matrix and dataframe
as.matrix(df)
as.data.frame(df)


#build deck of cards
# part one: build deck
#build card face, suit, value
v<-(13:1)
f<-c('king','queen','jack','ten','nine','eight','seven','six','five','four','three','two','ace')
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
deck


#load a prewritten file for deck
card.deck<-read.table(file=file.choose(), sep = ',', header=TRUE)
# or read csv
card.deck1read.csv(file=file.choose(), sep = ',', header=TRUE)
# or straight from github
card.deck2<-read.csv(file="https://gist.githubusercontent.com/garrettgman/9629323/raw/ee5dfc039fd581cb467cc69c226ea2524913c3d8/deck.csv")

#ways to view
view(card.deck)
head(card.deck)
tail(card.deck)

#check class amd str
class(card.deck)
str(card.deck)

#gets the working directory
getwd()

# save our created deck to our computer
write.table(deck,file = "deck", sep = ",", row.names = FALSE)

#selecting values from objects, indexing begins at 1
#vectors
practice<-c(1,2,3,6,7,4,5,9,0)
practice[4] #6
practice[8] #9
length(practice) #9
practice[c(3,5)] #3 7
practice[-5] #prints everything but item in 5th position
practice[-c(3,5)] #all but 3 and 7
i<-practice>2
practice[i] #all but 1,2,0
j<-(practice<2 | practice>6) #less than 2 and greater than 6
practice[j]
#Matix and Data Frame: [row, column]
deck[2,3]
deck[2,]
deck[,3]

#dollar sign notation for data frames
#pulls columns
deck$card.face

#pull first row of deck
deck[1,]

#function to deal top card of deck
#d is deck used
deal<-function(d){
  return(d[1,])
}
deal(d=deck)

#shuffle deck
deck[1:52,]
deck[c(2,1,3:52),]

row.order<-sample(1:52,52)
new<-deck[row.order,]

deal(d=new)

#function to shuffle deck
shuffle<-function(d){
  row.order<-sample(1:52,52)
  return(d[row.order,])
}
new<-shuffle(d=deck)
deal(d=new)


card.deck[1,3]
card.deck[1,]

vec<-rep(1:3,4)
vec[4]
vec[4]<-8
vec[c(4,5,6)]
vec[c(4,5,6)]<-10
vec[c(1,2,3)]<-c(10,20,30)
#add extra value
vec[13]<-55

### GAME - WAR ###
# aces = 14 points
# all other values are ok
deck.war<-deck
#change value of ace to 14
deck.war[c(13,26,39,52),3]<-14

#function to shuffle deck
shuffle<-function(d){
  row.order<-sample(1:52,52)
  return(d[row.order,])
}

deck.war<-shuffle(d=deck.war)
# now find aces
#extract first column (dollar sign or bracket notation)
deck.war$card.face == "ace"
deck.war[deck.war$card.face == "ace",]
deck.war[deck.war$card.face == "ace",]<-14

ii<-deck.war$card.face == 'ace'
deck.war[ii,]
deck.war[ii,3]<-14

### Game - Hearts ###
# card value = 0
# hearts = 1
# queen of spades = 13
deck.hearts<-deck

#can use both bracket and dollar sign notation
#change all values to 0
deck.hearts[1:52,3]<-0
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
deck.BlackJack<-deck
deck.BlackJack<-shuffle(d=deck.BlackJack)

#set king queen and jack to 10
ii<-deck.BlackJack$card.face=="king"|deck.BlackJack$card.face=="queen"|deck.BlackJack$card.face=="jack"
deck.BlackJack[ii,3]<-10
#or you can use
deck.BlackJack$card.face %in% c("king","queen","jack")
#set ace to NA
deck.BlackJack[deck.BlackJack$card.face=="ace",3]<-NA



#NA work
vec<-c(12:25,NA)
vec
mean(vec) #returns NA
mean(vec,na.rm = TRUE) #removes NA for computation
length(vec) #15
length(vec,na.rm = TRUE) #does not work
length(na.omit(vec)) #14
is.na(vec) #
sum(is.na(vec)) #1 Na
which(is.na(vec)) #it is in 15th position

help(apply)
m<-c(1:6)
matrix1<-
apply(matrix1,2,is.na,)


# 2-1-2022
#save object in environment we specify
assign('deck', deck[-1,],envir=globalenv())

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
