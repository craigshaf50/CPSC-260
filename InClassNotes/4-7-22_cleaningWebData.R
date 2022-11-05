library(tidyverse)
library(rvest)

webpage<-read_html("http://books.toscrape.com/")

#clean up price
price<-webpage %>% html_nodes("p.price_color") %>% html_text()
price
#get rid of currency symbol
#gsub("take this", "replace it with this", data)
#and make data numeric
price.2<-gsub("\\£","",price) %>% as.numeric()
mean(price.2)

#clean up rating
rating<-webpage %>% html_nodes("p.star-rating") %>% html_attr("class")
rating

rating.2<-gsub("star-rating ","",rating)

#espn super bowl winners
espn<-read_html("http://www.espn.com/nfl/superbowl/history/winners#:~:text=Super%20Bowl%20Winners%20and%20Results%20%20%20NO.,23%2C%20Cincinnati%2020%20%2022%20more%20rows%20")
tb<-espn %>% html_nodes("table") %>% html_table()

sb.tb<-tb[[1]]
class(sb.tb)

table.col.names<-sb.tb[2,]
colnames(sb.tb)<-table.col.names
sb.tb

sb.tb2<-sb.tb[-c(1,2),]
sb.tb2

#separate(dataset, column, c("new column names","name"), sep=)
sb.tb3<-separate(sb.tb2, RESULT, c("Winner","Loser"), sep=",")

#str_extract(data, pattern)
#to extract number: "\\d+"
W.score<-str_extract(sb.tb3$Winner, "\\d+") %>% as.numeric()
L.score<-str_extract(sb.tb3$Loser, "\\d+") %>% as.numeric()

sb.tb4<-data.frame(sb.tb3,W.score,L.score)
sb.tb4

#use gsub to get rid of scores by team names
sb.tb4$Winner<-gsub("\\d+","",sb.tb4$Winner)
sb.tb4$Loser<-gsub("\\d+","",sb.tb4$Loser)
sb.tb4

#chap 16 Lubridate Package
#chap 14 string patterns

#chap 19 functions