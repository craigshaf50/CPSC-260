install.packages("rvest")
library(rvest)

myView.page<-read_html("https://www.grandview.edu/about/community/news/2022-commencement-speakers")
#get title
nodes.h1<-html_nodes(myView.page,"h1")
nodes.h1<-myView.page %>% html_nodes("h1")
nodes.h1

title<-html_text(nodes.h1)
title<-myView.page %>% html_nodes("h1") %>% html_text()
title

#now try to get date
myView.page %>% html_nodes("span") %>% html_text()
#need to include class to find it properly
#include period between tag and attribute
myView.page %>% html_nodes("span.publish-date") %>% html_text()
#can also just use attribute
myView.page %>% html_nodes(".publish-date") %>% html_text()


#scrape ny times best seller heading - website wont let scrape
BN.page<-read_html("https://www.barnesandnoble.com/b/the-new-york-times-bestsellers/_/N-1p3n")
heading<-BN.page %>% html_nodes("heading") %>% html_nodes("h1") %>% html_nodes("span.text") %>% html_text()

#IMBD
imbd.page<-read_html("https://www.imdb.com/what-to-watch/fan-favorites/?ref_=hm_fanfav_sm")
#header
imbd.page %>% html_nodes("h1.ipc-title__text") %>% html_text()
#scrape ratings
imbd.page.2<-read_html("https://www.imdb.com/list/ls093785287/")
imbd.page.2 %>% html_nodes("a") %>% html_text()
#rating
imbd.page.2 %>% html_nodes(".ipl-rating-star__rating") %>% html_text()
#metascore
imbd.page.2 %>% html_nodes(".metascore.favorable") %>% html_text()
#runtime
imbd.page.2 %>% html_nodes(".runtime") %>% html_text()
#rated ( like pg13, R, etc.)
imbd.page.2 %>% html_nodes(".certificate") %>% html_text()
#genre
imbd.page.2 %>% html_nodes(".genre") %>% html_text()

#largest employers
wiki.page<-read_html("https://en.wikipedia.org/wiki/List_of_largest_employers#:~:text=Largest%20employers%20%20%20%20Employer%20%20,%20see%20initial%20%2013%20more%20rows%20")
tables<-wiki.page %>% html_nodes("table") %>% html_table
class(tables[[1]])
tables[[1]]

#