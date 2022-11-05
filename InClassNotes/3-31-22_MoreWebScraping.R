library(tidyverse)
library(rvest)

webpage<-read_html("http://books.toscrape.com/")

#goal: get title
webpage %>% html_nodes("a") %>% html_text()
webpage %>% html_nodes("h3") %>% html_text()
#looks for a tags in h3 tags
webpage %>% html_nodes("h3 a") %>% html_text()
#gets the title attribute from a tags in h3 tags
webpage %>% html_nodes("h3 a") %>% html_attr("title")

#goal: get price
price<-webpage %>% html_nodes("p.price_color") %>% html_text()
price
#goal: get in stock
webpage %>% html_nodes("p.instock.availability") %>% html_text()
webpage %>% html_nodes("p.instock.availability") %>% html_text(trim=TRUE)

#goal: get rating
webpage %>% html_nodes("p.star-rating") %>% html_attr("class")

#goal: get book covers
imgsrc<-webpage %>% html_nodes("img") %>% html_attr("src")
URL="http://books.toscrape.com/"
download.file(paste0(URL,imgsrc),destfile = basename(imgsrc))

#gsub()
#seperate()
#str_extract()
