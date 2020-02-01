install.packages("rvest")
install.packages("XML")
install.packages("magrittr")
library(rvest)
library(XML)
library(magrittr)

amazon_url <- "https://www.amazon.in/OnePlus-Glacier-Display-Storage-3800mAH/product-reviews/B07DJHXTLJ/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber"

amazon_review <- NULL

for (i in 1:20){
  murl <- read_html(as.character(paste(amazon_url,i,sep = "=")))
  reve <- murl %>% html_nodes(".review-text") %>% html_text()
  amazon_review <- c(amazon_review,reve)
}

write.table(amazon_review,"oneplus7T.txt")
getwd()

txt <- amazon_review
str(txt)

install.packages("tm")
library(tm)

x <- Corpus(VectorSource(txt))
x <- tm_map(x, function(x) iconv(enc2utf8(x), sub = 'byte'))

# Data Cleansing

x1 <- tm_map(x, tolower)
x1 <- tm_map(x1, removePunctuation)
x1 <- tm_map(x1, removeNumbers)
x1 <- tm_map(x1, removeWords, stopwords('english'))
x1 <- tm_map(x1, stripWhitespace)

# Term Document Matrix
tdm <- TermDocumentMatrix(x1)
tdm
#<<TermDocumentMatrix (terms: 3585, documents: 200)>>
#Non-/sparse entries: 11513/705487
#Sparsity           : 98%

dtm <- t(tdm)
tdm <- as.matrix(tdm)
dim(tdm)

#Barplot
e <- rowSums(tdm)
e

e_sub <- subset(e, e>=30)

barplot(e_sub, las= 2, col = rainbow(30))
x1 <- tm_map(x1, removeWords, c('day','dont','one','phone','like','pro','just','will','now','still'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

dtm <- t(tdm)
tdm <- as.matrix(tdm)
dim(tdm)

e <- rowSums(tdm)
e

e_sub <- subset(e, e>=35)

barplot(e_sub, las= 2, col = rainbow(30))

e_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
install.packages("wordcloud")
library(wordcloud)

wordcloud(words= names(e_sub), freq = e_sub)

#Better visualization

windows()
wordcloud(words= names(e_sub1), freq = e_sub1, random.order = F, colors = rainbow(30),scale = c(2,0.5), rot.per = 0.4)

#Wordcloud2

install.packages("wordcloud2")
library(wordcloud2)

e1 <- data.frame(names(e_sub),e_sub)
colnames(e1) <- c('word','freq')

wordcloud2(e1,size = 0.5,shape = 'circle')

