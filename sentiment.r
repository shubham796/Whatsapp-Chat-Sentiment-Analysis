install.packages("ggplot2")
install.packages("tm")
install.packages("wordcloud")
install.packages("syuzhet")
install.packages("SnowballC")

library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)
library(SnowballC)

getwd()

setwd("F:/shub data")
texts=readLines("wcsi.txt") #CHANGE THE TEXT FILE
docs=Corpus(VectorSource(texts))
trans=content_transformer(function(x,pattern)gsub(pattern,"",x))
docs=tm_map(docs,trans,"/")
docs=tm_map(docs,trans,"@")
docs=tm_map(docs,trans,"\\|")
docs=tm_map(docs,content_transformer(tolower))
docs=tm_map(docs, removeWords, c("shubham"))
docs=tm_map(docs, removeWords, c("simar"))
docs=tm_map(docs,removeNumbers)
docs=tm_map(docs,removeWords,stopwords("english"))
docs=tm_map(docs,removePunctuation)
docs=tm_map(docs,stripWhitespace)
docs=tm_map(docs,stemDocument)

dtm=TermDocumentMatrix(docs)
memory.limit()
mat=as.matrix(dtm)
mat
v=sort(rowSums(mat),decreasing=TRUE)
d=data.frame(word=names(v),freq=v)
head(d)
wordcloud(words=d$word,freq=d$freq,min=1,max.words=300,random.order=F,rot.per=0.35,colors=brewer.pal(8,"Dark2"))
#### To make the wordcloud
getwd()

sentiment=get_nrc_sentiment(texts)
text=cbind(texts,sentiment)
print(text)
table(text)

TotalSentiment=data.frame(colSums(text[,c(2:11)]))
TotalSentiment

names(TotalSentiment)="count"

TotalSentiment=cbind("sentiment"=rownames(TotalSentiment),TotalSentiment)

print(TotalSentiment)
ggplot(data=TotalSentiment,aes(x=sentiment,y= count))+geom_bar(aes(fill=sentiment),stat="identity")+
  theme(legend.position="none")+xlab("sentiment")+ylab("Total Count")+ggtitle("Total Sentiment Score")

