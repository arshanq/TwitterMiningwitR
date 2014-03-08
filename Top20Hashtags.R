library(tm)
library(ggplot2)
library(twitteR)
library(wordcloud)
library(stringr)
#tweets<-searchTwitter('delhi elections 2013',n=100,lang="en")
#tweets10DEC->'delhi elections 2013'
load('tweets10DEC.Rdata')

results<-sapply(tweets, function(x) x$getText())


results=str_extract_all(results,"#\\w+")

result_tags=unlist(results)

result_tag_freq=table(result_tags)

tags= sort(result_tag_freq)

top20=tail(tags,20)
wordcloud(names(result_tag_freq), result_tag_freq, random.order=FALSE, min.freq=1, 
          colors=rep("#1B9E77",length(result_tag_freq)), ordered.colors=TRUE)

barplot(top20, border=NA, las=2, main="Top 20 most frequent hashtags", cex.main=1)
