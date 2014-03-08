library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)
# function score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
   

   # create simple array of scores with laply
   scores = laply(sentences,
   function(sentence, pos.words, neg.words)
   {
      # remove punctuation
      sentence = gsub("[[:punct:]]", "", sentence)
      # remove control characters
      sentence = gsub("[[:cntrl:]]", "", sentence)
      # remove digits?
      sentence = gsub('\\d+', '', sentence)

      # define error handling function when trying tolower
      tryTolower = function(x)
      {
         # create missing value
         y = NA
         # tryCatch error
         try_error = tryCatch(tolower(x), error=function(e) e)
         # if not an error
         if (!inherits(try_error, "error"))
         y = tolower(x)
         # result
         return(y)
      }
      # use tryTolower with sapply 
      sentence = sapply(sentence, tryTolower)

      # split sentence into words with str_split (stringr package)
      word.list = str_split(sentence, "\\s+")
      words = unlist(word.list)

      # compare words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)

      # get the position of the matched term or NA
      # we just want a TRUE/FALSE
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)

      # final score
      score = sum(pos.matches) - sum(neg.matches)
      return(score)
      }, pos.words, neg.words, .progress=.progress )

   # data frame with scores for each sentence
   scores.df = data.frame(text=sentences, score=scores)
   return(scores.df)
}

load('AAP500.Rdata')
load('BJP500.Rdata')
load('Congress500.Rdata')

#aap_tweets = searchTwitter("AAP", n=1000, lang="en",locale="Delhi")
#bjp_tweets = searchTwitter("BJP", n=1000, lang="en", locale= "Delhi")
#cong_tweets = searchTwitter("Congress", n=1000, lang="en", locale= "Delhi")

aap_txt = sapply(aap_tweets, function(x) x$getText())
bjp_txt = sapply(bjp_tweets, function(x) x$getText())
cong_txt = sapply(congress_tweets, function(x) x$getText())

nd = c(length(aap_txt), length(bjp_txt), length(cong_txt))

all_parties = c(aap_txt, bjp_txt, cong_txt) 

pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")

scores = score.sentiment(all_parties, pos, neg, .progress='text')

scores$party_name = factor(rep(c("AAP","BJP","Congress"), nd))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

global_score = round( 100 * numpos / (numpos + numneg) )

cols = c("#7CAE00", "#00BFC4", "#F8766D")
names(cols) = c("AAP","BJP","Congress")
#barplot
meanscore = tapply(scores$score, scores$party_name, mean)
df = data.frame(party_name=names(meanscore), meanscore=meanscore)
df$all_parties <- reorder(df$party_name, df$meanscore)

ggplot(df, aes(y=meanscore)) +
geom_bar(data=df, aes(x=party_name, fill=party_name)) +
scale_fill_manual(values=cols[order(df$meanscore)]) +
labs(element_text = "Average Sentiment Score",
    legend.position = "none")

	
