# required pakacges
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(RTextTools)
library(e1071)

#--twitter authentication
options(httr_oauth_cache=T)
api_key <- "REDACTED"
api_secret <- "REDACTED"
access_token <- "REDACTED"
access_token_secret <- "REDACTED"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#-uses the twitter api and twitteR to search the last n number of tweets
#mach_tweets = searchTwitter("climate+change", n=500, lang="en", resultType = "recent")
seattle_mach_tweets = searchTwitter("climate+change", n=500, lang="en", geocode='46.732,-117.001, 100mi', resultType = "recent")

mach_tweets = searchTwitter("climate+change", n=500, lang="en", geocode='37.774,-122.431, 100mi', resultType = "recent")

mach_tweets = searchTwitter("climate+change", n=500, lang="en", geocode='47.608,-122.335, 100mi', resultType = "recent")


#--create a matrix using apply of the tweet texts
mach_text = sapply(mach_tweets, function(x) x$getText())
mach_text <- iconv(mach_text,to="utf-8")
# create a corpus
mach_corpus = Corpus(VectorSource(mach_text))
# create document term matrix applying some transformations
control <- list(
  removePunctuation = TRUE,
  stopwords = c("climate", "change", "http", "https", stopwords("english")),
  removeNumbers = TRUE,
  tolower = TRUE)

tdm <- TermDocumentMatrix(mach_corpus, control = control)
# define tdm as matrix
m = as.matrix(tdm)
mat <- m
classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )





# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)


dm_sentiments <- join(dm, sentiments, type = "left")

dm_negative <- subset(dm_sentiments, sentiment == "negative")

dm_positive <- subset(dm_sentiments, sentiment == "positive")


pos <- dm_positive[!duplicated(dm_positive$word), ]

neg <- dm_negative[!duplicated(dm_negative$word), ]
posneg <- rbind(pos, neg)
posneg <- posneg[,1:3]
pn1 <- rep(posneg$word, posneg$freq)
pn2 <- rep(posneg$sentiment, posneg$freq)

pn3 <- cbind(as.character(pn1), pn2)

pn3 <- as.data.frame(pn3) 
colnames(pn3) <- c("word", "sentiment")

pn4 <- pn3[!duplicated(pn3$word), ]
pn4 <- with(pn4,  pn4[order(word) , ])

## 75% of the sample size
smp_size <- floor(0.75 * nrow(pn4))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(pn4)), size = smp_size)

train <- pn4[train_ind, ]
test <- pn4[-train_ind, ]




classifier = naiveBayes(train, as.factor(as.factor(train[,2]) ))



# test the validity
predicted = predict(classifier, test); predicted
table(test[, 2], predicted)
recall_accuracy(test[, 2], predicted)





posneg <- as.matrix(posneg)



#all <- dm_sentiments[!duplicated(dm_sentiments$word), ]
all2 <- subset(dm_sentiments, sentiment != "<NA>")
all3 <- all2[,1:3]

vec <- rep(all3$word, all3$freq)
vec2 <- rep(all3$sentiment, all3$freq)

vec3 <- cbind(as.character(vec), vec2)

classifier = naiveBayes(vec3, as.factor(as.factor(vec3[,2]) ))


# test the validity
predicted = predict(classifier, mat[11:15,]); predicted
table(tweets[11:15, 2], predicted)
recall_accuracy(tweets[11:15, 2], predicted)





as.factor(posneg[,3]) 
classifier = naiveBayes(vec3, as.factor(as.factor(vec3[,2]) ))
