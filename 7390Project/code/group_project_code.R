## James individual account
library (rtweet)

twitter_token <- create_token(
  consumer_key = 'AujjlqjuMD0R7fQzbreBvpmf5',
  consumer_secret = '7I1JHKilh8FP04CShuDuMp2YoyImFWj8pvqxgJsUtySGgYAInx',
  access_token = '1330488273374900224-5E2MMVZkXyBO8G6ajLdW6JSUjqQmof',
  access_secret = 'FaMYGlJsj0czW199UoQrxQTyzlHfE9rv44UTqnyKmT9Ln',
  set_renv = TRUE)

James_tweets <- get_timeline("@KingJames", n= 10000, lang ="en")

James_tweets_organic <- James_tweets[James_tweets$is_retweet==FALSE, ] 
James_tweets_organic <- subset(James_tweets_organic, is.na(James_tweets_organic$reply_to_status_id)) 
James_tweets_organic

#dataframe rowdata
James_tweets_organic.rawdata <- data.frame(James_tweets_organic$user_id, James_tweets_organic$screen_name, James_tweets_organic$created_at, James_tweets_organic$text)
write.csv(James_tweets_organic.rawdata, '../data/James_tweets_organic.rawdata.csv', row.names = FALSE)

#install.packages("twitteR")
library("twitteR")
library("dplyr")
James_tweets_organic <- James_tweets_organic %>% arrange(-favorite_count)
James_tweets_organic <- James_tweets_organic %>% arrange(-retweet_count)


James_retweets <- James_tweets[James_tweets$is_retweet==TRUE,]
James_retweets#181

James_replies <- subset(James_tweets, !is.na(James_tweets$reply_to_status_id))
James_replies#137

data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2245, 749, 244)
)

library(forestmangr)
library(ggplot2)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))

data <- round_df(data, 2)
Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


colnames(James_tweets)[colnames(James_tweets)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(James_tweets, Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from KingJames",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


James_app <- James_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
James_app <- subset(James_app, count > 11)

data <- data.frame(
  category=James_app$source,
  count=James_app$count
)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
data <- round_df(data, 2)
Source <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


#install.packages("dplyr")
library(dplyr)
#install.packages("tidytext")
library(tidytext)
James_tweets_organic$text <-  gsub("https\\S*", "", James_tweets_organic$text)
James_tweets_organic$text <-  gsub("@\\S*", "", James_tweets_organic$text) 
James_tweets_organic$text  <-  gsub("amp", "", James_tweets_organic$text) 
James_tweets_organic$text  <-  gsub("[\r\n]", "", James_tweets_organic$text)
James_tweets_organic$text  <-  gsub("[[:punct:]]", "", James_tweets_organic$text)

tweets <- James_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>%anti_join(stop_words)

tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of KingJames",
       subtitle = "Stop words removed from the list")


#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)
#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#wordcloud james tweet hashtags.
James_tweets_organic$hashtags <- as.character(James_tweets_organic$hashtags)
James_tweets_organic$hashtags <- gsub("c\\(", "", James_tweets_organic$hashtags)

set.seed(1234)
wordcloud(James_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


set.seed(1234)
wordcloud(James_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))


#install.packages("syuzhet")
library(syuzhet)


tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")

tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)

tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()





## NBA official account
library (rtweet)
library(forestmangr)
library(ggplot2)
library("dplyr")


twitter_token <- create_token(
  consumer_key = 'AujjlqjuMD0R7fQzbreBvpmf5',
  consumer_secret = '7I1JHKilh8FP04CShuDuMp2YoyImFWj8pvqxgJsUtySGgYAInx',
  access_token = '1330488273374900224-5E2MMVZkXyBO8G6ajLdW6JSUjqQmof',
  access_secret = 'FaMYGlJsj0czW199UoQrxQTyzlHfE9rv44UTqnyKmT9Ln',
  set_renv = TRUE)


NBA_tweets <- search_tweets("#NBA", n = 10000, include_rts = FALSE, lang = "en")

NBA_topic.rawdata <- data.frame(NBA_tweets$user_id, NBA_tweets$screen_name, NBA_tweets$created_at, NBA_tweets$text)
write.csv(NBA_topic.rawdata, '../data/NBA_topic.rawdata.csv', row.names = FALSE)
  
NBA_tweets_organic <- NBA_tweets[NBA_tweets$is_retweet==FALSE, ] 
NBA_tweets_organic <- subset(NBA_tweets_organic, is.na(NBA_tweets_organic$reply_to_status_id)) 
NBA_tweets_organic


library("dplyr")
NBA_tweets_organic <- NBA_tweets_organic %>% arrange(-favorite_count)
NBA_tweets_organic <- NBA_tweets_organic %>% arrange(-retweet_count)


NBA_retweets <- NBA_tweets[NBA_tweets$is_retweet==TRUE,]
NBA_retweets#181

NBA_replies <- subset(NBA_tweets, !is.na(NBA_tweets$reply_to_status_id))
NBA_replies#137

data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2245, 0, 538)
)

library(forestmangr)
library(ggplot2)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))

data <- round_df(data, 2)
Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


NBA_app <- NBA_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
NBA_app <- subset(NBA_app, count > 11)

data <- data.frame(
  category=NBA_app$source,
  count=NBA_app$count
)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
data <- round_df(data, 2)
Source <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")



ts_plot(NBA_tweets, by="days") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from NBA",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


library(dplyr)
library(tidytext)
NBA_tweets$text <-  gsub("https\\S*", "", NBA_tweets$text)
NBA_tweets$text <-  gsub("@\\S*", "", NBA_tweets$text) 
NBA_tweets$text  <-  gsub("amp", "", NBA_tweets$text) 
NBA_tweets$text  <-  gsub("[\r\n]", "", NBA_tweets$text)
NBA_tweets$text  <-  gsub("[[:punct:]]", "", NBA_tweets$text)

tweets <- NBA_tweets %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>%anti_join(stop_words)

tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of NBA",
       subtitle = "Stop words removed from the list")



library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

mt.v <- VectorSource(NBA_tweets$text)
NBA.c <- SimpleCorpus(mt.v)
inspect(NBA.c)

NBA.c.p <- tm_map(NBA.c, content_transformer(tolower))
NBA.c.p <- tm_map(NBA.c.p, removeNumbers)
NBA.c.p <- tm_map(NBA.c.p, removeWords, stopwords("english"))
NBA.c.p <- tm_map(NBA.c.p, removePunctuation)
NBA.c.p <- tm_map(NBA.c.p, stripWhitespace)

inspect(NBA.c.p)

dtm <- TermDocumentMatrix(NBA.c.p)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(v), freq = v)
head(d, 10)
set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

library(wordcloud2)
wordcloud2(data=df, size=3, color='random-dark')



library(syuzhet)
tweets <- iconv(NBA_tweets, from="UTF-8", to="ASCII", sub="")

tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)

tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()





## NBA difference country
NBA <- get_timelines(c("NBA","NBA_de","NBACanada","NBAFRANCE","NBAUK","NBA_AU","NBA_Africa","NBAIndia","NBAItalia"), n = 3200)
NBA_Countries.rawdata <- data.frame(NBA$user_id, NBA$screen_name, NBA$created_at, NBA$text)
write.csv(NBA_Countries.rawdata, '../data/NBA_Countries.rawdata.csv', row.names = FALSE)

NBA$text <-  gsub("https\\S*", "", NBA$text)
NBA$text <-  gsub("@\\S*", "", NBA$text) 
NBA$text  <-  gsub("amp", "", NBA$text) 
NBA$text  <-  gsub("[\r\n]", "", NBA$text)
NBA$text  <-  gsub("[[:punct:]]", "", NBA$text)

tweets <- NBA %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>%anti_join(stop_words)

tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of official NBA accounts in different countries",
       subtitle = "Stop words removed from the list")





NBA %>%
  dplyr::filter(created_at > "2019-9-1") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("day",trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by official NBA accounts in different countries",
    subtitle = "Twitter status(tweet)counts aggregated by day from Sep 2019",
    caption = "\nSource: Date collected from Twitter's REST API via rtweet"
  )



library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

v <- VectorSource(NBA$text)
NBA.c <- SimpleCorpus(mt.v)
inspect(NBA.c)

NBA.c.p <- tm_map(NBA.c, content_transformer(tolower))
NBA.c.p <- tm_map(NBA.c.p, removeNumbers)
NBA.c.p <- tm_map(NBA.c.p, removeWords, stopwords("english"))
NBA.c.p <- tm_map(NBA.c.p, removePunctuation)
NBA.c.p <- tm_map(NBA.c.p, stripWhitespace)

inspect(NBA.c.p)
dtm <- TermDocumentMatrix(NBA.c.p)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(v), freq = v)
head(d, 10)
set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

library(wordcloud2)
wordcloud2(data=df, size=3, color='random-dark')





