# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("RCurl")
install.packages("XML")
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(tidyverse)
# first get df with different categories
# ch
tf_idf_ch <- tf_idf_desc %>%
  filter(category == 'chemistry')
# cs
tf_idf_cs <- tf_idf_desc %>%
  filter(category == 'computer science')
# bi
tf_idf_bi <- tf_idf_desc %>%
  filter(category == 'biology')
# da
tf_idf_da <- tf_idf_desc %>%
  filter(category == 'data science')
# ec
tf_idf_ec <- tf_idf_desc %>%
  filter(category == 'economics')
# en
tf_idf_en <- tf_idf_desc %>%
  filter(category == 'engineering')

set.seed(1235)
#wordcloud(words = tf_idf_ch$word, freq = tf_idf_ch$tf_idf, min.freq = 0.00001,scale=c(3,.3),
          #max.words=300, random.order=FALSE, rot.per=0.30, 
          #colors=brewer.pal(8, "Dark2"))
# plot ch
pal2 <- brewer.pal(8,"Dark2")
png("ch.png", width=1280,height=800)
wordcloud(words = tf_idf_ch$word, freq = tf_idf_ch$tf_idf, scale=c(8,.2),min.freq=0.00001,
          max.words=280, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
# plot cs
pal2 <- brewer.pal(8,"Dark2")
png("cs.png", width=1280,height=800)
wordcloud(words = tf_idf_cs$word, freq = tf_idf_cs$tf_idf, scale=c(8,.2),min.freq=0.00001,
          max.words=280, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
# plot bi
pal2 <- brewer.pal(8,"Dark2")
png("bi.png", width=1280,height=800)
wordcloud(words = tf_idf_bi$word, freq = tf_idf_bi$tf_idf, scale=c(8,.2),min.freq=0.00001,
          max.words=280, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
# plot da
pal2 <- brewer.pal(8,"Dark2")
png("da.png", width=1280,height=800)
wordcloud(words = tf_idf_da$word, freq = tf_idf_da$tf_idf, scale=c(8,.2),min.freq=0.00001,
          max.words=280, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
# plot ec
pal2 <- brewer.pal(8,"Dark2")
png("ec.png", width=1280,height=800)
wordcloud(words = tf_idf_ec$word, freq = tf_idf_ec$tf_idf, scale=c(8,.2),min.freq=0.00001,
          max.words=280, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
# plot en
pal2 <- brewer.pal(8,"Dark2")
png("en.png", width=1280,height=800)
wordcloud(words = tf_idf_en$word, freq = tf_idf_en$tf_idf, scale=c(8,.2),min.freq=0.00001,
          max.words=280, random.order=FALSE, rot.per=.17, colors=pal2)
dev.off()
