# redo
# This is the first Rscript we should run, it aims to parse 6 xml files
# bi, ch, cs, da, ec, en
# then just get the posts column of each, do processing
# including lowercase, remove blank, remove number, lemmatization
# then go to tf_idf.r

#######################################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/textstem")

library(pacman)
library(tidyverse)
library(XML)
library(methods)
library(dplyr)
library(stringr)
library(qdapRegex)
library(utils)
library(magrittr)
library(tokenizers)
#########
# parse bi
bi <- xmlToList("Posts_bi.xml")
dfres_bi <- do.call(rbind, lapply(lapply(bi, unlist), "[",
                                  unique(unlist(c(sapply(bi,names))))))
# only select those with contents
dfres_bi <- subset(dfres_bi,dfres_bi[,7]!="")
# remove all html tags
dfres_bi[,7] <- gsub("<[^>]+>", "", dfres_bi[,7])
# remove commas and periods and questoin marks
dfres_bi[,7] <- str_replace_all(dfres_bi[,7], "[^[:alnum:]]", " ")
# remove single characters
dfres_bi[,7] <- gsub("\\W*\\b\\w\\b\\W*", " ", dfres_bi[,7])
# remove all extra white space
dfres_bi[,7] <- rm_white(dfres_bi[,7])

# lemmatize the data to better fit the tfidf
col_7_bi <- lemmatize_strings(as.character(dfres_bi[,7]))
# get all column 7 and save as dataframe
col_7_bi <- as.data.frame(col_7_bi)
# confused here, but still there are NAs so remove them
col_7_bi <- filter(col_7_bi, col_7_bi != 'NA')
# change them all to lower cases
col_7_bi <- col_7_bi %>% 
  mutate(col_7_bi = tolower(col_7_bi))
# save them in case of lost
save(col_7_bi, file = "col_7_bi.RData")

#########
# parse ch
ch <- xmlToList("Posts_ch.xml")
dfres_ch <- do.call(rbind, lapply(lapply(ch, unlist), "[",
                                  unique(unlist(c(sapply(ch,names))))))
# only select those with contents
dfres_ch <- subset(dfres_ch,dfres_ch[,7]!="")
# remove all html tags
dfres_ch[,7] <- gsub("<[^>]+>", "", dfres_ch[,7])
# remove commas and periods and questoin marks
dfres_ch[,7] <- str_replace_all(dfres_ch[,7], "[^[:alnum:]]", " ")
# remove single characters
dfres_ch[,7] <- gsub("\\W*\\b\\w\\b\\W*", " ", dfres_ch[,7])
# remove all extra white space
dfres_ch[,7] <- rm_white(dfres_ch[,7])

# lemmatize the data to better fit the tfidf
col_7_ch <- lemmatize_strings(as.character(dfres_ch[,7]))
# get all column 7 and save as dataframe
col_7_ch <- as.data.frame(col_7_ch)
col_7_ch <- filter(col_7_ch, col_7_ch != 'NA')
col_7_ch <- col_7_ch %>% 
  mutate(col_7_ch = tolower(col_7_ch))
save(col_7_ch, file = "col_7_ch.RData")

##########
# parse cs
cs <- xmlToList("Posts_cs.xml")
dfres_cs <- do.call(rbind, lapply(lapply(cs, unlist), "[",
                                  unique(unlist(c(sapply(cs,names))))))
# only select those with contents
dfres_cs <- subset(dfres_cs,dfres_cs[,7]!="")
# remove all html tags
dfres_cs[,7] <- gsub("<[^>]+>", "", dfres_cs[,7])
# remove commas and periods and questoin marks
dfres_cs[,7] <- str_replace_all(dfres_cs[,7], "[^[:alnum:]]", " ")
# remove single characters
dfres_cs[,7] <- gsub("\\W*\\b\\w\\b\\W*", " ", dfres_cs[,7])
# remove all extra white space
dfres_cs[,7] <- rm_white(dfres_cs[,7])

# lemmatize the data to better fit the tfidf
col_7_cs <- lemmatize_strings(as.character(dfres_cs[,7]))
col_7_cs <- as.data.frame(col_7_cs)
col_7_cs <- filter(col_7_cs, col_7_cs != 'NA')
# change all to lowercase
col_7_cs <- col_7_cs %>% 
  mutate(col_7_cs = tolower(col_7_cs))
# get all column 7 and save as dataframe
save(col_7_cs, file = "col_7_cs.RData")

#########
# parse da
da <- xmlToList("Posts_da.xml")
dfres_da <- do.call(rbind, lapply(lapply(da, unlist), "[",
                                  unique(unlist(c(sapply(da,names))))))
# only select those with contents
dfres_da <- subset(dfres_da,dfres_da[,6]!="")
# remove all html tags
dfres_da[,6] <- gsub("<[^>]+>", "", dfres_da[,6])
# remove commas and periods and questoin marks
dfres_da[,6] <- str_replace_all(dfres_da[,6], "[^[:alnum:]]", " ")
# remove single characters
dfres_da[,6] <- gsub("\\W*\\b\\w\\b\\W*", " ", dfres_da[,6])
# remove all extra white space
dfres_da[,6] <- rm_white(dfres_da[,6])

# lemmatize the data to better fit the tfidf
col_6_da <- lemmatize_strings(as.character(dfres_da[,6]))
# get all column 6 and save as dataframe
col_6_da <- as.data.frame(col_6_da)
col_6_da <- filter(col_6_da, col_6_da != 'NA')
# change all to lowercase
col_6_da <- col_6_da %>% 
  mutate(col_6_da = tolower(col_6_da))
save(col_6_da, file = "col_6_da.RData")

#########
# parse ec
ec <- xmlToList("Posts_ec.xml")
dfres_ec <- do.call(rbind, lapply(lapply(ec, unlist), "[",
                                  unique(unlist(c(sapply(ec,names))))))
# only select those with contents
dfres_ec <- subset(dfres_ec,dfres_ec[,6]!="")
# remove all html tags
dfres_ec[,6] <- gsub("<[^>]+>", "", dfres_ec[,6])
# remove commas and periods and questoin marks
dfres_ec[,6] <- str_replace_all(dfres_ec[,6], "[^[:alnum:]]", " ")
# remove single characters
dfres_ec[,6] <- gsub("\\W*\\b\\w\\b\\W*", " ", dfres_ec[,6])
# remove all extra white space
dfres_ec[,6] <- rm_white(dfres_ec[,6])

# lemmatize the data to better fit the tfidf
col_6_ec <- lemmatize_strings(as.character(dfres_ec[,6]))
# get all column 6 and save as dataframe
col_6_ec <- as.data.frame(col_6_ec)
col_6_ec <- filter(col_6_ec, col_6_ec != 'NA')
# change all to lowercase
col_6_ec <- col_6_ec %>% 
  mutate(col_6_ec = tolower(col_6_ec))
save(col_6_ec, file = "col_6_ec.RData")

#########
# parse en
en <- xmlToList("Posts_en.xml")
dfres_en <- do.call(rbind, lapply(lapply(en, unlist), "[",
                                  unique(unlist(c(sapply(en,names))))))
# only select those with contents
dfres_en <- subset(dfres_en,dfres_en[,7]!="")
# remove all html tags
dfres_en[,7] <- gsub("<[^>]+>", "", dfres_en[,7])
# remove commas and periods and questoin marks
dfres_en[,7] <- str_replace_all(dfres_en[,7], "[^[:alnum:]]", " ")
# remove single characters
dfres_en[,7] <- gsub("\\W*\\b\\w\\b\\W*", " ", dfres_en[,7])
# remove all extra white space
dfres_en[,7] <- rm_white(dfres_en[,7])

# lemmatize the data to better fit the tfidf
col_7_en <- lemmatize_strings(as.character(dfres_en[,7]))
# get all column 7 and save as dataframe
col_7_en <- as.data.frame(col_7_en)
col_7_en <- filter(col_7_en, col_7_en != 'NA')
# change all to lowercase
col_7_en <- col_7_en %>% 
  mutate(col_7_en = tolower(col_7_en))
save(col_7_en, file = "col_7_en.RData")

# all done
# next: tf_idf.r
