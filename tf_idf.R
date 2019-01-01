# do parse_xml.r first
# tutorial from http://tidytextmining.com/tfidf.html#term-frequency-in-jane-austens-novels
# ch cs bi da ec en
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyverse)
library(janeaustenr)
library(tidytext)
library(ggplot2)
library(devtools)
#library(qdap)
options(tibble.width = Inf)
options(tibble.print_max = 1, tibble.print_min = 20)

# First column text(posts)
# Second colum book name(category)
# try with chem posts
words_ch <- data_frame(text = col_7_ch[,1], category = "chemistry")
words_ch
# cs
words_cs <- data_frame(text = col_7_cs[,1], category = "computer science")
words_cs
# bi
words_bi <- data_frame(text = col_7_bi[,1], category = "biology")
words_bi
# da
words_da <- data_frame(text = col_6_da[,1], category = "data science")
words_da
# ec
words_ec <- data_frame(text = col_6_ec[,1], category = "economics")
words_ec
# en
words_en <- data_frame(text = col_7_en[,1], category = "engineering")
words_en


# connect them altogether to form a n*2 df: first column posts and second column categories
posts_all <- rbind(words_ch, words_cs, words_bi, words_da, words_ec, words_en)

# change to character
posts_all[, 1] <- sapply(posts_all[, 1], as.character)
posts_words <- posts_all %>%
  unnest_tokens(word, text) %>%
  dplyr::count(category, word, sort = TRUE) %>%
  ungroup()
posts_words
# get the total words in each category for later use
total_words <- posts_words %>%
  group_by(category) %>%
  summarize(total = sum(n))
# join them together
posts_words <- left_join(posts_words, total_words)
posts_words
# excited
#######################################################################
# use tf-idf function
posts_words <- posts_words %>%
  bind_tf_idf(word, category, n)
posts_words
# let's look at terms with high tf-idf
tf_idf_desc <- posts_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))
tf_idf_desc
save(tf_idf_desc, file = "tf_idf_desc.RData")
save(posts_words, file = "tf_idf.RData")

# visualization
# general plot
load("tf_idf.RData")
plot_category <- posts_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_category %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = category)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

options(scipen=999)
# plot individually
plot_category %>% 
  group_by(category) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = category)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~category, ncol = 2, scales = "free") +
  coord_flip()


# all done
# next:
# go to either wordcloud.r
# or knn_sample_vincent.r