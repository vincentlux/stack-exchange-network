# code from Larsen
# Before running this script, run the following scripts (in order):
# 1. parsexml.R (possibly parsexml_chem.R if we upload Vincent's stuff)
# 2. tf_idf.R 

# GOAL(Larsen): Make Binary Classification Feature Sets for as Many Combos of the Categories as Possible
# GOAL(Vincent): First sample and balance the df (lemmatized version), then do the same thing, and compare the accuracy result
# I use PCA, which requires adding noise with jitter to the training set
# knn, 3 fold-cv
# for now, use default neighbors, 3, 5, 9

# Use strings as labels:
# chemistry
# economics
# data science
# biology
# computer science
# engineering

# Boilerplate -------------------------------------------------------------

library(tidyverse)
library(stringr)
library(ggplot2)
library(caret)

# given df of posts (text, category), and the corresponding keywords, get feature set
# where each post is converted to a frequency vector of length keywords
frequency_keyword_feature_list <- function(df, keywords){
  # df is a dataframe containing all the posts. First column is the post "text", second col is label
  # keywords is a dataframe containing all the keywords
  feature_list <- vector("list", length = length(df$text))
  all_keywords <- keywords$word
  for (x in seq_along(df$text)){
    # initialize the frequency vector
    freq_vec <- vector("numeric", length = length(all_keywords))
    # get current post
    current_post <- as.character(df$text[[x]])
    for (i in seq_along(all_keywords)) {
      # get current keyword
      kw_word <- all_keywords[[i]]
      # get count of kw_word's occurence in current post and store it
      # use regular expression to adjust keyword so it can only search full words
      # paste aims to merge strings
      kw_word <- paste0("\\b", kw_word, "\\b")
      freq_count <- str_count(current_post, kw_word)
      freq_vec[[i]] <- freq_count
    }
    feature_list[[x]] <- freq_vec # takes a very long time to run
  }
  return(feature_list)
}

# format output from frequency_keyword_feature_list into a df
feature_to_df <- function(feature_list, type1_posts, type2_posts, label1, label2){
  # feature_list: output from frequency_keyword_feature_list()
  # type1_posts: df of posts on top - used to get number of posts 
  # type2_posts: df of posts on bottom - used to get number of posts
  # label1, label2: corresponding labels
  feat_df <- as.data.frame((t(data.frame(feature_list))))
  n_type1_posts <- dim(type1_posts)[1]
  n_type2_posts <- dim(type2_posts)[1]
  feat_df[["category"]] <- c(rep(label1, n_type1_posts), rep(label2, n_type2_posts))
  return(feat_df)
}

# make a wrapper function for plotting the confusion matrix:
plot_confusion <- function(cm, title){
  cm <- data.frame(cm$table)
  ggplot(cm, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_raster() +
    theme_bw() +
    geom_text(aes(label = Freq), colour = "white") +
    ggtitle(title) +
    theme(legend.position = "none", aspect.ratio = 1)
}

# Get all posts and add a label column
ch_posts <- data_frame(text = col_7_ch[, 1], label = "chemistry")
cs_posts <- data_frame(text = col_7_cs[, 1], label = "computer science")
bi_posts <- data_frame(text = col_7_bi[, 1], label = "biology")
da_posts <- data_frame(text = col_6_da[, 1], label = "data science")
ec_posts <- data_frame(text = col_6_ec[, 1], label = "economics")
en_posts <- data_frame(text = col_7_en[, 1], label = "engineering")

# Extract 500 keywords from each category. Keywords are those with the highest tf-idfs
kw_ch_500 <- tf_idf_desc %>%
  filter(category == 'chemistry') %>%
  select(category, word) %>%
  slice(1:500)
kw_cs_500 <- tf_idf_desc %>%
  filter(category == 'computer science') %>%
  select(category, word) %>%
  slice(1:500)
kw_bi_500 <- tf_idf_desc %>%
  filter(category == 'biology') %>%
  select(category, word) %>%
  slice(1:500)
kw_da_500 <- tf_idf_desc %>%
  filter(category == 'data science') %>%
  select(category, word) %>%
  slice(1:500)
kw_ec_500 <- tf_idf_desc %>%
  filter(category == 'economics') %>%
  select(category, word) %>%
  slice(1:500)
kw_en_500 <- tf_idf_desc %>%
  filter(category == 'engineering') %>%
  select(category, word) %>%
  slice(1:500)

# DO SAMPLING TO BALANCE THE DF
# randomly select 5000 pieces of posts each
# Chemistry and Economics -------------------------------------------------

# get chem and econ posts

ch_posts_samp <- ch_posts[sample(nrow(ch_posts), 5000), ]
ec_posts_samp <- ec_posts[sample(nrow(ec_posts), 5000), ]

ch_ec_samp_posts <- rbind(ch_posts_samp, ec_posts_samp)
# get chem and econ keywords
kw_ch_ec <- rbind(kw_ch_500, kw_ec_500)
#write.csv(kw_ch_ec, "keywords_ch_ec.csv")

# create feature list
ch_ec_samp_feature_list <- frequency_keyword_feature_list(ch_ec_samp_posts, kw_ch_ec)
save(ch_ec_samp_feature_list, file = "ch_ec_samp_feature_list.RData")
#write.csv(ch_ec_samp_feature_list, "ch_ec.csv")

# convert to data frame
ch_ec_samp_feature_df <- feature_to_df(ch_ec_samp_feature_list, ch_posts_samp, ec_posts_samp, "chemistry", "economics")

# Save to csv to check 
#temp <- ch_ec_samp_feature_df
#temp_keywords <- as.data.frame(t(kw_ch_ec))
#temp <- cbind(ch_ec_samp_posts, temp)
#write.csv(temp_keywords, "ch_ec_kw.csv")


# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = ch_ec_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- ch_ec_samp_feature_df[train_idx, ]
test_set <- ch_ec_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
ch_ec_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(ch_ec_samp_knn_model, "ch_ec_samp_knn_model.rds")

ch_ec_samp_knn_model

ggplot(ch_ec_samp_knn_model) +
  theme_bw() +
  ggtitle("Chemistry and Economics(5000 each), 
          3-fold Knn Training Accuracy")

ch_ec_samp_knn_predictions <- predict(ch_ec_samp_knn_model, newdata = test_set)
ch_ec_samp_cm <- confusionMatrix(data = ch_ec_samp_knn_predictions, reference = test_set$category)
print(ch_ec_samp_cm)
saveRDS(ch_ec_samp_cm, "ch_ec_samp_cm.rds")
plot_confusion(ch_ec_samp_cm, "Chemistry/Economics")



# Chemistry and Computer Science ------------------------------------------

ch_posts_samp <- ch_posts[sample(nrow(ch_posts), 5000), ]
cs_posts_samp <- cs_posts[sample(nrow(cs_posts), 5000), ]

# get chem and cs posts
ch_cs_samp_posts <- rbind(ch_posts_samp, cs_posts_samp)
# get chem and cs keywords
kw_ch_cs <- rbind(kw_ch_500, kw_cs_500)

# create feature list
ch_cs_samp_feature_list <- frequency_keyword_feature_list(ch_cs_samp_posts, kw_ch_cs)
save(ch_cs_samp_feature_list, file = "ch_cs_samp_feature_list.RData")
# convert to data frame
ch_cs_samp_feature_df <- feature_to_df(ch_cs_samp_feature_list, ch_posts_samp, cs_posts_samp, "chemistry", "computer science")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = ch_cs_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- ch_cs_samp_feature_df[train_idx, ]
test_set <- ch_cs_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
ch_cs_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(ch_cs_samp_knn_model, "ch_cs_samp_knn_model.rds")

ch_cs_samp_knn_model

ggplot(ch_cs_samp_knn_model) +
  theme_bw() +
  ggtitle("Chemistry and Computer Science(5000 each), 
          3-fold Knn Training Accuracy")

ch_cs_samp_knn_predictions <- predict(ch_cs_samp_knn_model, newdata = test_set)
ch_cs_samp_cm <- confusionMatrix(data = ch_cs_samp_knn_predictions, reference = test_set$category)
print(ch_cs_samp_cm)
saveRDS(ch_cs_samp_cm, file = "ch_cs_samp_cm.rds")
plot_confusion(ch_cs_samp_cm, "Chemistry/Computer Science")



# Chemistry and Biology ---------------------------------------------------

# get chem and bi posts
ch_posts_samp <- ch_posts[sample(nrow(ch_posts), 5000), ]
bi_posts_samp <- bi_posts[sample(nrow(bi_posts), 5000), ]

ch_bi_samp_posts <- rbind(ch_posts_samp, bi_posts_samp)
# get chem and bi keywords
kw_ch_bi <- rbind(kw_ch_500, kw_bi_500)

# create feature list
ch_bi_samp_feature_list <- frequency_keyword_feature_list(ch_bi_samp_posts, kw_ch_bi)
save(ch_bi_samp_feature_list, file = "ch_bi_samp_feature_list.RData")
# convert to data frame
ch_bi_samp_feature_df <- feature_to_df(ch_bi_samp_feature_list, ch_posts_samp, bi_posts_samp, "chemistry", "biology")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = ch_bi_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- ch_bi_samp_feature_df[train_idx, ]
test_set <- ch_bi_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]
train_set_jittered[,1001]
# run knn
cv_3 <- trainControl(method = "cv", number = 3)
ch_bi_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(ch_bi_samp_knn_model, "ch_bi_samp_knn_model.rds")

ch_bi_samp_knn_model

ggplot(ch_bi_samp_knn_model) +
  theme_bw() +
  ggtitle("Chemistry and Biology (5000 each), 
          3-fold Knn Training Accuracy")

ch_bi_samp_knn_predictions <- predict(ch_bi_samp_knn_model, newdata = test_set)
ch_bi_samp_cm <- confusionMatrix(data = ch_bi_samp_knn_predictions, reference = test_set$category)
print(ch_bi_samp_cm)
saveRDS(ch_bi_samp_cm, file = "ch_bi_samp_cm.rds")
plot_confusion(ch_bi_samp_cm, "Chemistry/Biology")




# Chemistry and Data Science ----------------------------------------------

# get chem and da posts
ch_posts_samp <- ch_posts[sample(nrow(ch_posts), 5000), ]
da_posts_samp <- da_posts[sample(nrow(da_posts), 5000), ]
ch_da_samp_posts <- rbind(ch_posts_samp, da_posts_samp)
# get chem and da keywords
kw_ch_da <- rbind(kw_ch_500, kw_da_500)

# create feature list
ch_da_samp_feature_list <- frequency_keyword_feature_list(ch_da_samp_posts, kw_ch_da)
# save(ch_da_feature_list, "ch_da_feature_list.RData")
# convert to data frame
ch_da_samp_feature_df <- feature_to_df(ch_da_samp_feature_list, ch_posts_samp, da_posts_samp, "chemistry", "data science")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = ch_da_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- ch_da_samp_feature_df[train_idx, ]
test_set <- ch_da_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
ch_da_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(ch_da_samp_knn_model, "ch_da_samp_knn_model.rds")

ch_da_samp_knn_model

ggplot(ch_da_samp_knn_model) +
  theme_bw() +
  ggtitle("Chemistry and Data Science (5000 each), 
          3-fold Knn Training Accuracy")

ch_da_samp_knn_predictions <- predict(ch_da_samp_knn_model, newdata = test_set)
ch_da_samp_cm <- confusionMatrix(data = ch_da_samp_knn_predictions, reference = test_set$category)
print(ch_da_samp_cm)
saveRDS(ch_da_samp_cm, "ch_da_samp_cm.rds")
plot_confusion(ch_da_samp_cm, "Chemistry/Data Science")




# Chemistry and Engineering -----------------------------------------------

# get chem and en posts
ch_posts_samp <- ch_posts[sample(nrow(ch_posts), 5000), ]
en_posts_samp <- en_posts[sample(nrow(en_posts), 5000), ]
ch_en_samp_posts <- rbind(ch_posts_samp, en_posts_samp)
# get chem and en keywords
kw_ch_en <- rbind(kw_ch_500, kw_en_500)

# create feature list
ch_en_samp_feature_list <- frequency_keyword_feature_list(ch_en_samp_posts, kw_ch_en)
# save(ch_en_feature_list, "ch_en_feature_list.RData")
# convert to data frame
ch_en_samp_feature_df <- feature_to_df(ch_en_samp_feature_list, ch_posts_samp, en_posts_samp, "chemistry", "engineering")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = ch_en_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- ch_en_samp_feature_df[train_idx, ]
test_set <- ch_en_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
ch_en_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(ch_en_samp_knn_model, "ch_en_samp_knn_model.rds")

ch_en_samp_knn_model

ggplot(ch_en_samp_knn_model) +
  theme_bw() +
  ggtitle("Chemistry and Engineering (5000 each), 
          3-fold Knn Training Accuracy")

ch_en_samp_knn_predictions <- predict(ch_en_samp_knn_model, newdata = test_set)
ch_en_samp_cm <- confusionMatrix(data = ch_en_samp_knn_predictions, reference = test_set$category)
print(ch_en_samp_cm)
saveRDS(ch_en_samp_cm, "ch_en_samp_cm.rds")
plot_confusion(ch_en_samp_cm, "Chemistry/Engineering")





# Computer Science and Biology --------------------------------------------

# get chem and bi posts
cs_posts_samp <- cs_posts[sample(nrow(cs_posts), 5000), ]
bi_posts_samp <- bi_posts[sample(nrow(bi_posts), 5000), ]

cs_bi_samp_posts <- rbind(cs_posts_samp, bi_posts_samp)
# get cs and bi keywords
kw_cs_bi <- rbind(kw_cs_500, kw_bi_500)

# create feature list
cs_bi_samp_feature_list <- frequency_keyword_feature_list(cs_bi_samp_posts, kw_cs_bi)
# save(cs_bi_feature_list, "cs_bi_feature_list.RData")
# convert to data frame
cs_bi_samp_feature_df <- feature_to_df(cs_bi_samp_feature_list, cs_posts_samp, bi_posts_samp, "computer science", "biology")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = cs_bi_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- cs_bi_samp_feature_df[train_idx, ]
test_set <- cs_bi_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
cs_bi_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(cs_bi_samp_knn_model, "cs_bi_samp_knn_model.rds")

cs_bi_samp_knn_model

ggplot(cs_bi_samp_knn_model) +
  theme_bw() +
  ggtitle("Computer Science and Biology (5000 each), 
          3-fold Knn Training Accuracy")

cs_bi_samp_knn_predictions <- predict(cs_bi_samp_knn_model, newdata = test_set)
cs_bi_samp_cm <- confusionMatrix(data = cs_bi_samp_knn_predictions, reference = test_set$category)
print(cs_bi_samp_cm)
saveRDS(cs_bi_samp_cm, "cs_bi_samp_cm.rds")
plot_confusion(cs_bi_samp_cm, "Computer Science/Biology")




# Computer Science and Data Science ---------------------------------------

# get chem and da posts
cs_posts_samp <- cs_posts[sample(nrow(cs_posts), 5000), ]
da_posts_samp <- da_posts[sample(nrow(da_posts), 5000), ]

cs_da_samp_posts <- rbind(cs_posts_samp, da_posts_samp)
# get cs and da keywords
kw_cs_da <- rbind(kw_cs_500, kw_da_500)

# create feature list
cs_da_samp_feature_list <- frequency_keyword_feature_list(cs_da_samp_posts, kw_cs_da)
# save(cs_da_feature_list, "cs_da_feature_list.RData")
# convert to data frame
cs_da_samp_feature_df <- feature_to_df(cs_da_samp_feature_list, cs_posts_samp, da_posts_samp, "computer science", "data science")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = cs_da_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- cs_da_samp_feature_df[train_idx, ]
test_set <- cs_da_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
cs_da_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(cs_da_samp_knn_model, "cs_da_samp_knn_model.rds")

cs_da_samp_knn_model

ggplot(cs_da_samp_knn_model) +
  theme_bw() +
  ggtitle("Computer Science and Data Science (5000 each), 
          3-fold Knn Training Accuracy")

cs_da_samp_knn_predictions <- predict(cs_da_samp_knn_model, newdata = test_set)
cs_da_samp_cm <- confusionMatrix(data = cs_da_samp_knn_predictions, reference = test_set$category)
print(cs_da_samp_cm)
saveRDS(cs_da_samp_cm, "cs_da_samp_cm.rds")
plot_confusion(cs_da_samp_cm, "Computer Science/Data Science")



# Computer Science and Economics ------------------------------------------

# get cs and ec posts
cs_posts_samp <- cs_posts[sample(nrow(cs_posts), 5000), ]
ec_posts_samp <- ec_posts[sample(nrow(ec_posts), 5000), ]

cs_ec_samp_posts <- rbind(cs_posts_samp, ec_posts_samp)
# get cs and ec keywords
kw_cs_ec <- rbind(kw_cs_500, kw_ec_500)

# create feature list
cs_ec_samp_feature_list <- frequency_keyword_feature_list(cs_ec_samp_posts, kw_cs_ec)
# save(cs_ec_feature_list, "cs_ec_feature_list.RData")
# convert to data frame
cs_ec_samp_feature_df <- feature_to_df(cs_ec_samp_feature_list, cs_posts_samp, ec_posts_samp, "computer science", "economics")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = cs_ec_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- cs_ec_samp_feature_df[train_idx, ]
test_set <- cs_ec_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
cs_ec_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(cs_ec_samp_knn_model, "cs_ec_samp_knn_model.rds")

cs_ec_samp_knn_model

ggplot(cs_ec_samp_knn_model) +
  theme_bw() +
  ggtitle("Computer Science and Economics (5000 each), 
          3-fold Knn Training Accuracy")

cs_ec_samp_knn_predictions <- predict(cs_ec_samp_knn_model, newdata = test_set)
cs_ec_samp_cm <- confusionMatrix(data = cs_ec_samp_knn_predictions, reference = test_set$category)
print(cs_ec_samp_cm)
saveRDS(cs_ec_samp_cm, "cs_ec_samp_cm.rds")
plot_confusion(cs_ec_samp_cm, "Computer Science/Economics")



# Computer Science and Engineering ----------------------------------------

# get cs and en posts
cs_posts_samp <- cs_posts[sample(nrow(cs_posts), 5000), ]
en_posts_samp <- en_posts[sample(nrow(en_posts), 5000), ]

cs_en_samp_posts <- rbind(cs_posts_samp, en_posts_samp)
# get cs and en keywords
kw_cs_en <- rbind(kw_cs_500, kw_en_500)

# create feature list
cs_en_samp_feature_list <- frequency_keyword_feature_list(cs_en_samp_posts, kw_cs_en)
# save(cs_en_feature_list, "cs_en_feature_list.RData")
# convert to data frame
cs_en_samp_feature_df <- feature_to_df(cs_en_samp_feature_list, cs_posts_samp, en_posts_samp, "computer science", "engineering")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = cs_en_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- cs_en_samp_feature_df[train_idx, ]
test_set <- cs_en_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
cs_en_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(cs_en_samp_knn_model, "cs_en_samp_knn_model.rds")

cs_en_samp_knn_model

ggplot(cs_en_samp_knn_model) +
  theme_bw() +
  ggtitle("Computer Science and Engineering (5000 each), 
          3-fold Knn Training Accuracy")

cs_en_samp_knn_predictions <- predict(cs_en_samp_knn_model, newdata = test_set)
cs_en_samp_cm <- confusionMatrix(data = cs_en_samp_knn_predictions, reference = test_set$category)
print(cs_en_samp_cm)
saveRDS(cs_en_samp_cm, "cs_en_samp_cm.rds")

plot_confusion(cs_en_samp_cm, "Computer Science/Engineering")




# Biology and Data Science ------------------------------------------------

# get bi and da posts
bi_posts_samp <- bi_posts[sample(nrow(bi_posts), 5000), ]
da_posts_samp <- da_posts[sample(nrow(da_posts), 5000), ]

bi_da_samp_posts <- rbind(bi_posts_samp, da_posts_samp)
# get bi and da keywords
kw_bi_da <- rbind(kw_bi_500, kw_da_500)

# create feature list
bi_da_samp_feature_list <- frequency_keyword_feature_list(bi_da_samp_posts, kw_bi_da)
# save(bi_da_feature_list, "bi_da_feature_list.RData")
# convert to data frame
bi_da_samp_feature_df <- feature_to_df(bi_da_samp_feature_list, bi_posts_samp, da_posts_samp, "biology", "data science")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = bi_da_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- bi_da_samp_feature_df[train_idx, ]
test_set <- bi_da_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
bi_da_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(bi_da_samp_knn_model, "bi_da_samp_knn_model.rds")

bi_da_samp_knn_model

ggplot(bi_da_samp_knn_model) +
  theme_bw() +
  ggtitle("Biology and Data Science (5000 each), 
          3-fold Knn Training Accuracy")


bi_da_samp_knn_predictions <- predict(bi_da_samp_knn_model, newdata = test_set)
bi_da_samp_cm <- confusionMatrix(data = bi_da_samp_knn_predictions, reference = test_set$category)
print(bi_da_samp_cm)
saveRDS(bi_da_samp_cm, "bi_da_samp_cm.rds")

plot_confusion(bi_da_samp_cm, "Biology/Data Science")




# Biology and Economics ---------------------------------------------------

# get bi and ec posts
bi_posts_samp <- bi_posts[sample(nrow(bi_posts), 5000), ]
ec_posts_samp <- ec_posts[sample(nrow(ec_posts), 5000), ]

bi_ec_samp_posts <- rbind(bi_posts_samp, ec_posts_samp)
# get bi and ec keywords
kw_bi_ec <- rbind(kw_bi_500, kw_ec_500)

# create feature list
bi_ec_samp_feature_list <- frequency_keyword_feature_list(bi_ec_samp_posts, kw_bi_ec)
# save(bi_ec_feature_list, "bi_ec_feature_list.RData")
# convert to data frame
bi_ec_samp_feature_df <- feature_to_df(bi_ec_samp_feature_list, bi_posts_samp, ec_posts_samp, "biology", "economics")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = bi_ec_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- bi_ec_samp_feature_df[train_idx, ]
test_set <- bi_ec_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
bi_ec_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                         preProcess = "pca", trControl = cv_3)

saveRDS(bi_ec_samp_knn_model, "bi_ec_samp_knn_model.rds")

bi_ec_samp_knn_model

ggplot(bi_ec_samp_knn_model) +
  theme_bw() +
  ggtitle("Biology and Economics (5000 each), 
          3-fold Knn Training Accuracy")

bi_ec_samp_knn_predictions <- predict(bi_ec_samp_knn_model, newdata = test_set)
bi_ec_samp_cm <- confusionMatrix(data = bi_ec_samp_knn_predictions, reference = test_set$category)
print(bi_ec_samp_cm)
saveRDS(bi_ec_samp_cm, "bi_ec_samp_cm.rds")

plot_confusion(bi_ec_samp_cm, "Biology/Economics")




# Biology and Engineering -------------------------------------------------

# get bi and en posts
bi_posts_samp <- bi_posts[sample(nrow(bi_posts), 5000), ]
en_posts_samp <- en_posts[sample(nrow(en_posts), 5000), ]

bi_en_samp_posts <- rbind(bi_posts_samp, en_posts_samp)
# get bi and en keywords
kw_bi_en <- rbind(kw_bi_500, kw_en_500)

# create feature list
bi_en_samp_feature_list <- frequency_keyword_feature_list(bi_en_samp_posts, kw_bi_en)
# save(bi_ec_feature_list, "bi_en_feature_list.RData")
# convert to data frame
bi_en_samp_feature_df <- feature_to_df(bi_en_samp_feature_list, bi_posts_samp, en_posts_samp, "biology", "engineering")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = bi_en_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- bi_en_samp_feature_df[train_idx, ]
test_set <- bi_en_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
bi_en_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                              preProcess = "pca", trControl = cv_3)

saveRDS(bi_en_samp_knn_model, "bi_en_samp_knn_model.rds")

bi_en_samp_knn_model

ggplot(bi_en_samp_knn_model) +
  theme_bw() +
  ggtitle("Biology and Engineering (5000 each), 
          3-fold Knn Training Accuracy")

bi_en_samp_knn_predictions <- predict(bi_en_samp_knn_model, newdata = test_set)
bi_en_samp_cm <- confusionMatrix(data = bi_en_samp_knn_predictions, reference = test_set$category)
print(bi_en_samp_cm)
saveRDS(bi_en_samp_cm, "bi_en_samp_cm.rds")

plot_confusion(bi_en_samp_cm, "Biology/Engineering")




# Data Science and Economics ---------------------------------------------------

# get da and ec posts
da_posts_samp <- da_posts[sample(nrow(da_posts), 5000), ]
ec_posts_samp <- ec_posts[sample(nrow(ec_posts), 5000), ]

da_ec_samp_posts <- rbind(da_posts_samp, ec_posts_samp)
# get bi and ec keywords
kw_da_ec <- rbind(kw_da_500, kw_ec_500)

# create feature list
da_ec_samp_feature_list <- frequency_keyword_feature_list(da_ec_samp_posts, kw_da_ec)

# convert to data frame
da_ec_samp_feature_df <- feature_to_df(da_ec_samp_feature_list, da_posts_samp, ec_posts_samp, "data science", "economics")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = da_ec_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- da_ec_samp_feature_df[train_idx, ]
test_set <- da_ec_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
da_ec_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                              preProcess = "pca", trControl = cv_3)

saveRDS(da_ec_samp_knn_model, "da_ec_samp_knn_model.rds")

da_ec_samp_knn_model

ggplot(da_ec_samp_knn_model) +
  theme_bw() +
  ggtitle("Data Science and Economics (5000 each), 
          3-fold Knn Training Accuracy")

da_ec_samp_knn_predictions <- predict(da_ec_samp_knn_model, newdata = test_set)
da_ec_samp_cm <- confusionMatrix(data = da_ec_samp_knn_predictions, reference = test_set$category)
print(da_ec_samp_cm)
saveRDS(da_ec_samp_cm, "da_ec_samp_cm.rds")
plot_confusion(da_ec_samp_cm, "Data Science/Economics")




# Data Science and Engineering ---------------------------------------------------

# get da and en posts
da_posts_samp <- da_posts[sample(nrow(da_posts), 5000), ]
en_posts_samp <- en_posts[sample(nrow(en_posts), 5000), ]

da_en_samp_posts <- rbind(da_posts_samp, en_posts_samp)
# get da and en keywords
kw_da_en <- rbind(kw_da_500, kw_en_500)

# create feature list
da_en_samp_feature_list <- frequency_keyword_feature_list(da_en_samp_posts, kw_da_en)
# convert to data frame
da_en_samp_feature_df <- feature_to_df(da_en_samp_feature_list, da_posts_samp, en_posts_samp, "data science", "engineering")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = da_en_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- da_en_samp_feature_df[train_idx, ]
test_set <- da_en_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
da_en_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                              preProcess = "pca", trControl = cv_3)

saveRDS(da_en_samp_knn_model, "da_en_samp_knn_model.rds")

da_en_samp_knn_model

ggplot(da_en_samp_knn_model) +
  theme_bw() +
  ggtitle("Data Science and Engineering (5000 each), 
          3-fold Knn Training Accuracy")

da_en_samp_knn_predictions <- predict(da_en_samp_knn_model, newdata = test_set)
da_en_samp_cm <- confusionMatrix(data = da_en_samp_knn_predictions, reference = test_set$category)
print(da_en_samp_cm)
saveRDS(da_en_samp_cm, "da_en_samp_cm.rds")
plot_confusion(da_en_samp_cm, "Data Science/Engineering")




# Economics and Engineering ---------------------------------------------------

# get da and en posts
ec_posts_samp <- ec_posts[sample(nrow(ec_posts), 5000), ]
en_posts_samp <- en_posts[sample(nrow(en_posts), 5000), ]

ec_en_samp_posts <- rbind(ec_posts_samp, en_posts_samp)
# get da and en keywords
kw_ec_en <- rbind(kw_ec_500, kw_en_500)

# create feature list
ec_en_samp_feature_list <- frequency_keyword_feature_list(ec_en_samp_posts, kw_ec_en)
# convert to data frame
ec_en_samp_feature_df <- feature_to_df(ec_en_samp_feature_list, ec_posts_samp, en_posts_samp, "economics", "engineering")

# split into training and test sets (70% train, 30% test)
# NOTE: USING SAME VARIABLE NAMES - SO BE SURE TO UPDATE FOR EACH CHUNCK OF CODE.
train_idx <- createDataPartition(y = ec_en_samp_feature_df$category, p = 0.7, list = FALSE)
train_set <- ec_en_samp_feature_df[train_idx, ]
test_set <- ec_en_samp_feature_df[-train_idx, ]
# caret needs the training set labels to be categorical (otherwise will throw error if you don't want regression)
train_set[["category"]] = factor(train_set[["category"]])
# add noise so PCA will work
train_set_jittered <- data.frame(lapply(as.data.frame(train_set[, 1:1000]), jitter)) # leave out the category column
train_set_jittered[["category"]] <- train_set[["category"]]

# run knn
cv_3 <- trainControl(method = "cv", number = 3)
ec_en_samp_knn_model <- train(category ~., data = train_set_jittered, method = "knn",
                              preProcess = "pca", trControl = cv_3)

saveRDS(ec_en_samp_knn_model, "ec_en_samp_knn_model.rds")

ec_en_samp_knn_model

ggplot(ec_en_samp_knn_model) +
  theme_bw() +
  ggtitle("Economics and Engineering (5000 each), 
          3-fold Knn Training Accuracy")

ec_en_samp_knn_predictions <- predict(ec_en_samp_knn_model, newdata = test_set)
ec_en_samp_cm <- confusionMatrix(data = ec_en_samp_knn_predictions, reference = test_set$category)
print(ec_en_samp_cm)
saveRDS(ec_en_samp_cm, "ec_en_samp_cm.rds")

plot_confusion(ec_en_samp_cm, "Economics/Engineering")










