knn_names <- c("ec_en_samp_knn_model.rds",
               "ch_ec_samp_knn_model.rds",
               "ch_cs_samp_knn_model.rds",
               "ch_bi_samp_knn_model.rds",
               "da_ec_samp_knn_model.rds",
               "da_en_samp_knn_model.rds",
               "bi_en_samp_knn_model.rds",
               "cs_da_samp_knn_model.rds",
               "cs_ec_samp_knn_model.rds",
               "cs_bi_samp_knn_model.rds",
               "ch_da_samp_knn_model.rds",
               "bi_da_samp_knn_model.rds",
               "bi_ec_samp_knn_model.rds")

knn_names_2 <- c("ec_en", "ch_ec", "ch_cs", "ch_bi", "da_ec", "da_en", "bi_en", "cs_da","cs_ec",
                 "cs_bi", "ch_da", "bi_da", "bi_ec")

read_in_knn_models <- lapply(knn_names, readRDS)
names(read_in_knn_models) <- knn_names_2

# get a named list of the training accuracies of the models used for predictions
knn_best_model_train_acc <- lapply(read_in_knn_models, function(i) max(i$results$Accuracy))
# do the same to get the corresponding number of neighbors
knn_best_model_n_neigh <- lapply(read_in_knn_models, function(i) i$bestTune)

knn_best_model_train_acc_df <- data.frame(t(data.frame(knn_best_model_train_acc)))
colnames(knn_best_model_train_acc_df) <- "Training_Accuracy"
knn_best_model_n_neigh_df <- data.frame(t(data.frame(knn_best_model_n_neigh)))
colnames(knn_best_model_n_neigh_df) <- "k"
write.csv(knn_best_model_train_acc_df, "knn_best_model_train_acc_df.csv")

# now get the predicted accuracies on the test sets and the balanced accuracies.

knn_cm_names <- c("ec_en_samp_cm.rds",
                  "ch_ec_samp_cm.rds",
                  "ch_cs_samp_cm.rds",
                  "ch_bi_samp_cm.rds",
                  "da_ec_samp_cm.rds",
                  "da_en_samp_cm.rds",
                  "bi_en_samp_cm.rds",
                  "cs_da_samp_cm.rds",
                  "cs_ec_samp_cm.rds",
                  "cs_bi_samp_cm.rds",
                  "ch_da_samp_cm.rds",
                  "bi_da_samp_cm.rds",
                  "bi_ec_samp_cm.rds")

read_in_knn_cm <- lapply(knn_cm_names, readRDS)
names(read_in_knn_cm) <- knn_names_2

# extract the accuracy on the test set
cm_test_acc <- lapply(read_in_knn_cm, function(i) i$overall[['Accuracy']])
cm_test_acc_df <- data.frame(t(data.frame(cm_test_acc)))
colnames(cm_test_acc_df) <- "Test_Accuracy"

# do the same for balanced accuracy
cm_balanced_acc <- lapply(read_in_knn_cm, function(i) i$byClass[["Balanced Accuracy"]])
cm_balanced_acc_df <- data.frame(t(data.frame(cm_balanced_acc)))
colnames(cm_balanced_acc_df) <- "Balanced_Accuracy"

# combine into a single dataset
knn_models_summary <- cbind(knn_best_model_train_acc_df, cm_test_acc_df, cm_balanced_acc_df, 
                            knn_best_model_n_neigh_df)
write.csv(knn_models_summary, "knn_models_summary.csv")
