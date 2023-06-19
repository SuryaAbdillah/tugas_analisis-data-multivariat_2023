# 5025201229 / Surya Abdillah

# tugas: melakukan logistic regression dan multidimensional scaling

# ** CLUSTERING **
#install library yang diperlukan
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("cluster")
#install.packages("dendextend")
#install.packages("factoextra")
#install.packages("tidyverse")

# call library yang diperlukan
library("readxl")
library("dplyr")
library("cluster")
library("ggplot2")
library("dendextend")
library("factoextra")
library("tidyverse")

setwd('E:/Kuliah/SEMESTER 6/Analisis Data Multivariat')

# membaca dataset
# NB: dataset telah diubah dari tipe 97 - 2003 menjadi .xlsx
df_HBAT <- read_excel("1_Multivariate_Data_Analysis_7e_Datasets_EXCEL.xlsx", sheet = "HBAT")
df_HBAT

# dimensi
dim(df_HBAT)

# isi
glimpse(df_HBAT)

# isi data secara singkat
head(df_HBAT)
tail(df_HBAT)

# ringkasan penyebaran data
summary(df_HBAT)

# ** KLASIFIKASI **
#install.packages("mvnormtest")
#install.packages("BART")

# call library yang diperlukan
library("mvnormtest")
library("BART")

# Copy data frame
df_classification <- df_HBAT

# mengambil fitur yang berhubungan dengan soal
df_classification = subset(df_classification, select = c(X4, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18))

# membuat matriks korelasi sebagai pertimbangan feature selection
cor_matrix <- cor(df_classification)
cor_matrix <- round(cor_matrix, 2)
cor_matrix

# dari matriks korelasi tersebut kita akan menggunakan fitur yang memiliki nilai korelasi di atas 0.3
abs(cor_matrix[1,1:14]) > 0.3

# didapatkan fitur dengan korelasi diatas 0.3 adalah:
# x6, x11, x12, x13, x17
df_classification2 <- df_classification
df_classification2 = subset(df_classification, select = c(X4, X6, X11, X12, X13, X17))

# merubah X4 menjadi tipe factor
df_classification$X4 = as.factor(df_classification$X4)
df_classification2$X4 = as.factor(df_classification2$X4)

# * LOGISTIC REGRESSION *
#install.packages("tidymodels")
#install.packages("glmnet")
#install.packages("micompr")

# call library yang diperlukan
library("tidymodels")
library("glmnet")
library("micompr")

# Mendapatkan nilai significance
logistic <- glm(X4 ~ ., data = df_classification, family = "binomial")
tidy(logistic)

df_classification3 = subset(df_classification, select = c(X4, X6, X7, X12, X13))
logistic <- glm(X4 ~ ., data = df_classification3, family = "binomial")
tidy(logistic)

# menemukan PARAMETER terbaik
# definisi model dengan parameter penalty dan mixture
log_reg <- logistic_reg(mixture = tune(), penalty = tune(), engine = "glmnet")

# definisi pencarian grid untuk hyperparameter
grid <- grid_regular(mixture(), penalty(), levels = c(mixture = 4, penalty = 3))

# definisi workflow model
log_reg_wf <- workflow() %>%
  add_model(log_reg) %>%
  add_formula(X4 ~ .)

# definisi metode resampling
# Split data into train and test
set.seed(42) # random_state

split <- initial_split(df_classification, prop = 0.8, strata = X4)
train <- split %>% 
  training()
test <- split %>% 
  testing()

split2 <- initial_split(df_classification2, prop = 0.8, strata = X4)
train2 <- split2 %>% 
  training()
test2 <- split2 %>% 
  testing()

split3 <- initial_split(df_classification3, prop = 0.8, strata = X4)
train3 <- split3 %>% 
  training()
test3 <- split3 %>% 
  testing()

folds <- vfold_cv(train, v = 5)
folds2 <- vfold_cv(train2, v = 5)
folds3 <- vfold_cv(train3, v = 5)

# mencari parameter per grid
log_reg_tuned <- tune_grid(
  log_reg_wf,
  resamples = folds,
  grid = grid,
  control = control_grid(save_pred = TRUE)
)
log_reg_tuned2 <- tune_grid(
  log_reg_wf,
  resamples = folds2,
  grid = grid,
  control = control_grid(save_pred = TRUE)
)
log_reg_tuned3 <- tune_grid(
  log_reg_wf,
  resamples = folds3,
  grid = grid,
  control = control_grid(save_pred = TRUE)
)


# berdasarkan metode di atas didapati parameter terbaik adalah penalty = 1 dan mixture = 0
select_best(log_reg_tuned, metric = "roc_auc")
 select_best(log_reg_tuned2, metric = "roc_auc")
select_best(log_reg_tuned3, metric = "roc_auc")

# PERCOBAAN dengan hasil parameter terbaik
# Fit the model using the optimal hyperparameters
log_reg_final <- logistic_reg(penalty = 0.0000000001, mixture = 0.333) %>%
                 set_engine("glmnet") %>%
                 set_mode("classification") %>%
                 fit(X4~., data = train)
tidy(log_reg_final)

log_reg_final2 <- logistic_reg(penalty = 0.0000000001, mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(X4~., data = train2)
tidy(log_reg_final2)

log_reg_final3 <- logistic_reg(penalty = 0.0000000001, mixture = 0.333) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(X4~., data = train3)
tidy(log_reg_final3)

# Evaluate the model performance on the testing set
pred_class <- predict(log_reg_final,
                      new_data = test,
                      type = "class")
pred_class2 <- predict(log_reg_final2,
                      new_data = test2,
                      type = "class")
pred_class3 <- predict(log_reg_final3,
                       new_data = test3,
                       type = "class")

# Class Probabilities
pred_proba <- predict(log_reg_final,
                      new_data = test,
                      type = "prob")
pred_proba2 <- predict(log_reg_final2,
                      new_data = test2,
                      type = "prob")
pred_proba3 <- predict(log_reg_final3,
                       new_data = test3,
                       type = "prob")
pred_proba2
results <- test %>%
  bind_cols(pred_class, pred_proba)
results2 <- test2 %>%
  bind_cols(pred_class2, pred_proba2)
results3 <- test3 %>%
  bind_cols(pred_class3, pred_proba3)

# akurasi
pred_class_mat = as.matrix(pred_class)
mean(pred_class_mat==test$X4)

pred_class1_mat = as.matrix(pred_class2)
mean(pred_class1_mat==test2$X4)

pred_class2_mat = as.matrix(pred_class3)
mean(pred_class2_mat==test3$X4)

# Create confusion matrix
conf_mat(results, truth = X4, estimate = .pred_class)
conf_mat(results2, truth = X4, estimate = .pred_class)
conf_mat(results3, truth = X4, estimate = .pred_class)

# RECALL
recall(results, truth = X4, estimate = .pred_class)
recall(results2, truth = X4, estimate = .pred_class)
recall(results3, truth = X4, estimate = .pred_class)

# PRECISION
precision(results, truth = X4, estimate = .pred_class)
precision(results2, truth = X4, estimate = .pred_class)
precision(results3, truth = X4, estimate = .pred_class)


## ** MDS ** ##
#install.packages("magrittr")
#install.packages("pheatmap")
library(magrittr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(pheatmap)

df_MDS <- read_excel("1_Multivariate_Data_Analysis_7e_Datasets_EXCEL.xlsx", sheet = "HBAT_MDS")
df_MDS

new_df <- df_MDS[1:10,]
new_df <- new_df %>% mutate_all(as.numeric)
new_df

for(baris in 11:180){
  for(kolom in 1:10){
    bar <- baris %% 10
    if(bar == 0){
      bar <- 10
    }
    new_df[bar, kolom] <-  as.integer(new_df[bar, kolom]) + as.integer(df_MDS[baris, kolom])
  }
}

new_df

for(baris in 1:10)
{
  for(kolom in 1:10)
  {
    if(kolom > baris)
    {
      new_df[baris, kolom] <- new_df[kolom, baris]
    }
  }
}

min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
new_df <- as.data.frame(lapply(new_df, min_max_scale))
new_df

pheatmap(new_df, cluster_rows = TRUE, treeheight_row = 0.0001, treeheight_col = 0.8, fontsize_col = 8, cellwidth = 13, cellheight = 13)

MDS <- cmdscale(new_df, eig = TRUE)

plotbar <- function(res, m = 9){
  ggplot(data.frame(list(eig = res$eig[seq_len(m)],
                         k = seq(along = res$eig[seq_len(m)]))),
         aes(x = k, y = eig)) +
  scale_x_discrete("k", limits = factor(seq_len(m))) +
    theme_minimal() +
    geom_bar(stat = "identity", width = 0.5, color = "orange", fill = "pink")
}

labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
plotbar(MDS, m = 10)

MDSuer <- data.frame(list(PCo1 = MDS$points[, 1],
                          PCo2 = MDS$points[, 2]))
ggplot(MDSuer, aes(x = PCo1, y = PCo2, label = labels)) +
  geom_point(color = "red") +
  xlim(-1,1)+
  ylim(-1,1)+
  geom_text(aes(label = labels), vjust = -1)

