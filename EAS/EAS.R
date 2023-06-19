library("readxl")
library("dplyr")
library("cluster")
library("ggplot2")
library("dendextend")
library("factoextra")
library("tidyverse")
library("tidymodels")
library("glmnet")
library("micompr")
library("mvnormtest")
library("BART")
library("cat")
library("MASS")
library("magrittr")
library("ggplot2")
library("ggpubr")
library("pheatmap")
library("GGally")

setwd('E:/Kuliah/SEMESTER 6/Analisis Data Multivariat/Data_3_Soal_EAS_ADM')

# ** LOGISTIC REGRESSION ** #
df_lr <- read_csv("soal_logistic_regr.csv")
df_lr

# dimensi
dim(df_lr)

# isi
glimpse(df_lr)

# isi data secara singkat
head(df_lr)
tail(df_lr)

# ringkasan penyebaran data
summary(df_lr)

# uniqeu pada model, didapati model == id, maka akan didrop
length(unique(df_lr$model))
# binary class
length(unique(df_lr$am))

# distribusi am
xtabs(~am, data = df_lr)

df_lr <- subset(df_lr, select=-c(model))

df_lr$am = as.factor(df_lr$vs)
df_lr$am = as.factor(df_lr$am)

# Mendapatkan nilai significance
logistic <- glm(am ~ ., data = df_lr, family = "binomial")
tidy(logistic)

# didapati bahwa nilai p.value pada semua prediktor bernilai 1, hal ini menandakan bahwa reject hipotesis null, sehingga tidak ada  prediktor yang berpengaruh signifikan pada target

# menemukan PARAMETER terbaik
# definisi model dengan parameter penalty dan mixture
log_reg <- logistic_reg(mixture = tune(), penalty = tune(), engine = "glmnet")

# definisi pencarian grid untuk hyperparameter
grid <- grid_regular(mixture(), penalty(), levels = c(mixture = 4, penalty = 3))

# definisi workflow model
log_reg_wf <- workflow() %>%
  add_model(log_reg) %>%
  add_formula(am ~ .)

# definisi metode resampling
# Split data into train and test
set.seed(42) # random_state

split <- initial_split(df_lr, prop = 0.8, strata = am)
train <- split %>% 
  training()
test <- split %>% 
  testing()

folds <- vfold_cv(train, v = 3)

# mencari parameter per grid
log_reg_tuned <- tune_grid(
  log_reg_wf,
  resamples = folds,
  grid = grid,
  control = control_grid(save_pred = TRUE)
)

select_best(log_reg_tuned, metric = "roc_auc")

# PERCOBAAN dengan hasil parameter terbaik
# Fit the model using the optimal hyperparameters
log_reg_final <- logistic_reg(penalty = 0.0000000001, mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(am~., data = train)
tidy(log_reg_final)

# Evaluate the model performance on the testing set
pred_class <- predict(log_reg_final,
                      new_data = test,
                      type = "class")
# Class Probabilities
pred_proba <- predict(log_reg_final,
                      new_data = test,
                      type = "prob")
results <- test %>%
  bind_cols(pred_class, pred_proba)

# Create confusion matrix
conf_mat(results, truth = am, estimate = .pred_class)

# akurasi
pred_class_mat = as.matrix(pred_class)
mean(pred_class_mat==test$am)
# RECALL
recall(results, truth = am, estimate = .pred_class)
# PRECISION
precision(results, truth = am, estimate = .pred_class)

# ** LDA ** #
df_lda <- read_excel("soal Discriinant Analysis.xlsx")
colnames(df_lda) <- c('sepal_length', 'sepal_width', 'petal_length', 'petal_width', 'class', 'label')
df_lda <- subset(df_lda, select=-c(class))
df_lda

# dimensi
dim(df_lda)

# isi
glimpse(df_lda)

# ringkasan penyebaran data
summary(df_lda)

ggpairs(df_lda)

# multi class
length(unique(df_lda$label))

# distribusi am
xtabs(~label, data = df_lda)

# membuat matriks korelasi sebagai pertimbangan feature selection
cor_matrix <- cor(df_lda)
cor_matrix <- round(cor_matrix, 2)
cor_matrix
cor_matrix[5, 1:5]

# ANALISIS MANOVA
# asumsi normalitas multivariat
# H0: data berdistribusi normal multivariat
# H1: data bukan berdistribusi normal multivariat
mshapiro.test(t(df_lda[1]))
mshapiro.test(t(df_lda[2]))
mshapiro.test(t(df_lda[3]))
mshapiro.test(t(df_lda[4]))

# kesamaan matriks kovarian
# H0: tidak terdapat perbedaan variance antar group
# H1: terdapat perbedaan variance antar group
result = bartlett.test(df_lda[,0:4], df_lda$label)
print(result)

# MANOVA
df_lda$label = as.factor(df_lda$label)
manova_res = manova(cbind(df_lda$sepal_length, df_lda$sepal_width, df_lda$petal_length, df_lda$petal_width) ~ df_lda$label, data = df_lda)
# H0: rata-rata 1 = rata-rata 2 = ... = rata-rata 9 =  0
# H1: setidaknya ada satu rata-rata, dimana tidak sama dengan 0
summary(manova_res, test = "Pillai")
summary(manova_res, test = "Roy")
summary(manova_res, test = "Wilks")
summary(manova_res, test = "Hotelling-Lawley")
# H0: Xi tidak berpengaruh signifikan terhadap X23
# H1: Xi berpengaruh signifikan terhadap X23
summary.aov(manova_res)

# Split data into train and test
set.seed(42) # random_state
split <- initial_split(df_lda, prop = 0.7, strata = label)
train <- split %>% training()
test <- split %>% testing()

# membuat model
lda_model = lda(label ~ ., data = train)
lda_model

# prediksi
y_pred = predict(lda_model, test)
names(y_pred) # atribut yang dimiliki
head(y_pred$class) # class hasil prediksi
head(y_pred$posterior) # probabilitas data masuk ke class tersebut
head(y_pred$x) # linear discriminant

# akurasi
acc_lda <- mean(y_pred$class==test$label)
results_lda <- test %>% bind_cols(y_pred$class, y_pred$posterior)
acc_lda
# Create confusion matrix
conf_matLDA <- conf_mat(results_lda, truth = label, estimate =...6)
conf_matLDA

# RECALL
recall(results_lda, truth = label, estimate = ...6)

# PRECISION
precision(results_lda, truth = label, estimate = ...6)

lda.data <- cbind(train, predict(lda_model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = label))

# ** MULTIDIMENSIONAL SCALING ** #
df_MDS <- read_excel("soal MDS.xlsx")
df_MDS

new_df <- df_MDS[3:13, 2:12]
new_df
dim(new_df)
colnames(new_df) <- c('Mustang SVO', 'Cadillac Seville', 'Lincoln Continental', 'Ford Escort', 'Corvette', 'Chevrolet Chevette', 'Nissan 300 ZX', 'Renault Alliance', 'Porsche 944', 'Jaguar XJ6', 'Mercedes 500 SEL')

new_df <- data.frame(sapply(new_df, as.numeric))

for(baris in 1:11)
{
  for(kolom in 1:11)
  {
    if(kolom == baris){
      new_df[baris, kolom] <- 0
    }
    if(kolom > baris)
    {
      new_df[baris, kolom] <- new_df[kolom, baris]
    }
  }
}
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
plotbar(MDS, m = 11)

labels =c('Mustang', 'Cadillac', 'Lincoln', 'Ford', 'Corvette', 'Chevrolet', 'Nissan', 'Renault', 'Porsche', 'Jaguar', 'Mercedes')
MDSuer <- data.frame(list(PCo1 = MDS$points[, 1],
                          PCo2 = MDS$points[, 2]))
ggplot(MDSuer, aes(x = PCo1, y = PCo2, label = labels)) +
  geom_point(color = "red") +
  xlim(-50,50)+
  ylim(-50,50)+
  geom_text(aes(label = labels), vjust = -1, angle = 90, size = 3)

