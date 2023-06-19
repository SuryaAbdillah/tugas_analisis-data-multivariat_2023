# IDENTITAS
# 5025201229 / Surya Abdillah

#install library yang diperlukan
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("writexl")

# call library yang diperlukan
library("readxl")
library("dplyr")
library("writexl")

# membaca dataset
# NB: dataset telah diubah dari tipe 97 - 2003 menjadi .xlsx
df_HBAT200 <- read_excel("Multivariate_Data_Analysis_7e_Datasets_EXCEL.xlsx", sheet = "HBAT_200")
df_HBAT200

# dimensi
dim(df_HBAT200)

# isi
glimpse(df_HBAT200)

# isi data secara singkat
head(df_HBAT200)
tail(df_HBAT200)

# ringkasan penyebaran data
summary(df_HBAT200)

# ** CLUSTERING **
#install library yang diperlukan
#install.packages("cluster")
#install.packages("dendextend")
#install.packages("factoextra")
#install.packages("tidyverse")

# call library yang diperlukan
library("cluster")
library("dendextend")
library("factoextra")
library("tidyverse")

# mengambil data yang diperlukan
X6_index <- grep("X6", colnames(df_HBAT200))
X21_index <- grep("X21", colnames(df_HBAT200))
df_clustering <- df_HBAT200[X6_index:X21_index]
head(df_clustering)
write_xlsx(df_clustering, "C:\\Users\\surya\\OneDrive\\Documents\\ADM\\df_clustering.xlsx")

# HIERARKIKAL | NOMOR 1
# distance matrix
dist_mat <- dist(df_clustering)
dist_mat

# membuat cluster agglomeratif
# SINGLE LINKAGE
agglo_single = hclust(dist_mat, "single")
agglo_single
# COMPLETE LINKAGE
agglo_complete = hclust(dist_mat, "complete")
agglo_complete
# AVERAGE LINKAGE
agglo_average = hclust(dist_mat, "average")
agglo_average

# menampilkan dendogram
plot(agglo_single, cex = 0.2, main = "Dendogram Agglomeratif Single", hang = -1)
plot(agglo_complete, cex = 0.2, main = "Dendogram Agglomeratif Complete", hang = -1)
plot(agglo_average, cex = 0.2, main = "Dendogram Agglomeratif Average", hang = -1)

# memotong dendogram menjadi 3 cluster
sub_grpSingle <- cutree(agglo_single, k = 3)
sub_grpComplete <- cutree(agglo_complete, k = 3)
sub_grpAverage <- cutree(agglo_average, k = 3)

# banyak anggota per cluster
table(sub_grpSingle)
table(sub_grpComplete)
table(sub_grpAverage)

# menambakan kolom cluster
df_single <- df_clustering %>% mutate(cluster = sub_grpSingle) %>% head
df_complete <- df_clustering %>% mutate(cluster = sub_grpComplete) %>% head
df_average <- df_clustering %>% mutate(cluster = sub_grpAverage) %>% head

# membagi dendogram menjadi 3 cluster 
rect.hclust(agglo_single, k = 3, border = 2:5)
rect.hclust(agglo_complete, k = 3, border = 2:5)
rect.hclust(agglo_average, k = 3, border = 2:5)

# visualisasi cluster
fviz_cluster(list(data = df_clustering, cluster = sub_grpSingle), main = "Cluster Agglomerative Single Linkage")
fviz_cluster(list(data = df_clustering, cluster = sub_grpComplete), main = "Cluster Agglomerative Complete Linkage")
fviz_cluster(list(data = df_clustering, cluster = sub_grpAverage), main = "Cluster Agglomerative Average Linkage")

# mendapatkan nilai silhouette
sil_cl <- silhouette(cutree(agglo_single, k = 3), dist_mat, title=title(main = 'Good'))
sil_single <- sil_cl[3,3]
sil_cl <- silhouette(cutree(agglo_complete, k = 3), dist_mat, title=title(main = 'Good'))
sil_complete <- sil_cl[3,3]
sil_cl <- silhouette(cutree(agglo_average, k = 3), dist_mat, title=title(main = 'Good'))
sil_average <- sil_cl[3,3]

sil_single
sil_complete
sil_average

# K-MEANS | NOMOR 2
kmeans_clust <- kmeans(df_clustering, centers = 3, nstart = 1)
print(kmeans_clust)

# mendapatkan nilai silhouette
sil_cl <- silhouette(kmeans_clust$cluster, dist_mat)
sil_kmeans <- sil_cl[3,3]

sil_kmeans

# visualisasi cluster
fviz_cluster(kmeans_clust, data = df_clustering, main = "Cluster K-Means")

# * ANALISIS DISKRIMINAN *
#install.packages("mvnormtest")
#install.packages("BART")

# call library yang diperlukan
library("mvnormtest")
library("BART")

# NOMOR 3
# membuat data frame
num_kolom = 7
df_classification = subset(df_HBAT200, select = c(X17, X18, X19, X20, X21, X22, X5))

# membuat matriks korelasi sebagai pertimbangan feature selection
cor_matrix <- cor(df_classification)
cor_matrix <- round(cor_matrix, 2)
cor_matrix
cor_matrix[num_kolom, 1:num_kolom]

# ANALISIS MANOVA
# merubah X5 menjadi tipe factor
df_classification$X5 = as.factor(df_classification$X5)

# pemenuhan asumsi manova
# asumsi normalitas multivariat
# H0: data berdistribusi normal multivariat
# H1: data bukan berdistribusi normal multivariat
mshapiro.test(t(df_classification[1]))
mshapiro.test(t(df_classification[2]))
mshapiro.test(t(df_classification[3]))
mshapiro.test(t(df_classification[4]))
mshapiro.test(t(df_classification[5]))
mshapiro.test(t(df_classification[6]))

# kesamaan matriks kovarian
# H0: tidak terdapat perbedaan matriks kovarian
# H1: terdapat perbedaan matriks kovarian
#result = bartlett.test(X5 ~ interaction(X17, X18, X19, X20, X21, X22), data = df_classification)
#print(result)

# MANOVA
manova_res = manova(cbind(df_classification$X17, df_classification$X18, df_classification$X19, df_classification$X20, df_classification$X21, df_classification$X22) ~ df_classification$X5, data = df_classification)

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
split <- initial_split(df_classification, prop = 0.7, strata = X5)
train <- split %>% training()
test <- split %>% testing()

# * LDA *
#install.packages("MASS")
#install.packages("tidymodels")

# call library yang diperlukan
library("MASS")
library("tidymodels")

# membuat model
lda_model = lda(X5 ~ ., data = train)
lda_model

# prediksi
y_pred = predict(lda_model, test)
names(y_pred) # atribut yang dimiliki
head(y_pred$class) # class hasil prediksi
head(y_pred$posterior) # probabilitas data masuk ke class tersebut
head(y_pred$x) # linear discriminant

# akurasi
acc_lda <- mean(y_pred$class==test$X5)
results_lda <- test %>% bind_cols(y_pred$class, y_pred$posterior)
acc_lda
# Create confusion matrix
conf_matLDA <- conf_mat(results_lda, truth = X5, estimate = ...8)
conf_matLDA

# * QDA *
# membuat model
qda_model = qda(X5~., data=train)
qda_model

# prediksi
y_pred = predict(qda_model, test)
names(y_pred) # atribut yang dimiliki
head(y_pred$class) # class hasil prediksi
head(y_pred$posterior) # probabilitas data masuk ke class tersebut

# akurasi
acc_qda <- mean(y_pred$class==test$X5)
acc_qda
results_qda <- test %>% bind_cols(y_pred$class, y_pred$posterior)

# Create confusion matrix
conf_matQDA <- conf_mat(results_qda, truth = X5, estimate = ...8)
conf_matQDA

# KESIMPULAN
if(acc_lda > acc_qda){
  print("lda memiliki akurasi lebih tinggi")
} else{
  print("qda memiliki akurasi lebih tinggi")
}

# NOMOR 4
# membuat data frame
num_kolom = 7
df_classification = subset(df_HBAT200, select = c(X17, X18, X19, X20, X21, X22, X1))

# membuat matriks korelasi sebagai pertimbangan feature selection
cor_matrix <- cor(df_classification)
cor_matrix <- round(cor_matrix, 2)
cor_matrix
cor_matrix[num_kolom, 1:num_kolom]

# ANALISIS MANOVA
# merubah X5 menjadi tipe factor
df_classification$X1 = as.factor(df_classification$X1)

# pemenuhan asumsi manova
# asumsi normalitas multivariat
# H0: data berdistribusi normal multivariat
# H1: data bukan berdistribusi normal multivariat
mshapiro.test(t(df_classification[1]))
mshapiro.test(t(df_classification[2]))
mshapiro.test(t(df_classification[3]))
mshapiro.test(t(df_classification[4]))
mshapiro.test(t(df_classification[5]))
mshapiro.test(t(df_classification[6]))

# kesamaan matriks kovarian
# H0: tidak terdapat perbedaan matriks kovarian
# H1: terdapat perbedaan matriks kovarian
#result = bartlett.test(X1 ~ interaction(X17, X18, X19, X20, X21, X22), data = df_classification)
# print(result)

# MANOVA
manova_res = manova(cbind(df_classification$X17, df_classification$X18, df_classification$X19, df_classification$X20, df_classification$X21, df_classification$X22) ~ df_classification$X1, data = df_classification)

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
split <- initial_split(df_classification, prop = 0.7, strata = X1)
train <- split %>% training()
test <- split %>% testing()

# * LDA *
# membuat model
lda_model = lda(X1 ~ ., data = train)
lda_model

# prediksi
y_pred = predict(lda_model, test)
names(y_pred) # atribut yang dimiliki
head(y_pred$class) # class hasil prediksi
head(y_pred$posterior) # probabilitas data masuk ke class tersebut
head(y_pred$x) # linear discriminant

# akurasi
acc_lda <- mean(y_pred$class==test$X1)
acc_lda
results_lda <- test %>% bind_cols(y_pred$class, y_pred$posterior)

# Create confusion matrix
conf_matLDA <- conf_mat(results_lda, truth = X1, estimate = ...8)
conf_matLDA

# * QDA *
# membuat model
qda_model = qda(X1~., data=train)
qda_model

# prediksi
y_pred = predict(qda_model, test)
names(y_pred) # atribut yang dimiliki
head(y_pred$class) # class hasil prediksi
head(y_pred$posterior) # probabilitas data masuk ke class tersebut

# akurasi
acc_qda <- mean(y_pred$class==test$X1)
acc_qda
results_qda <- test %>% bind_cols(y_pred$class, y_pred$posterior)

# Create confusion matrix
conf_matQDA <- conf_mat(results_qda, truth = X1, estimate = ...8)
conf_matQDA

# KESIMPULAN
if(acc_lda > acc_qda){
  print("lda memiliki akurasi lebih tinggi")
} else{
  print("qda memiliki akurasi lebih tinggi")
}

acc_qda - acc_lda 

# * CONJOINT ANALYSIS *
#install.packages("conjoint")

# call library yang diperlukan
library("conjoint")

# load dataset yang diperlukan
df_profile <- read_excel("Multivariate_Data_Analysis_7e_Datasets_EXCEL.xlsx", sheet = "HBAT_CPLAN")
df_profile = subset(df_profile, select = -c(CARD_))
df_profile = df_profile[-(23:25),]
df_profile

df_RATING <- read_excel("Multivariate_Data_Analysis_7e_Datasets_EXCEL.xlsx", sheet = "HBAT_CONJOINT")
df_RATING = subset(df_RATING, select = -c(QN))
head(df_RATING)

df_levels <- data.frame(c("mixture1", "mixture2", "mixture3", "numapp1", "numapp2", "numapp3", "germfre1", "germfre2", "bioprot1", "bioprot2", "price1", "price2", "price3", "status1", "status2"))
df_levels

# (menghitung preferensi pada suatu responden)
# model
model <- caModel(df_RATING[1,], df_profile)
model

# importance dari masing-masing factor
importance = caImportance(y = df_RATING, x = df_profile)
importance
barplot(importance)

# (menghitung preferensi dalam skala aggregate)
# part-worths utilities
partutilities = caPartUtilities(y=df_RATING, x = df_profile, z = df_levels)
print(partutilities)

# total utilities
totalutilities=caTotalUtilities(y=df_RATING,x=df_profile)
print(totalutilities)

# order: kecil ke besar
# PROFILE
list_profile <- c()
for(i in 1:22){
  list_profile <- append(list_profile, mean(totalutilities[i]))
}
list_profile
order(list_profile)

# LEVELS
list_level <- c()
for(i in 1:15){
  list_level <- append(list_level, mean(partutilities[i]))
}
list_level
order(list_level)
df_levels
# summary 
#Conjoint(df_RATING[,1:22], df_profile[,1:6], z = df_levels)

# segmentasi customer
segment <- caSegmentation(df_RATING[,1:22], df_profile[,1:6], c=3)

kmeans_clust <- kmeans(segment$util, centers = 3, nstart = 1)
print(kmeans_clust)

fviz_cluster(kmeans_clust, data = segment$util, main = "Segmentasi Customer")

