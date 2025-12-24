
# Amacım SVM uygulaması ile veri seti için güzel bir sınıflandırıcı model oluşturmaktır. Bu modeli oluşturabilmem için de
# öncelikle veri setindeki bağımsız değişkenimin gruplarının bağımlı değişken ya da değişkenlerde ortalamalarının 
# ne kadar farklı olduğunu gözlemleyebilmektir. Ne kadar farklı grup ortalamaları, o kadar görülebilir, 
# ayırt edilebilir sınıflar demektir ve grupların ortalamalarını da plot grafik ile gözlemleyeceğim.
# Hangi bağımlı değişkenlerde grup ortalamalarının farklı olduğunu bulabilmek için de MANOVA uygulamak istedim. 
# Fakat MANOVA için varsayımların sağlanması gerekmektedir. Sağlanmıyorsa başka bir yöntem olan, Box-Plot ile bağımsız
# değişken gruplarının bağımlı değişkenlerde grafiklerine bakılır. Grup ortalamaları en farklı olan değişkenler ele alınabilir.

# Diyelim ki sınıfların ayrımını en iyi şekilde gözleyebildiğin değişkenleri buldun. Ve (linear,redial,polynomal,sigmoid) 
# hyperplane lerinden birine karar verdin. İşte o zaman SVM modelini oluşturmaya başlayabiliriz.

# Bağımsız değişkenimiz = Mission Type
# Bağımlı değişkenler = ...



# VERİ SETİ
# Bu veri seti penguen türlerine ve bu türlere ait bazı özellikleri içerisinde barındıran bir veri setidir.

df <- read.csv("C:/Users/Utku/Downloads/penguins_size.csv")
View(df)
head(df)


# SVM modelinde, sürekli değişken değerleri ile çalışmak daha verimli olduğu için
# yalnızca sınıflandırmayı yapabileceğim sürekli değişkenleri aldım.

table(df$species)

names(df)
library(dplyr)
df <- df %>% select(species,culmen_length_mm,culmen_depth_mm,flipper_length_mm,body_mass_g)
View(df)

# Tür dışında hiçbir değer bulundurmayan eksik gözlemleri çikarttim. Zaten 2 tane vardı.


missing_rows <- c()

for (i in 1:nrow(df)) {
  if (any(is.na(df[i, ]))) {
    missing_rows <- c(missing_rows,i)
  }
}

df <- df[-missing_rows,]
View(df)





######## Birden fazla bağımlı değişkenin grup ortalamaları kontrolü 

# Manova ile bağımsız değişken gruplarının hangi Multi bağımlı değişkenler ile ortalama farklarının yüksek 
# olduğuna bakacağım. Bunun için önce Manova varsayımlarını sağlatmam lazım. 
# (Çok değişkenli normallik, Var-Kov homojenliği, Gözlemlerin bağımsızlığı, Çoklu doğrusal ilişki olmamalı)

# Manova varsayımları kontrolü:
# Normallik, var-kov homojenliği, Çoklu doğrusallık. Biz buradan Normallik ve var-kov homojenliklerini kontrol edeceğiz.


library(tidyverse)
# ------------- Bazı özet istatistikler ------------- 

library(rstatix)
df %>%
  group_by(species) %>%
  get_summary_stats(culmen_length_mm, culmen_depth_mm,
                    flipper_length_mm, body_mass_g,type = "mean_sd")

# ------------- Grupların sayısı------------- 
df %>%
  group_by(species) %>%
  summarise(N = n())


#------------- Aykırı değer ve Normallik testi------------- 

# önce değişkenlerin tek tek histogram grafiklerini çizdirip normal dağılıp dağılmadığına ve aykırı değer olup
# olmadığına bakalım.

# 1. YOL
library(ggplot2)
library(gridExtra)
data <- df[,2:5]

h1 <- ggplot(data,aes(x=culmen_length_mm))+
  geom_histogram(fill="blue",color="black")+
  labs(title = "culmen_length_mm")

h2 <- ggplot(data,aes(x=culmen_depth_mm))+
  geom_histogram(fill="blue",color="black")+
  labs(title = "culmen_depth_mm")

h3 <- ggplot(data,aes(x=flipper_length_mm))+
  geom_histogram(fill="blue",color="black")+
  labs(title = "flipper_length_mm")

h4 <- ggplot(data,aes(x=body_mass_g))+
  geom_histogram(fill="blue",color="black")+
  labs(title = "body_mass_g")


grid.arrange(h1,h2,h3,h4,ncol=2)


# 2. YOL
shapiro.test(data$culmen_length_mm)
shapiro.test(data$culmen_depth_mm)
shapiro.test(data$flipper_length_mm)  # H0: Normallik vardır.
shapiro.test(data$body_mass_g)        # Ha: Yoktur.            H0 reddedilebilir.


# Verilerimiz normal dağılmıyor. Manovanın bir varsayımı sağlanmıyor.







#----------- Var-Kov homojenliği -----------

# var-kov homojenliğine bakabilmemiz için Box-M teski kullanılır.
# install.packages("biotools")
library(biotools)
boxM_result <- boxM(df[2:5], df$species)  # H0: Varyanslar-kov homojendir. 
print(boxM_result)                        # Ha: Varyanslar-kov homojen değildir. H0 RED. 











# Var-kov eşitliği sağlanmadı. Manovanın bir varsayımı daha sağlanmıyor.


# Varsayımlar sağlanmadığı için Mabova kullanılamaz bunun yerine alternatifi yani non-parametriği olan Kruskall-Wallis kullanılır.
# bağımlı değişkenler için tek tek yapılır. Ya da
# Birden fazla bağımsız değişkenin aynı anda non-parametrik Manova test istatistiğine bakmak istiyorsak:
# PERMANOVA kullanılır. Bunu da Adonis2 fonksiyonu ile kullanırız.


## Tek tek Kruskall Wallis
kruskal.test(culmen_length_mm ~ species, data = df)
kruskal.test(culmen_depth_mm ~ species, data = df)
kruskal.test(flipper_length_mm ~ species, data = df)
kruskal.test(body_mass_g ~ species, data = df)


## Çoklu kruskall wallis 

# install.packages("vegan")
library(vegan)

names(df)
# Rank tabanlı uzaklık hesaplama (örneğin Euclidean + rank)
rank_dist <- dist(apply(df[, c("culmen_length_mm", "culmen_depth_mm", "flipper_length_mm", "body_mass_g")],
                        2, rank))  # çoklu bağımlı değişkenler


# Grup değişkenine göre Manovanın alternatifi PERMANOVA
adonis2(rank_dist ~ species, data = df, permutations = 999)  # uzaklık sıraları gruplara göre değişip değişmediğine bakıldı ve 
                                                            # H0: Bağımlı değişkenlerin uzaklıkları gruplara göre değişmiyor.
                                                            # Ha: Değişiyor. H0 reddedilebilir. Türler arasındaki farklar çok anlamlı. 


disp <- betadisper(rank_dist, df$species) 
TukeyHSD(disp)    # Chinstrap-Adelie gruplarının bağımsız değişken değerlerine göre istatistiksel olarak anlamlı bir farklılık yoktur.


########## Bu sebepledir ki hangi bağımsız değişkenleri SVM modelimizde kullanırsak kullanalım %100 bir ayrım yakalayamayacağız.




#----------- BOX-Plot ile grup ort. kontrolü -----------
names(df)

library(ggpubr)
g1 <- ggboxplot(
  df, x = "species", y = c("culmen_length_mm"), 
  merge = TRUE, color = "blue")

g2 <- ggboxplot(
  df, x = "species", y = c("culmen_depth_mm"), 
  merge = TRUE, color = "orange")

g3 <- ggboxplot(
  df, x = "species", y = c("flipper_length_mm"), 
  merge = TRUE, color = "lightblue")

g4 <- ggboxplot(
  df, x = "species", y = c("body_mass_g"), 
  merge = TRUE, color = "red")

grid.arrange(g1,g2,g3,g4,ncol=2)


# Burada görmek istediğim şey bütün bağımlı değişkenlere karşılık gelen için bağımsız değişken gruplarının 
# ortalamalarının farklı olmasıdır.
# Burada hepsi birbirine yakın. Bu durumlarda plot grafikler ile sınıf ayrımını gözlemleyebilmek oldukça zor.

# Eğer grup ortalamalarının farklı olduğu tek bağımlı değişken görebilseydim. Tek bağımlı değişken üzerinden 
# plot grafik incelemesi yapıp sınıflandırma yapacağım.
# Eğer grup ortalamalarının farklı olduğu multi değişken görebilseydim. Multi bağımsız değişken üzerinden
# plot grafik incelemesi yapıp sınıflandırma yapacağım.











####### Tek değişkenlerin plot grafikleri 

#------------- Culmen Uzunluğu -------------

# Görev süresi değişkeni değerlerimizin. görev tiplerine göre plot grafiğini çizdirdim.
plot(df$culmen_length_mm,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)]) # lineer, polinomal, redia ya da sigmoid ilişki gözükmüyor.



# Görev süresi değişkenimiz ve bu değişkenin karesini alarak 2 boyuta çıkarttım ve exp bir artış yakalayarak 
# görev tiplerine göre görev yılları arasında görünür bir ayrım yakalamaya çalışıyorum. 
culmen2 <- (df$culmen_length_mm)**2

plot(df$culmen_length_mm,culmen2,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)]) # Burada da lineer, polinomal, redia ya da sigmoid ilişki gözükmüyor.



# Bu sefer 3D incelemek için bu değişkenin küpünü de alıyorum. Böylelikle 3 boyutlu epx bir grafik yapalamaya çalışıyorum.
culmen3 <- c(1:nrow(df))

library(plotly)
plot_ly(
  x = df$culmen_length_mm,
  y = culmen2,
  z = culmen3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)

#------------- Culmen derinliği -------------

# Tek boyutlu
plot(df$culmen_depth_mm,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])

# iki boyutlu
Culmend2 <- (df$culmen_depth_mm)**2

plot(df$culmen_depth_mm,Culmend2,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])

# üç boyutlu
Culmend3 <- c(1:nrow(df))

library(plotly)
plot_ly(
  x = df$culmen_depth_mm,
  y = Culmend2,
  z = Culmend3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)




#------------- Yüzgeç Uzunluğu -------------

# Tek boyut
plot(df$flipper_length_mm,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])

# İki boyut
flipper2 <- (df$flipper_length_mm)**2

plot(df$flipper_length_mm,flipper2,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])

# 3 boyut
flipper3 <- c(1:nrow(df))

library(plotly)
plot_ly(
  x = df$flipper_length_mm,
  y = flipper2,
  z = flipper3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)



# ------------- Vücut Kütlesi -------------

plot(df$body_mass_g,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])


body2 <- (df$body_mass_g)**2

plot(df$body_mass_g,body2,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])


body3 <- c(1:nrow(df))

library(plotly)
plot_ly(
  x = df$body_mass_g,
  y = body2,
  z = body3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)





######### Multi değişkenlerin plot grafiği SVM uygulamak için 

# ------------- Culmen Uzunluğu ve Culmen Derinliği -------------

# Tek boyutlu
plot(df$culmen_length_mm,df$culmen_depth_mm,pch=19,
     col=c("blue","orange","pink")[as.factor(df$species)])  


# Üç boyutlu
Mculmen3 <- c(1:nrow(df))
library(plotly)

plot_ly(
  x = df$culmen_length_mm,
  y = df$culmen_depth_mm,
  z = Mculmen3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)
# ------------- Culmen Uzunluğu ve Yüzgeç Uzunluğu ------------- !!!!!!!!

plot(df$culmen_length_mm,df$flipper_length_mm,pch=19,
     col=c("blue","orange","pink")[as.factor(df$species)])  


# Üç boyutlu
Mculmen3 <- c(1:nrow(df))
library(plotly)

plot_ly(
  x = df$culmen_length_mm,
  y = df$flipper_length_mm,
  z = Mculmen3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)


# ------------- Yüzgeç uzunluğu ve Vücut Kütlesi -------------

# Tek boyutlu
plot(df$flipper_length_mm,df$body_mass_g,pch=19,
     col=c("blue","orange","pink")[as.factor(df$species)]) 

# Üç boyutlu
library(plotly)

plot_ly(
  x = df$flipper_length_mm,
  y = df$body_mass_g,
  z = 1:nrow(df),
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)












# Model oluşturma

library(caret)
library(e1071)
library(caTools)
library(ggplot2)

df$species <- as.factor(df$species)
class(df$species)

table(df$species)


# Train ve Test veri setlerine ayırma işlemi
set.seed(312)
TrainIndex <- sample(1:nrow(df), size= 0.8*nrow(df))

TrainSet <- df[TrainIndex,]
TestSet <- df[-TrainIndex,]

table(TrainSet$species)
table(TestSet$species)




# culmen_length_mm ve flipper_length_mm
# Model 1
library(e1071)
library(ggplot2)

# Model 1
SVM11 <- svm(species ~ culmen_length_mm + flipper_length_mm, data = TrainSet, kernel = "linear")
summary(SVM11)
SVM11$SV
plot(SVM11, TrainSet, formula = culmen_length_mm ~ flipper_length_mm)

# Linear Model
# Tahminler
predLinear1 <- predict(SVM11, TestSet)
predLinear1
# Tahmin değerlerimiz ile gerçek değerlerimizin karşılaştırılması
caret::confusionMatrix(predLinear1, TestSet$species)
# F1 skoru eklemek için
caret::confusionMatrix(predLinear1, TestSet$species, mode="prec_recall")



# Model 2
SVM12 <- svm(species ~ culmen_length_mm + flipper_length_mm, data = TrainSet, kernel = "radial")
summary(SVM12)
SVM12$SV
plot(SVM12, TrainSet, formula = culmen_length_mm ~ flipper_length_mm)

# Radial Model
# Tahminler
predRedial1 <- predict(SVM12, TestSet)
# Tahmin değerlerimiz ile gerçek değerlerimizin karşılaştırılması
caret::confusionMatrix(predRedial1, TestSet$species)
# F1 skoru eklemek için
caret::confusionMatrix(predRedial1, TestSet$species,mode="prec_recall")







# culmen_length_mm ve culmen_depth_mm 

# Model 1
SVM21 <- svm(species ~ culmen_length_mm + culmen_depth_mm, data = TrainSet, kernel = "linear")
summary(SVM21)
SVM21$SV

plot(SVM21, TrainSet, formula = culmen_length_mm ~ culmen_depth_mm)

# Model üzerinden tahminler
predLinear2 <- predict(SVM21, TestSet)
predLinear2
# Tahmin değerlerimiz ile gerçek değerlerimizin karşılaştırılması
caret::confusionMatrix(predLinear2, TestSet$species)
# F1 skoru eklemek için
caret::confusionMatrix(predLinear2, TestSet$species, mode="prec_recall")



# Model 2
SVM22 <- svm(species ~ culmen_length_mm + culmen_depth_mm, data = TrainSet, kernel = "radial")
summary(SVM22)
SVM22$SV
plot(SVM22, TrainSet, formula = culmen_length_mm ~ culmen_depth_mm)


# Model üzerinden tahminler
predRedial2 <- predict(SVM22, TestSet)
# Tahmin değerlerimiz ile gerçek değerlerimizin karşılaştırılması
caret::confusionMatrix(predRedial2, TestSet$species)
# F1 skoru eklemek için
caret::confusionMatrix(predRedial2, TestSet$species,mode="prec_recall")







# MODEL TUNİNG

# culmen_length_mm ve flipper_length_mm modellerinin Tuning İşlemleri (Optimal Gamma ve C parametreleri)

SVM11Tune <- tune(svm, species ~ culmen_length_mm + flipper_length_mm , data = TrainSet, 
                  kernel = "linear",
                  ranges = list(cost= 2^(-4:2)),               # linear karnel da gamma parametresi olmaz.
                  tunecontrol = tune.control(cross = 5))

SVM12Tune <- tune(svm, species ~ culmen_length_mm + flipper_length_mm , data = TrainSet, 
                  kernel = "radial",
                  ranges = list(gamma = 2^(-2:2), cost= 2^(-4:2)),
                  tunecontrol = tune.control(cross = 5))

SVM11Tune
SVM12Tune


SVM11Tune$performances # Görünürde En az hata veren parametreler, cost= 1
SVM12Tune$performances # Görünürde En az hata veren parametreler, gamma= 4, cost= 4
# Bulduğumuz bu parametreler ile tahmin değerleri elde edeceğiz.

SVM11Tune$best.model  # Modelimizde 56 adet support vector elde edilmiş
SVM12Tune$best.model  # Modelimizde 90 adet support vector elde edilmiş


# predict
predLinearTune1 <- predict(SVM11Tune$best.model, TestSet)
predRedialTune1 <- predict(SVM12Tune$best.model, TestSet)

caret::confusionMatrix(predLinearTune1, TestSet$species)
caret::confusionMatrix(predRedialTune1, TestSet$species)
# optimal gamma ve c parametrelerini elde ettikten sonraki model tahmin sonuçlarının iyiliği ile
# optimal parametre değerlerini bulmadan önceki yapılan predict değerleri hemen hemen aynı iylikte.








# culmen_length_mm ve culmen_depth_mm Modellerinin Tuning İşlemleri (Optimal Gamma ve C parametreleri)
library(e1071)

SVM21Tune <- tune(svm, species ~ culmen_length_mm + culmen_depth_mm , data = TrainSet, 
                 kernel = "linear",
                 ranges = list(cost= 2^(-4:2)),  # linear karnel da gamma parametresi olmaz.
                 tunecontrol = tune.control(cross = 5))

SVM22Tune <- tune(svm, species ~ culmen_length_mm + culmen_depth_mm , data = TrainSet, 
                 kernel = "radial",
                 ranges = list(gamma = 2^(-2:2), cost= 2^(-4:2)),
                 tunecontrol = tune.control(cross = 5))

SVM21Tune
SVM22Tune


SVM21Tune$performances # En az hata veren parametreler, cost= 1
SVM22Tune$performances # En az hata veren parametreler, gamma=0.5, cost=0.25
# Bulduğumuz bu parametreler ile tahmin değerleri elde edeceğiz.

SVM21Tune$best.model  # Modelimizde 41 adet support vector elde edilmiş
SVM22Tune$best.model  # Modelimizde 57 adet support vector elde edilmiş


# predict
predLinearTune2 <- predict(SVM21Tune$best.model, TestSet)
predRedialTune2 <- predict(SVM22Tune$best.model, TestSet)

caret::confusionMatrix(predLinearTune2, TestSet$species)
caret::confusionMatrix(predRedialTune2, TestSet$species)
# optimal gamma ve c parametrelerini elde ettikten sonraki model tahmin sonuçlarının iyiliği ile
# optimal parametre değerlerini bulmadan önceki yapılan predict değerleri hemen hemen aynı iylikte.

















































######### Decision Tree 

# Bu veri setimiz için SVM uyguladım iyi sonuçlar elde ettim. Fakat SVM uygularken yalnızca sürekli bağımsız değişkenleri
# kullandım. Şimdi ise decision tree ile bütün bağımsımsız değişkenlerin kullanıldığı bir model oluşturmak istiyorum.
# Bunun için öncelikle karakter türüne sahip olan değişkenlerimi factore çevireceğim ya da dummy değişkenler elde edeceğim.
# Daha sonrasında decision tree modeli uygulamaya koyulabilirim.

df <- read.csv("C:/Users/Utku/Downloads/penguins_size.csv")
View(df)
names(df)


# Öncelikle ben SVM ile güzel bir sınıflandırma modeli elde ettim. Fakat Decision tree ile daha iyi veya aynı kalitede bir sınıflandırma modeli yapılabilir mi buna bakıyorum. 
# Decision tree kullanmak istememin sebebi sınıflandırma modelime SVM deki gibi yalnızca sürekli değişkenleri değil, cinsiyet ve lokasyon kategorik değişkenlerini de
# hesaba katmak istiyorum. Fakat bunun için YALNIZCA kategorik değişkenlerimde rslaycağım NA değerli satırları tespit edip bu NA değerlerine atamalar yapmak istiyorum.
# (neredeyse tüm değerleri NA olan gözlemleri kastetmiyorum.) O NA değerlerini doldurayım ki iyi bir decision tree modeli elde edebileyim.

# NA değerlerini nasıl dolduracağım ? SVM modeli ile elde ettiğim sınıflandırma modelimi kullanarak.

# Kayıp gözlem var mı?
library(mice)
md.pattern(df)

na <- is.na(df$sex)
table(na)



# Sürekli değişkenlere karşılık gelen satırlarda bütün değerleri na olanlar var ise onları tespit edeceğim. Ve veriden atacağım.

numeric_col <- sapply(df, is.numeric)
numeric_col

numeric_data <- df[,numeric_col]
numeric_data

na_rows <- apply(numeric_data, 1, function(x) all(is.na(x)))
na_rows

df_cleaned <- df[!na_rows,]

df_cleaned
View(df_cleaned)

# artık elimde yalnızca kategorik değişkenlerdeki na değerleri kaldı. Bunlara düzgün atamalar yapacağım.
library(dplyr)
df_cleaned <- df_cleaned %>%
  mutate(across(where(is.character), ~na_if(., ".")))

table(df_cleaned$sex)
View(df_cleaned)

sum(is.na(df$sex)) # 10 adet NA değerimiz var.




######## NA değeri olan ve olmayan satırlar olarak veriyi ikiye ayıracağım. 
# Amacım, önceden türlerin sınıflandırması için oluşturduğum SVM modelini cinsiyet için oluşturacağım ki buna göre, NA değerlerine bir atama yapabileyim.

df_cleaned_train <- df_cleaned[!is.na(df_cleaned$sex), ]  # Sınıfı bilinenler
df_cleaned_train
df_cleaned_test <- df_cleaned[is.na(df_cleaned$sex), ]    # Sınıfı bilinmeyenler
df_cleaned_test


library(e1071)
names(df_cleaned)
df_cleaned_train$sex <- as.factor(df_cleaned_train$sex)

model <- svm(sex ~ culmen_length_mm + culmen_length_mm + flipper_length_mm + body_mass_g, 
             data= df_cleaned_train,
             karnel="radial")

summary(model)

pred <- predict(model, df_cleaned_test)
pred


library(caret)
caret::confusionMatrix(pred, df_cleaned_test$sex)














# NA değerleri doldurulmuş olan kategorik değişkenlerim için Dummy encoding oluşturacağım bunun için 
# model.matrix fonksiyonu işimi görmez onun yerine dummy_cols fonksyionunu kullanmam daha pratik.

#install.packages("fastDummies")
library(fastDummies)
df <- fastDummies::dummy_columns(df, select_columns = c("island","sex"), remove_selected_columns = T)
View(df)

# kategorik tüm kategorik değişkenleri factore çevireceğim.
ifelse()

# 
df$island <- as.factor(df$island)
df$island
table(df$island)

class(df$island)
