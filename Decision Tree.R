
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

table(df_cleaned$sex)
View(df_cleaned)

# kategorik değişkenimizdeki na değerleri dışında bir tane . değerimiz var. Ona na değerini vereceğim.
library(dplyr)
df_cleaned <- df_cleaned %>%
  mutate(across(where(is.character), ~na_if(., ".")))

table(df_cleaned$sex)
View(df_cleaned)

sum(is.na(df_cleaned$sex)) # Artık elimizde 9 adet NA değerimiz var.

# Yalnızca kategorik değişkenlerdeki na değerleri kaldı. Bunlara düzgün atamalar yapacağım. 







# veri setimizden bazı satırları çıkartıtğım için df_cleaned label larında karışıklı olmaması için sıralı bir şekilde ilerlemesini istedim.
rownames(df_cleaned) <- NULL
View(df_cleaned)













######## NA değeri olan ve olmayan satırlar olarak veriyi ikiye ayıracağım. 
# Ayırdığım bu iki veri setinde yani (sex sınıfları bilinenler ve bilinmeyenler)
# Sex sınıfları bilinen veri setimi de kendi içinde Train ve test olarak ayıracağım ki.
# elde edeceğim tahmin modelimi test edebileyim. Test ettiğim güzel modelimi de 
# sınıfları bilinmeyen cinsiyet veri setimdeki değişken değerleri tahmini için kullanabileyim.


df_cleaned_know_sex <- df_cleaned[!is.na(df_cleaned$sex), ]  # Sex lerin Sınıfı bilinen veri
df_cleaned_know_sex
nrow(df_cleaned_know_sex)

df_cleaned_unknow_sex <- df_cleaned[is.na(df_cleaned$sex), ]    # Sex lerin Sınıfı bilinmeyen veri NA lı olanlar
df_cleaned_unknow_sex
nrow(df_cleaned_unknow_sex)


Train_Index <- sample(1:nrow(df_cleaned_know_sex), size = 0.75*nrow(df_cleaned_know_sex))
Train_set <- df_cleaned_know_sex[Train_Index,]
Train_set
nrow(Train_set)
str(Train_set)


Test_set <- df_cleaned_know_sex[-Train_Index,]
Test_set
nrow(Test_set)
str(Test_set)


Train_set$sex <- as.factor(Train_set$sex)
Test_set$sex <- as.factor(Test_set$sex)


# SVM modeli
library(e1071)
names(df_cleaned)

df_cleaned_know_sex$sex <- as.factor(df_cleaned_know_sex$sex)
df_cleaned_know_sex$sex

names(df_cleaned_know_sex)

model <- svm(sex ~ culmen_length_mm + culmen_depth_mm + flipper_length_mm + body_mass_g, 
             data= Train_set,
             kernel="radial")


summary(model)
model$fitted


str(Train_set)
str(Test_set)

levels(Train_set$sex)
levels(Test_set$sex)


pred_know_sex <- predict(model, Test_set)
pred_know_sex

library(caret)
caret::confusionMatrix(pred_know_sex, Test_set$sex) # %88, %91 doğrulukla svm sınıflandırma modeli ile 
# sex değişkeni değerlerimizin bilindiği veri seti ile oluşturdum ve testini yaptım.






#################### SVM
#################### SİMDİ İSE Sex DEGİSKENİ DEGERLERİ BİLİNMEYEN VERİ SETİMDEKİ Sex DEGİSKENİ DEGERLERİNİ % 88 ile % 95 DOGRULUK oranı aralıklarına sahip tahminler ile değerler atayacağım.


# Sex sınıfları bilinmeyen veri setimdeki sex değişkenlerinin değerlerinin hepsi na olduğu için 
# Sex sınıfları bilinem veri setimdeki gibi faktöre çevirdim ve level larını aynı yaptım ki 
# modelim neyi tahmin edeceğini bilsin.

df_cleaned_unknow_sex$sex <- factor(df_cleaned_unknow_sex$sex, levels = levels(df_cleaned_know_sex$sex))
levels(df_cleaned_unknow_sex$sex)
levels(df_cleaned_know_sex$sex)

# Sex sınıfları bilinmeyen veri setimizdeki tahmin edilecek değişkenimin değerlerinin hepsi na olduğu için 
# o sex değişkenimizi çıkartıp bir tahmin elde edeceğim.
# Tahmin edilecek değişkeni çıkartıp neye göre tahmin yapılır utku ne diyorsun diyebilirsin fakat, bu yapmamın sebebi
# önceden oluşturduğum modeller ile test veri setinde tahmin yapıtığım değerlerimin hepsi na değildi. Sebebi bu olabilir.

df_test_input <- df_cleaned_unknow_sex[, c("culmen_length_mm","culmen_depth_mm", "flipper_length_mm", "body_mass_g")]

pred <- predict(model, df_test_input)
pred
# % 89 oranında kayip yani na gozlemlerimiz tahmin ettigimiz sekilde.









######################### GUNCEL VERİ SETİMİZİ TOPARLAYALIM. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

View(df_cleaned)
table(df_cleaned$sex)

pred
which(is.na(df_cleaned$sex))

# gercek veri setimizdeki na degerlerinin nerede oldugunu gorup pred ile elde ettigim degerleri atayalım

library(dplyr)

# Önce NA olan satırların indeksini al
na_index <- which(is.na(df_cleaned$sex))
na_index

# Bu na değerli index lere 1="Female" değeri ve 2="Male" değerleri atandı.
df_cleaned$sex[na_index] <- pred
df_cleaned


# Bu döngü ile de 1 yeine FEMALE 2 YERİNE MALE YAZMASINI İSTEDİM.
for (i in colnames(df_cleaned)) {
  if (i == "sex") {
    df_cleaned[[i]][df_cleaned[[i]] == 1] <- "FEMALE"
    df_cleaned[[i]][df_cleaned[[i]] == 2] <- "MALE"
  }
}

df_cleaned
View(df_cleaned)
























# Şimdi artık verimiz gayet temiz. Bu temiz veri üzerinden Decision Tree ve Random Forest ile penguenlerin türlerini tahmin edeceğim.

# Bunun için öncelikle kategorik değişkenlerim için Dummy encoding ya da label encoding yöntemleri kullanacağım. Bunun için 
# model.matrix fonksiyonu işimi görmez onun yerine dummy_cols fonksyionunu kullanmam daha pratik. 
# Bu sayede Penguen türlerinin tahmini için güzel bir zemin hazırlamış olacağım.






# Train ve Test veri setleri

library(rpart)

Train_Index <- sample(1:nrow(df_cleaned), size = 0.75*nrow(df_cleaned))

Train_set <- df_cleaned[Train_Index,]
Train_set

Test_set <- df_cleaned[-Train_Index,]
Test_set

table(Train_set$species)
table(Test_set$species)


Train_set$species <- as.factor(Train_set$species)
Test_set$species <- as.factor(Test_set$species)




# Decision Tree model oluşturma

# Entropy Model
ModelEntropy <- rpart(species ~ . ,data = Train_set, method = "class",
                      parms = list(split= "information"))
ModelEntropy

# Entropy model detayları
summary(ModelEntropy)


# Entropy parametresi ile yapılan modelin görselleştirilmesi
library(rattle)
fancyRpartPlot(ModelEntropy)


# ModelEntropy prediction
predModelEntropy <- predict(ModelEntropy,Test_set,type = "class") 
predModelEntropy

# Confusion Matrix
caret::confusionMatrix(predModelEntropy,Test_set$species)
