#Veri Üreterek Keşifsel Veri Analizi

#VERİ KAYNAĞI
# Rastgele veri oluşturma için kütüphane yükleniyor
install.packages("MASS")
library(MASS)


# 100 gözlem ve 8 sütuna sahip rastgele bir veri seti oluşturma
veri <- matrix(data = rnorm(100*8), nrow = 100, ncol = 8)

# Sütun başlıkları oluşturuluyor
colnames(veri) <- paste0("S", 1:8)

# Bağımlı bir değişken oluşturma
katsayilar <- c(3, -2, 0.5, 2, 1.8, -3, 2.3, -1)
y <- 5 + rowSums(veri * katsayilar)

# Veri Çerçevesi oluşturuluyor
yeni_veri <- data.frame(veri, Y = y)

# Veriyi görüntüleme
print(yeni_veri)

# Her bir sütundaki verilerin özeti
summary(yeni_veri)

# Her bir sütundaki verilerin yapısı
str(yeni_veri)


#AKTARIM ve DÖNÜŞÜM
# Sütunları farklı türlere dönüştürme
veri_tur <- data.frame(veri)

str(veri_tur)

veri_tur$S1 <- as.character(veri_tur$S1)
veri_tur$S2 <- as.factor(veri_tur$S2)
veri_tur$S3 <- as.logical(veri_tur$S3)
veri_tur$S4 <- as.character(veri_tur$S4)
veri_tur$S5 <- as.factor(veri_tur$S5)
veri_tur$S6 <- as.double(veri_tur$S6)
veri_tur$S7 <- as.character(veri_tur$S7)
veri_tur$S8 <- as.factor(veri_tur$S8)

str(veri_tur)


# VERİ KALİTESİ
#veri seti ile veri_kal veri setine oluşturuyoruz
veri_kal <- data.frame(veri) 

# S2 ve S5 sütunlarındaki bazı verileri döngü ile null yapıyoruz.
# veri setinde eksik veri olmadığı için bazı verileri null yapıyoruz.

for (i in 1:nrow(veri_kal)) {
  if (i %% 10 == 0) {  # her 10. gözlemi null yap
    veri_kal$S2[i] <- NA
    veri_kal$S5[i] <- NA
  }
}

print(veri_kal)

# Her sütundaki eksik değerleri listeleyen bir fonksiyon tanımlama
listele_eksik_degerler <- function(veriseti) {
  eksik_satirlar <- apply(veriseti, 2, function(x) which(is.na(x)))
  print(eksik_satirlar)
}

# Eksik değerleri listeleyen fonksiyonu çağırma
print("Her Sütundaki Eksik Değerler:")
listele_eksik_degerler(veri_kal)

# Eksik değerleri ortalama ile doldurma
veri_kal[] <- lapply(veri_kal, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Doldurulmuş veri setini görüntüleme
print("Doldurulmuş Veri Seti:")
print(veri_kal)


#AYKIRI DEĞER TESPİTİ
boxplot(veri_kal)

# Aykırı değerleri listeleyen fonksiyon tanımlama
listele_aykiri_degerler <- function(veriseti) {
  for (col in names(veriseti)) {
    if (is.numeric(veriseti[[col]])) {  # Sayısal sütunları seçme
      aykiri_degerler <- boxplot.stats(veriseti[[col]])$out
      print(paste("Sütun:", col))
      print(aykiri_degerler)
    }
  }
}

# Aykırı değerleri listeleme
print("Tüm Sütunlardaki Aykırı Değerler:")
listele_aykiri_degerler(veri_kal)

#DAĞILIMLARI KEŞİF

# Önceki satırlarda oluşturduğumuz yeni_veri veri setini başka bir veri setine aktarıyoruz
veri_dagilim <- yeni_veri

# Histogram - Y sütunu için
hist(veri_dagilim$Y, main="Y Değişkeninin Histogramı", xlab="Y Değerleri", ylab="Frekans", col="lightblue")

# Boxplot - Y sütunu için
boxplot(veri_dagilim$Y, main="Y Değişkeninin Boxplot'u", ylab="Y Değerleri", col="lightgreen")

# Q-Q plot
qqnorm(veri_dagilim$Y)
qqline(veri_dagilim$Y)

# Korelasyon matrisi - Bağımlı değişken "Y" ile bağımsız değişkenler arasındaki korelasyon katsayısını hesaplar
cor_matrix <- cor(veri_dagilim[, -ncol(veri_dagilim)], veri_dagilim$Y)
print(cor_matrix)

# Multicollineraty - Çoklu Bağlantı Analiz
model <- lm(Y ~ ., data = veri_dagilim)
summary(model)
# S1 ve S6 değişkenlerinin bağımlı değişken üzerinde anlamlı bir etkiye sahip olma olasılığı yüksektir.


# Bağımsız değişkenler arasındaki korelasyon 
cor_matrix_bagimsiz <- cor(veri_dagilim[, -ncol(veri_dagilim)])
print(cor_matrix_bagimsiz)
install.packages("corrplot") # corplot paketini yükler
library(corrplot)
corrplot(cor_matrix_bagimsiz, method = "color") # Korelasyon matrisini görselleştirin

# Bağımsız değişkenlerin grafiğini çizmek
par(mfrow=c(2,4)) # 2x4'lik bir grafik penceresi oluştur
colors <- rainbow(ncol(veri_dagilim)-1) # Farklı renkler üretmek için renk paleti
for (i in 1:(ncol(veri_dagilim)-1)) { # Y değişkenini hariç tut
  plot(veri_dagilim[,i], veri_dagilim$Y, main=paste(colnames(veri_dagilim)[i], "& Y"),
       xlab=paste(colnames(veri_dagilim)[i], "Ekseni"), ylab="Y Değeri", col=colors[i], pch=20)
}
# Bağımlı değişken Y S2 ile negatif yönde, diğerleri ile pozitif yönde orantılıdır.

# Veri ölçekleme (normalleştirme) - bağımsız değişkenlerin değerlerini normalleştirir
normalized_data <- scale(veri_dagilim[, -ncol(veri_dagilim)]) # Y -bağımlı değişken- hariç
colnames(normalized_data) <- paste0("Scaled_", colnames(veri_dagilim[, -ncol(veri_dagilim)]))
summary(normalized_data)


#SON DEĞİŞİKLİKLER

# caret paketini yükleme
install.packages("caret")
library(caret)

# Veri setini veri_bol'e kopyala
veri_bol <- yeni_veri

# Veri setini eğitim ve test verisi olarak bölmek için indeksleri oluştur
set.seed(42) # Rastgelelik için bir tohum belirle
indeksler <- createDataPartition(veri_bol$Y, p = 0.7, list = FALSE)

# Eğitim ve test verilerini oluştur
egitim_verisi <- veri_bol[indeksler, ]
test_verisi <- veri_bol[-indeksler, ]

#verinin boyutları kontrol ediliyor
dim(egitim_verisi)
dim(test_verisi)

View(egitim_verisi)
View(test_verisi)

#**********************************

# VERİYİ ÇEKEREK

# Gerekli kütüphanelerin yüklenmesi
# install.packages("MASS")
# install.packages("caret")
# install.packages("corrplot")
# library(MASS)
# library(caret)
# library(corrplot) Bu kütüphaneler önceki satırlarda yüklendi

# VERİ KAYNAĞI
# Boston veri setini yükleme
boston <- read.csv("boston.csv", header = TRUE)

# Veri setinin yapısını ve ilk gözlemlerini kontrol etme
str(boston)
head(boston)

# Boston veri setini yükleme
boston <- read.csv("boston.csv", header = TRUE, fileEncoding = "UTF-8")

# Başlıkları Türkçe olarak değiştirme
colnames(boston) <- c("Sehir", "Bolge", "Boylam", "Enlem", "Ort_medyan_deger", "Suc_oranı", 
                      "imar_alanı", "Yakınlık_nehir", "Hava_kalitesi", "Ort_oda_sayısı", 
                      "Bina_yası", "Merkeze_uzaklık", "Otoyol_erisebilirlik", "Emlak_vergisi", "Ogretmen_ogrenci_oranı")

# Veri setinin yapısını ve ilk gözlemlerini kontrol etme
str(boston)
head(boston)

# Veri Çerçevesi oluşturuluyor
bst <- data.frame(Sehir = boston$Sehir,
                  Suc_oranı = boston$Suc_oranı,
                  Yakınlık_nehir = boston$Yakınlık_nehir,
                  Ort_oda_sayısı = boston$Ort_oda_sayısı,
                  Bina_yası = boston$Bina_yası,
                  Merkeze_uzaklık = boston$Merkeze_uzaklık,
                  Otoyol_erisebilirlik = boston$Otoyol_erisebilirlik,
                  Emlak_vergisi = boston$Emlak_vergisi)

str(bst) # Veri yapısı
summary(bst) # Veri özeti

# AKTARIM VE DÖNÜŞÜM

# Emlak_vergisi double yapılıyor
bst$Emlak_vergisi <- as.double(bst$Emlak_vergisi)

str(bst) # Veri yapısı
summary(bst) # Veri özeti

# VERİ KALİTESİ

# Eksik değerlerin kontrolü ve doldurulması
missing_values <- sapply(bst, function(x) sum(is.na(x)))# Her sütun için eksik değerlerin sayısını bulma
total_missing <- sum(missing_values)# Toplam eksik değer sayısı
print(missing_values)# Eksik değerlerin gösterimi
print(total_missing)# Toplam eksik değer sayısının gösterimi

# veri setinde eksik veri olmadığı için bazı verileri null yapıyoruz.
null_percentage <- 0.1  # NULL yapılacak gözlemlerin yüzdesi
set.seed(42)  # Rastgelelik için seed belirleme

# Yakınlık_nehir sütunu için NULL işlemi
null_indices_nehir <- sample(1:nrow(bst), size = round(null_percentage * nrow(bst)))
for (index in null_indices_nehir) {
  bst$Yakınlık_nehir[index] <- NA
}

# Merkeze_uzaklık sütunu için NULL işlemi
null_indices_uzaklik <- sample(1:nrow(bst), size = round(null_percentage * nrow(bst)))
for (index in null_indices_uzaklik) {
  bst$Merkeze_uzaklık[index] <- NA
}

# Her sütundaki eksik değerleri listeleyen bir fonksiyon tanımlama
listele_eksik_degerler <- function(veriseti) {
  eksik_satirlar <- apply(veriseti, 2, function(x) which(is.na(x)))
  print(eksik_satirlar)
}

# Eksik değerleri listeleyen fonksiyonu çağırma
print("Her Sütundaki Eksik Değerler:")
listele_eksik_degerler(bst)

# NULL değerleri sütun medyanı ile doldurma
bst$Yakınlık_nehir[is.na(bst$Yakınlık_nehir)]  <- median(bst$Yakınlık_nehir, na.rm = TRUE)
bst$Merkeze_uzaklık[is.na(bst$Merkeze_uzaklık)] <- median(bst$Merkeze_uzaklık, na.rm = TRUE)

# Doldurulmuş veri setini görüntüleme
print("Doldurulmuş Veri Seti:")
print(bst)


#AYKIRI DEĞER TESPİTİ
boxplot(bst$Emlak_vergisi) # Emlak_vergisi sütunun kutu grafiğini göster

# Aykırı değerleri listeleyen fonksiyon tanımlama
listele_aykiri_degerler <- function(veriseti) {
  for (col in names(veriseti)) {
    if (is.numeric(veriseti[[col]])) {  # Sayısal sütunları seçme
      aykiri_degerler <- boxplot.stats(veriseti[[col]])$out
      print(paste("Sütun:", col))
      print(aykiri_degerler)
    }
  }
}

# Aykırı değerleri listeleme
print("Tüm Sütunlardaki Aykırı Değerler:")
listele_aykiri_degerler(bst)

#DAĞILIMLARI KEŞİF

# Emlak_vergisi değişkeni histogramı
hist(bst$Emlak_vergisi, main = "Emlak Vergisi Dağılımı", xlab = "Emlak Vergisi", ylab = "Frekans", col="lightblue")

# Emlak_vergisi sütunu için boxplot çizimi
boxplot(bst$Emlak_vergisi, main = "Emlak Vergisi Kutu Grafiği", ylab = "Emlak Vergisi", col="lightgreen")

# Q-Q plot
qqnorm(bst$Emlak_vergisi)
qqline(bst$Emlak_vergisi)

# Korelasyon matrisi oluşturma
correlation_matrix <- cor(bst[, sapply(bst, is.numeric)])

# Korelasyon matrisini yazdırma
print(correlation_matrix)

# Multicollineraty - Çoklu Bağlantı Analiz
model <- lm(Emlak_vergisi ~ .- Sehir, data = bst)
summary(model)

# modeldeki bağımsız değişkenlerin bazılarının Emlak_vergisi'ni açıklamada anlamlı olduğu görülüyor 
# Suc_oranı ve Yakınlık_nehir ve Ort_oda_sayısı değişkenleri son derece anlamlıdır çünkü 
# p-değerleri 0.001'den daha küçüktür. 
# Ancak Bina_yası, Merkeze_uzaklık ve Otoyol_erisebilirlik değişkenleri için p-değerleri belirtilen 
# eşik değerlerden büyük olduğu için istatistiksel olarak anlamlı değildir.

# Bağımsız değişkenlerin bulunduğu alt veri çerçevesini oluşturma (Sehir sütununu çıkararak)
independent_vars <- subset(bst, select = -c(Sehir, Emlak_vergisi))
correlation_matrix <- cor(independent_vars)# Korelasyon matrisini hesaplama
print(correlation_matrix)# Korelasyon matrisini yazdırma

# install.packages("corrplot") # önceki kod satırlarında yüklendi
# library(corrplot) # önceki kod satırlarında yüklendi

# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")

# Bağımsız değişkenler arasındaki ilişkiyi gösteren grafiklerin çizimi
par(mfrow=c(2,3)) # 2x3'lik bir grafik penceresi oluştur
plot(bst$Suc_oranı, bst$Emlak_vergisi,main="Suc_oranı & Emlak_vergisi", xlab = "Suc_oranı", ylab = "Emlak_vergisi", col="blue", pch=20)
plot(bst$Yakınlık_nehir, bst$Emlak_vergisi,main="Yakınlık_nehir & Emlak_vergisi", xlab = "Yakınlık_nehir", ylab = "Emlak_vergisi", col="green", pch=20)
plot(bst$Ort_oda_sayısı, bst$Emlak_vergisi,main="Ort_oda_sayısı & Emlak_vergisi", xlab = "Ort_oda_sayısı", ylab = "Emlak_vergisi", col="yellow", pch=20)
plot(bst$Bina_yası, bst$Emlak_vergisi,main="Bina_yası & Emlak_vergisi", xlab = "Bina_yası", ylab = "Emlak_vergisi", col="red", pch=20)
plot(bst$Merkeze_uzaklık, bst$Emlak_vergisi,main="Merkeze_uzaklık & Emlak_vergisi", xlab = "Merkeze_uzaklık", ylab = "Emlak_vergisi", col="orange", pch=20)
plot(bst$Otoyol_erisebilirlik , bst$Emlak_vergisi,main="Otoyol_erisebilirlik  & Emlak_vergisi", xlab = "Otoyol_erisebilirlik ", ylab = "Emlak_vergisi", col="brown", pch=20)
# Emlak_vergisi değişkeni ile Suc_oranı, Yakınlık_nehir ve Ort_oda_sayısı arasında pozitif yönde
# ilişki bulunmakta diğerleri ile anlamlı bir ilişki bulunmamaktadır.

# Veri ölçekleme (normalleştirme) - bağımsız değişkenlerin değerlerini normalleştirir
# Sayısal değerlere sahip olan sütunları seçme
numeric_columns <- sapply(bst[, -ncol(bst)], is.numeric)

# Sayısal değerlere sahip olan sütunlar üzerinde normalizasyon yapma
normalized_data <- scale(bst[, numeric_columns])

# Normalleştirilmiş sütunlara uygun isimler verme
colnames(normalized_data) <- paste0("Scaled_", colnames(bst[, numeric_columns]))

# Normalleştirilmiş verilerin özet istatistiklerini görüntüleme
summary(normalized_data)


#SON DEĞİŞİKLİKLER

# caret paketini yükleme
# install.packages("caret")
# library(caret) Bu kütüphaneler önceki satırlarda yüklendi

# Veri setini eğitim ve test verisi olarak bölmek için indeksleri oluştur
set.seed(42) # Rastgelelik için bir tohum belirle
# verinin %70'i Eğitim veri olacak şekilde bölünüyor
indeksler <- createDataPartition(bst$Emlak_vergisi, p = 0.7, list = FALSE)

# Eğitim ve test verilerini oluştur
egitim_verisi <- bst[indeksler, ]
test_verisi <- bst[-indeksler, ]

#verinin boyutları kontrol ediliyor
dim(egitim_verisi)
dim(test_verisi)

View(egitim_verisi)
View(test_verisi)