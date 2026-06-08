data <- read.csv(
  "Dataset/pembangunan_wilayah_missing_outlier.csv"
  )

# cek missing value missing value
is.na(data)

# Menghitung jumlah missing value tiap variabel
colSums(is.na(data))

jumlah_missing <- colSums(is.na(data))

#Missing value ada di kolom
#"pdrb_perkapita"
#"kemiskinan"
#"pengangguran"
#"ipm"
#"akses_internet"
#"jalan_baik"

jumlah_missing

# Persentase missing value
persen_missing <- (jumlah_missing / nrow(data)) * 100

persen_missing

data.frame(
  Variabel = names(jumlah_missing),
  Missing = jumlah_missing,
  Persentase = round(persen_missing, 2)
)

#Dampak missing value:
# Adanya missing value dapat mengurangi jumlah sample efektif yang didapat,
# menurunkan akurasi sebuah model, dan dapat menyebabkan error.

#Metode yang dipilih;
data_median <- data

numerik <- c(
  "pdrb_perkapita",
  "kemiskinan",
  "pengangguran",
  "ipm",
  "akses_internet",
  "jalan_baik"
)

for(i in numerik){
  data_median[[i]][is.na(data_median[[i]])] <-
    median(data_median[[i]], na.rm = TRUE)
}

data_median

#verifikasi

colSums(is.na(data_median))

data_median
