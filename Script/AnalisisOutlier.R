# Analisis oulier
# Outlier -> data yang aneh/berbeda dari rata rata data(q1-q3)
# Jadi Analisis outlier berfungsi untuk mendeteksi apakah ada data yang "anomali" / berbeda sendiri
# Nah fungsinya agar kita bisa memutuskan apakah datanya mau dibuang, diperbaiki atau harus diselidiki lagi

# Dampak outlier terhadap analisis statistik 
# 1. merusak nilai rata-rata -> misalkan ada 5 orang, 
# orang 1 bawa 4 buah, orang 2 5 buah, orang 3 5 buah, orang 4 4 buah. orang 5 100 buah
# nah orang kelima ini ngebuat rata-ratanya jadi 20an padahal kenyataannya harusnya 4-5 doang dia sendiri yang kejauhan/anomali 
# 2. mengganggu nilai standar deviasi
# standar deviasi itu mengukur penyebaran data, kalau ada outlier berarti datanya keliatan sangat renggang
# statistiknya akan menganggap datanya tidak stabil/tidak konsisten.

data <- read.csv(
  "Dataset/pembangunan_wilayah_missing_outlier.csv"
)
library(dplyr)
#===============================================
# BOXPLOT & IQR
#===============================================
# fungsi iqr 
hitung_iqr <- function(data_input, nama_kolom, nama_variabel) {
  # Ambil data kolom secara dinamis dari dataset utama
  data_kolom <- data_input[[nama_kolom]]
  
  # Q1, Q3, dan IQR
  q1 <- quantile(data_kolom, 0.25, na.rm = TRUE)
  q3 <- quantile(data_kolom, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  
  # batas atas dan bawah
  batas_bawah <- q1 - (1.5 * iqr_value)
  batas_atas <- q3 + (1.5 * iqr_value)
  
  # jumlah outlier
  jumlah_outlier_atas <- sum(data_kolom > batas_atas, na.rm = TRUE)
  jumlah_outlier_bawah <- sum(data_kolom < batas_bawah, na.rm = TRUE)
  
  # Cetak Hasil Ringkasan Statistik
  cat("===================================================\n")
  cat("=== ANALISIS IQR:", nama_variabel, "===\n")
  cat("===================================================\n")
  cat("Kuartil 1 (Q1)       :", q1, "\n")
  cat("Kuartil 3 (Q3)       :", q3, "\n")
  cat("Nilai IQR            :", iqr_value, "\n")
  cat("Batas Normal Bawah   :", batas_bawah, "\n")
  cat("Batas Normal Atas    :", batas_atas, "\n")
  cat("Jumlah Outlier Atas  :", jumlah_outlier_atas, "data\n")
  cat("Jumlah Outlier Bawah :", jumlah_outlier_bawah, "data\n")
  cat("---------------------------------------------------\n")
  
  # Filter data oknum outlier
  outlier_data <- data_input %>% 
    filter(!!sym(nama_kolom) > batas_atas | !!sym(nama_kolom) < batas_bawah) %>% 
    select(provinsi, tahun, !!sym(nama_kolom)) %>% 
    arrange(desc(!!sym(nama_kolom))) # Urutkan dari yang paling ekstrem
  
  # Cetak jumlah total outlier
  cat("Total Outlier Ditemukan:", nrow(outlier_data), "data\n")
  cat("---------------------------------------------------\n")
  
  # --- 3. TAMPILKAN DAFTAR OKNUM OUTLIER ---
  if(nrow(outlier_data) > 0) {
    cat("Daftar Data Outlier (Menampilkan hingga 10 teratas):\n")
    print(head(outlier_data, 10))
  } else {
    cat("Tidak ada data yang menjadi outlier.\n")
  }
  cat("\n\n")
}

# memilih data yang ingin ditampilkan (numeric aja)
df_indikator <- data %>% 
  select(pdrb_perkapita, kemiskinan, pengangguran, ipm, 
         harapan_hidup, rata_lama_sekolah, akses_internet, jalan_baik, air_bersih)

# bagi menjadi 3 baris 3 kolom
par(mfrow=c(3,3), mar=c(2, 2, 2, 1))

# buat boxplotnya sesuai tipenya masing-masing 
for(i in 1:ncol(df_indikator)) {
  boxplot(df_indikator[[i]], main=colnames(df_indikator)[i], col="skyblue")
}

# IQR untuk menghitung dan mencari siapa outliernya
hitung_iqr(data, "pdrb_perkapita", "PDRB PER KAPITA")
hitung_iqr(data, "kemiskinan", "KEMISKINAN")
hitung_iqr(data, "pengangguran", "PENGANGGURAN")

# Memisahkan dataset utama menjadi 5 dataset mandiri
# data 2020
data_2020 <- data %>% filter(tahun == 2020) 
data_2020_number <- data_2020 %>% 
  select(pdrb_perkapita, kemiskinan, pengangguran, ipm, 
         harapan_hidup, rata_lama_sekolah, akses_internet, jalan_baik, air_bersih)
par(mfrow = c(3, 3), mar = c(2, 2, 3, 1), oma = c(0, 0, 4, 0))

for(i in 1:ncol(data_2020_number)) {
  boxplot(data_2020_number[[i]], main = colnames(data_2020)[i], col = "green")
}
mtext("ANALISIS OUTLIER INDIKATOR DAERAH TAHUN 2020", 
      side = 3,
      outer = TRUE,   
      cex = 1,  
      font = 2)
hitung_iqr(data_2020,"pdrb_perkapita", "PDRB PER KAPITA Tahun 2020")
hitung_iqr(data_2020,"kemiskinan", "KEMISKINAN Tahun 2020")
hitung_iqr(data_2020,"pengangguran", "PENGANGGURAN Tahun 2020")

# data 2021
data_2021 <- data %>% filter(tahun == 2021)  
data_2021_number <- data_2021%>% 
  select(pdrb_perkapita, kemiskinan, pengangguran, ipm, 
         harapan_hidup, rata_lama_sekolah, akses_internet, jalan_baik, air_bersih)
par(mfrow = c(3, 3), mar = c(2, 2, 3, 1), oma = c(0, 0, 4, 0))

for(i in 1:ncol(data_2021_number)) {
  boxplot(data_2021_number[[i]], main=colnames(data_2021)[i], col="red")
}
mtext("ANALISIS OUTLIER INDIKATOR DAERAH TAHUN 2021", 
      side = 3,
      outer = TRUE,   
      cex = 1,  
      font = 2)
hitung_iqr(data_2021,"pdrb_perkapita", "PDRB PER KAPITA Tahun 2021")
hitung_iqr(data_2021,"kemiskinan", "KEMISKINAN Tahun 2021")
hitung_iqr(data_2021,"pengangguran", "PENGANGGURAN Tahun 2021")

# data 2022
data_2022 <- data %>% filter(tahun == 2022) 
data_2022_number <- data_2022 %>% 
  select(pdrb_perkapita, kemiskinan, pengangguran, ipm, 
         harapan_hidup, rata_lama_sekolah, akses_internet, jalan_baik, air_bersih)
data_2022
par(mfrow = c(3, 3), mar = c(2, 2, 3, 1), oma = c(0, 0, 4, 0))
for(i in 1:ncol(data_2022_number)) {
  boxplot(data_2022_number[[i]], main=colnames(data_2022)[i], col="purple")
}
mtext("ANALISIS OUTLIER INDIKATOR DAERAH TAHUN 2022", 
      side = 3,
      outer = TRUE,   
      cex = 1,  
      font = 2)
hitung_iqr(data_2022,"pdrb_perkapita", "PDRB PER KAPITA Tahun 2022")
hitung_iqr(data_2022,"kemiskinan", "KEMISKINAN Tahun 2022")
hitung_iqr(data_2022,"pengangguran", "PENGANGGURAN Tahun 2022")

# data 2023
data_2023 <- data %>% filter(tahun == 2023)
data_2023_number <- data_2023 %>% 
  select(pdrb_perkapita, kemiskinan, pengangguran, ipm, 
         harapan_hidup, rata_lama_sekolah, akses_internet, jalan_baik, air_bersih)
data_2023_number
par(mfrow = c(3, 3), mar = c(2, 2, 3, 1), oma = c(0, 0, 4, 0))
for(i in 1:ncol(data_2023_number)) {
  boxplot(data_2023_number[[i]], main=colnames(data_2023)[i], col="yellow")
}
mtext("ANALISIS OUTLIER INDIKATOR DAERAH TAHUN 2023", 
      side = 3,
      outer = TRUE,   
      cex = 1,  
      font = 2)
hitung_iqr(data_2023,"pdrb_perkapita", "PDRB PER KAPITA Tahun 2023")
hitung_iqr(data_2023,"kemiskinan", "KEMISKINAN Tahun 2023")
hitung_iqr(data_2023,"pengangguran", "PENGANGGURAN Tahun 2023")

# data 2024
data_2024 <- data %>% filter(tahun == 2024) 
data_2024_number <- data_2024 %>% 
  select(pdrb_perkapita, kemiskinan, pengangguran, ipm, 
         harapan_hidup, rata_lama_sekolah, akses_internet, jalan_baik, air_bersih)
par(mfrow = c(3, 3), mar = c(2, 2, 3, 1), oma = c(0, 0, 4, 0))
for(i in 1:ncol(data_2024_number)) {
  boxplot(data_2024_number[[i]], main=colnames(data_2024)[i], col="blue")
}
mtext("ANALISIS OUTLIER INDIKATOR DAERAH TAHUN 2024", 
      side = 3,
      outer = TRUE,   
      cex = 1,  
      font = 2)
hitung_iqr(data_2024,"pdrb_perkapita", "PDRB PER KAPITA Tahun 2024")
hitung_iqr(data_2024,"kemiskinan", "KEMISKINAN Tahun 2024")
hitung_iqr(data_2024,"pengangguran", "PENGANGGURAN Tahun 2024")


