# ==========================================
# 1.3.6 Analisis Probabilitas dan Distribusi Data
# ==========================================

# 1. Muat dataset terlebih dahulu (pastikan working directory sudah benar)
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")

# Bersihkan data dari missing value terlebih dahulu untuk analisis ini
# (Sebagai contoh, kita buat data frame sementara tanpa NA pada kolom yang diuji)
data_clean <- na.omit(data)

print("=== 1. Uji Distribusi Data (Uji Normalitas) ===")
# Kita akan menguji apakah variabel Indeks Pembangunan Manusia (IPM) terdistribusi normal
# Menggunakan Uji Shapiro-Wilk (P-value > 0.05 berarti data berdistribusi normal)
uji_normalitas_ipm <- shapiro.test(data_clean$ipm)
print(uji_normalitas_ipm)

# Opsional: Melihat distribusinya dengan histogram
# hist(data_clean$ipm, main="Distribusi IPM", xlab="IPM", col="lightblue")


print("=== 2. Menghitung Probabilitas Sederhana (Empiris) ===")
# a. Probabilitas wilayah dengan kemiskinan di atas rata-rata
rata_kemiskinan <- mean(data_clean$kemiskinan)
jumlah_diatas_rata_kemiskinan <- sum(data_clean$kemiskinan > rata_kemiskinan)
total_wilayah <- nrow(data_clean)

prob_kemiskinan_tinggi <- jumlah_diatas_rata_kemiskinan / total_wilayah
cat("Probabilitas empiris wilayah dengan kemiskinan di atas rata-rata:", prob_kemiskinan_tinggi, "\n")

# b. Probabilitas wilayah dengan IPM tinggi (misal: IPM > 70)
jumlah_ipm_tinggi <- sum(data_clean$ipm > 70)
prob_ipm_tinggi <- jumlah_ipm_tinggi / total_wilayah
cat("Probabilitas empiris wilayah dengan IPM > 70:", prob_ipm_tinggi, "\n")


print("=== 3. Menghitung Probabilitas Menggunakan Distribusi Normal ===")
# Menghitung probabilitas pengangguran lebih dari 6% MENGGUNAKAN pnorm()
# Asumsi: data pengangguran mengikuti distribusi normal
rata_pengangguran <- mean(data_clean$pengangguran)
sd_pengangguran <- sd(data_clean$pengangguran)

# pnorm(6, mean, sd) menghitung probabilitas <= 6. 
# Untuk > 6, kita gunakan 1 - pnorm() atau lower.tail = FALSE
prob_pengangguran_lebih_6 <- pnorm(6, mean = rata_pengangguran, sd = sd_pengangguran, lower.tail = FALSE)

cat("Rata-rata Pengangguran:", rata_pengangguran, "\n")
cat("Standar Deviasi Pengangguran:", sd_pengangguran, "\n")
cat("Probabilitas teoretis pengangguran > 6% :", prob_pengangguran_lebih_6, "\n")

print("=== 4. Menghitung Nilai Batas Menggunakan qnorm() ===")

# Contoh Kasus: Pemerintah ingin memberikan bantuan kepada 15% wilayah 
# dengan tingkat pengangguran tertinggi. Berapa batas minimal tingkat 
# pengangguran agar suatu wilayah masuk ke dalam kelompok 15% tersebut?

# Jika 15% tertinggi, berarti kita mencari nilai persentil ke-85 (100% - 15% = 85% atau 0.85)
batas_pengangguran_bantuan <- qnorm(0.85, mean = rata_pengangguran, sd = sd_pengangguran)

cat("Batas minimal pengangguran untuk masuk kelompok 15% tertinggi:", batas_pengangguran_bantuan, "%\n")

# Contoh Kasus 2: Mencari batas nilai IPM untuk 20% wilayah dengan IPM terbaik (persentil ke-80)
rata_ipm_clean <- mean(data_clean$ipm)
sd_ipm_clean <- sd(data_clean$ipm)
batas_ipm_top_20 <- qnorm(0.80, mean = rata_ipm_clean, sd = sd_ipm_clean)

cat("Nilai IPM minimal agar sebuah wilayah masuk dalam kategori 20% terbaik adalah:", batas_ipm_top_20, "\n")
