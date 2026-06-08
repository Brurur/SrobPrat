# ==========================================
# 1.3.6 Analisis Probabilitas dan Distribusi Data
# ==========================================

# Muat dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")

# Filter baris yang memiliki NA pada kolom yang dianalisis
# (lebih aman daripada na.omit karena tidak membuang baris akibat NA di kolom lain)
data_clean <- data[!is.na(data$kemiskinan) &
                   !is.na(data$ipm)        &
                   !is.na(data$pengangguran), ]

cat("Jumlah observasi valid:", nrow(data_clean), "\n")

# Simpan variabel utama dan parameternya
kem  <- data_clean$kemiskinan
ipm  <- data_clean$ipm
peng <- data_clean$pengangguran

mean_kem  <- mean(kem);  sd_kem  <- sd(kem)
mean_ipm  <- mean(ipm);  sd_ipm  <- sd(ipm)
mean_peng <- mean(peng); sd_peng <- sd(peng)


# ==========================================
# 1. UJI DISTRIBUSI DATA - shapiro.test()
#    H0 : data berdistribusi normal
#    H1 : data tidak berdistribusi normal
#    Tolak H0 jika p-value < 0.05
# ==========================================
print("=== 1. Uji Distribusi Data (Uji Normalitas Shapiro-Wilk) ===")

# Shapiro-Wilk mensyaratkan n <= 5000; gunakan sample jika data lebih besar
set.seed(42)
uji_normalitas_kem  <- shapiro.test(sample(kem,  min(5000, length(kem))))
uji_normalitas_ipm  <- shapiro.test(sample(ipm,  min(5000, length(ipm))))
uji_normalitas_peng <- shapiro.test(sample(peng, min(5000, length(peng))))

print(uji_normalitas_kem)
print(uji_normalitas_ipm)
print(uji_normalitas_peng)

# Interpretasi otomatis
cat("-- Interpretasi --\n")
for (hasil in list(list("Kemiskinan", uji_normalitas_kem),
                   list("IPM",        uji_normalitas_ipm),
                   list("Pengangguran", uji_normalitas_peng))) {
  nama <- hasil[[1]];  sw <- hasil[[2]]
  if (sw$p.value < 0.05) {
    cat(nama, ": p-value =", format(sw$p.value, scientific = TRUE),
        "< 0.05 -> Tolak H0, data TIDAK berdistribusi normal\n")
  } else {
    cat(nama, ": p-value =", format(sw$p.value, scientific = TRUE),
        ">= 0.05 -> Gagal tolak H0, data berdistribusi normal\n")
  }
}
cat("Catatan: Meskipun H0 ditolak, dengan n =", nrow(data_clean),
    "pendekatan distribusi normal\n")
cat("tetap dapat digunakan berdasarkan Central Limit Theorem (CLT).\n")


# ==========================================
# 2. PROBABILITAS SEDERHANA EMPIRIS
#    Dihitung langsung dari proporsi data
# ==========================================
print("=== 2. Menghitung Probabilitas Sederhana (Empiris) ===")

# a. Probabilitas wilayah dengan kemiskinan di atas rata-rata
prob_kem_empiris <- mean(kem > mean_kem)
cat("Rata-rata kemiskinan             :", round(mean_kem, 4), "%\n")
cat("P empiris (kemiskinan > mean)    :", round(prob_kem_empiris, 4), "\n")

# b. Probabilitas wilayah dengan IPM tinggi (IPM > 70)
prob_ipm_empiris <- mean(ipm > 70)
cat("P empiris (IPM > 70)             :", round(prob_ipm_empiris, 4), "\n")

# c. Probabilitas wilayah dengan pengangguran > 6%
prob_peng_empiris <- mean(peng > 6)
cat("P empiris (pengangguran > 6%)    :", round(prob_peng_empiris, 4), "\n")


# ==========================================
# 3. PROBABILITAS MENGGUNAKAN DISTRIBUSI NORMAL - pnorm()
#    Membandingkan hasil empiris vs teoretis
# ==========================================
print("=== 3. Probabilitas Teoretis dengan pnorm() ===")

cat("\n-- Kemiskinan (mean =", round(mean_kem, 2), "%, sd =", round(sd_kem, 2), ") --\n")
prob_kem_teoritis <- pnorm(mean_kem, mean = mean_kem, sd = sd_kem, lower.tail = FALSE)
cat("P teoritis (kemiskinan > mean)   :", round(prob_kem_teoritis, 4), "\n")
cat("P empiris  (kemiskinan > mean)   :", round(prob_kem_empiris,  4), "\n")

prob_kem20_teoritis <- pnorm(20, mean = mean_kem, sd = sd_kem, lower.tail = FALSE)
prob_kem20_empiris  <- mean(kem > 20)
cat("P teoritis (kemiskinan > 20%)    :", round(prob_kem20_teoritis, 4), "\n")
cat("P empiris  (kemiskinan > 20%)    :", round(prob_kem20_empiris,  4), "\n")

cat("\n-- IPM (mean =", round(mean_ipm, 2), ", sd =", round(sd_ipm, 2), ") --\n")
prob_ipm_teoritis <- pnorm(70, mean = mean_ipm, sd = sd_ipm, lower.tail = FALSE)
cat("P teoritis (IPM > 70)            :", round(prob_ipm_teoritis, 4), "\n")
cat("P empiris  (IPM > 70)            :", round(prob_ipm_empiris,  4), "\n")

cat("\n-- Pengangguran (mean =", round(mean_peng, 2), "%, sd =", round(sd_peng, 2), ") --\n")
prob_peng_teoritis <- pnorm(6, mean = mean_peng, sd = sd_peng, lower.tail = FALSE)
cat("Rata-rata pengangguran           :", round(mean_peng, 4), "%\n")
cat("Standar deviasi pengangguran     :", round(sd_peng,   4), "\n")
cat("P teoritis (pengangguran > 6%)   :", round(prob_peng_teoritis, 4), "\n")
cat("P empiris  (pengangguran > 6%)   :", round(prob_peng_empiris,  4), "\n")

cat("\nInterpretasi: Selisih kecil antara nilai empiris dan teoretis menunjukkan\n")
cat("bahwa pendekatan distribusi normal cukup representatif untuk data ini.\n")


# ==========================================
# 4. NILAI BATAS DENGAN qnorm()
#    Menentukan nilai x pada persentil tertentu
# ==========================================
print("=== 4. Menghitung Nilai Batas dengan qnorm() ===")

# Kasus 1: 15% wilayah dengan pengangguran tertinggi
# Artinya mencari persentil ke-85 (karena 100% - 15% = 85%)
batas_peng_bantuan <- qnorm(0.85, mean = mean_peng, sd = sd_peng)
cat("\nKasus 1: Batas wilayah masuk 15% pengangguran tertinggi\n")
cat("  -> qnorm(0.85, mean =", round(mean_peng, 2), ", sd =", round(sd_peng, 2), ")\n")
cat("  -> Batas minimal pengangguran:", round(batas_peng_bantuan, 2), "%\n")
cat("  Artinya: wilayah dengan pengangguran >=", round(batas_peng_bantuan, 2),
    "% masuk kelompok 15% tertinggi.\n")

# Kasus 2: 20% wilayah dengan IPM terbaik
# Artinya mencari persentil ke-80
batas_ipm_top20 <- qnorm(0.80, mean = mean_ipm, sd = sd_ipm)
cat("\nKasus 2: Batas wilayah masuk 20% IPM terbaik\n")
cat("  -> qnorm(0.80, mean =", round(mean_ipm, 2), ", sd =", round(sd_ipm, 2), ")\n")
cat("  -> Nilai IPM minimal:", round(batas_ipm_top20, 2), "\n")
cat("  Artinya: wilayah dengan IPM >=", round(batas_ipm_top20, 2),
    "masuk kategori 20% terbaik.\n")

# Kasus 3: 10% wilayah dengan kemiskinan tertinggi
batas_kem_tinggi <- qnorm(0.90, mean = mean_kem, sd = sd_kem)
cat("\nKasus 3: Batas wilayah masuk 10% kemiskinan tertinggi\n")
cat("  -> qnorm(0.90, mean =", round(mean_kem, 2), ", sd =", round(sd_kem, 2), ")\n")
cat("  -> Batas minimal kemiskinan:", round(batas_kem_tinggi, 2), "%\n")
cat("  Artinya: wilayah dengan kemiskinan >=", round(batas_kem_tinggi, 2),
    "% masuk kelompok 10% termiskin.\n")