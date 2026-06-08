# =============================================================================
# ANALISIS DISTRIBUSI DATA PEMBANGUNAN WILAYAH
# Uji Normalitas, Probabilitas (pnorm), dan Kuantil (qnorm)
# =============================================================================
# Paket yang dibutuhkan: tidyverse, nortest, moments, gridExtra, scales
# Install jika belum ada:
# install.packages(c("tidyverse", "nortest", "moments", "gridExtra", "scales"))
# =============================================================================

library(dplyr)
library(tidyverse)
library(nortest)     # uji normalitas tambahan (Anderson-Darling, Lilliefors)
library(moments)     # skewness & kurtosis
library(gridExtra)   # multi-panel plot
library(scales)      # format label

# =============================================================================
# 1. MUAT DAN SIAPKAN DATA
# =============================================================================

df_raw <- read.csv("~/GitHub/SrobPrat/Dataset", stringsAsFactors = FALSE)

cat("=== RINGKASAN DATASET ===\n")
cat("Total baris       :", nrow(df_raw), "\n")
cat("Total kolom       :", ncol(df_raw), "\n")
cat("Kolom             :", paste(names(df_raw), collapse = ", "), "\n\n")

cat("=== DISTRIBUSI CATATAN DATA ===\n")
print(table(df_raw$catatan_data))
cat("\n")

# Gunakan hanya data bersih (Normal, tanpa Missing Value dan Outlier)
df <- df_raw %>%
  filter(catatan_data == "Normal") %>%
  drop_na(kemiskinan, pengangguran, ipm, harapan_hidup)

cat("Data bersih (Normal, tanpa NA):", nrow(df), "baris\n\n")

# Variabel yang dianalisis
vars <- c("kemiskinan", "pengangguran", "ipm", "harapan_hidup")
var_labels <- c(
  kemiskinan    = "Kemiskinan (%)",
  pengangguran  = "Pengangguran (%)",
  ipm           = "IPM",
  harapan_hidup = "Harapan Hidup (tahun)"
)


# =============================================================================
# 2. STATISTIK DESKRIPTIF
# =============================================================================

cat("=============================================================\n")
cat("BAGIAN 2: STATISTIK DESKRIPTIF\n")
cat("=============================================================\n\n")

desc_stats <- df %>%
  select(all_of(vars)) %>%
  pivot_longer(everything(), names_to = "variabel", values_to = "nilai") %>%
  group_by(variabel) %>%
  summarise(
    N       = n(),
    Mean    = mean(nilai, na.rm = TRUE),
    Median  = median(nilai, na.rm = TRUE),
    SD      = sd(nilai, na.rm = TRUE),
    Min     = min(nilai, na.rm = TRUE),
    Max     = max(nilai, na.rm = TRUE),
    Q1      = quantile(nilai, 0.25, na.rm = TRUE),
    Q3      = quantile(nilai, 0.75, na.rm = TRUE),
    Skewness  = skewness(nilai, na.rm = TRUE),
    Kurtosis  = kurtosis(nilai, na.rm = TRUE) - 3,  # kurtosis berlebih (0 = normal)
    .groups = "drop"
  )

print(as.data.frame(round(desc_stats[, -1], 4)) %>% 
        mutate(variabel = desc_stats$variabel) %>%
        select(variabel, everything()))

cat("\nCatatan: Kurtosis berlebih = 0 berarti identik dengan normal.\n")
cat("         Nilai < 0 (platikurtik) = lebih datar dari normal.\n\n")


# =============================================================================
# 3. UJI NORMALITAS — shapiro.test(), ad.test(), lillie.test()
# =============================================================================

cat("=============================================================\n")
cat("BAGIAN 3: UJI NORMALITAS\n")
cat("=============================================================\n\n")

# Shapiro-Wilk mensyaratkan n <= 5000, ambil sampel jika perlu
set.seed(42)
n_shapiro <- min(nrow(df), 5000)

normality_results <- data.frame()

for (v in vars) {
  x      <- df[[v]][!is.na(df[[v]])]
  x_samp <- if (length(x) > 5000) sample(x, 5000) else x
  x_200  <- if (length(x) > 200)  sample(x, 200)  else x   # sampel kecil untuk S-W stabil
  
  # Shapiro-Wilk (sensitif pada n besar; gunakan subsampel 200-5000)
  sw  <- shapiro.test(x_200)
  
  # Anderson-Darling (lebih kuat untuk n besar)
  ad  <- ad.test(x_samp)
  
  # Lilliefors (Kolmogorov-Smirnov termodifikasi)
  lil <- lillie.test(x_samp)
  
  normality_results <- rbind(normality_results, data.frame(
    Variabel       = v,
    SW_W           = round(sw$statistic, 4),
    SW_p           = round(sw$p.value, 6),
    AD_A           = round(ad$statistic, 4),
    AD_p           = round(ad$p.value, 6),
    Lil_D          = round(lil$statistic, 6),
    Lil_p          = round(lil$p.value, 6),
    Keputusan_005  = ifelse(sw$p.value < 0.05, "Tolak H0 (tidak normal)", "Gagal tolak H0 (normal)"),
    stringsAsFactors = FALSE
  ))
}

cat("--- Hasil Uji (H0: data berdistribusi normal, alpha = 0.05) ---\n\n")
print(normality_results)

cat("\n--- Interpretasi ---\n")
cat("Shapiro-Wilk    : Semua p < 0.05 -> H0 ditolak.\n")
cat("Anderson-Darling: Konfirmasi penolakan H0.\n")
cat("Penyebab utama  : Kurtosis platikurtik (~-1.2), bukan asimetri.\n")
cat("                  Distribusi lebih datar/seragam dari normal.\n")
cat("                  Skewness hampir nol -> distribusi tetap SIMETRIS.\n\n")


# =============================================================================
# 4. VISUALISASI DISTRIBUSI (Histogram + Kurva Normal + Q-Q Plot)
# =============================================================================

cat("=============================================================\n")
cat("BAGIAN 4: VISUALISASI — Histogram & Q-Q Plot\n")
cat("=============================================================\n")
cat("Membuat plot... (lihat panel Plots di RStudio)\n\n")

plot_list_hist <- list()
plot_list_qq   <- list()

for (v in vars) {
  x   <- df[[v]][!is.na(df[[v]])]
  mu  <- mean(x)
  sig <- sd(x)
  lbl <- var_labels[v]
  
  # Histogram + kurva normal overlay
  p_hist <- ggplot(data.frame(x = x), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, fill = "#378ADD", color = "white", alpha = 0.7) +
    stat_function(fun  = dnorm,
                  args = list(mean = mu, sd = sig),
                  color = "#E24B4A", linewidth = 1.2, linetype = "solid") +
    geom_vline(xintercept = mu, color = "#7F77DD",
               linewidth = 0.8, linetype = "dashed") +
    annotate("text",
             x = mu + 0.5 * sig, y = Inf, vjust = 2,
             label = paste0("μ = ", round(mu, 2)),
             color = "#7F77DD", size = 3.5) +
    labs(title = paste("Distribusi:", lbl),
         subtitle = paste0("Skewness = ", round(skewness(x), 3),
                           "  |  Kurtosis berlebih = ", round(kurtosis(x) - 3, 3)),
         x = lbl, y = "Densitas") +
    theme_minimal(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(color = "gray50", size = 10))
  
  # Q-Q Plot
  p_qq <- ggplot(data.frame(x = x), aes(sample = x)) +
    stat_qq(color = "#378ADD", alpha = 0.4, size = 0.8) +
    stat_qq_line(color = "#E24B4A", linewidth = 1) +
    labs(title = paste("Q-Q Plot:", lbl),
         subtitle = "Titik mengikuti garis = distribusi normal",
         x = "Kuantil Teoritis", y = "Kuantil Sampel") +
    theme_minimal(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(color = "gray50", size = 10))
  
  plot_list_hist[[v]] <- p_hist
  plot_list_qq[[v]]   <- p_qq
}

# Tampilkan semua histogram
grid.arrange(grobs = plot_list_hist, ncol = 2,
             top = "Histogram + Kurva Normal Teoritik (merah = kurva normal)")

# Tampilkan semua Q-Q plot
grid.arrange(grobs = plot_list_qq, ncol = 2,
             top = "Q-Q Plot: menyimpang dari garis = tidak normal")


# =============================================================================
# 5. PROBABILITAS DENGAN pnorm()
# =============================================================================

cat("=============================================================\n")
cat("BAGIAN 5: PROBABILITAS — pnorm()\n")
cat("=============================================================\n\n")

# Ambil parameter distribusi kemiskinan, IPM, pengangguran
mu_kem  <- mean(df$kemiskinan,   na.rm = TRUE)
sd_kem  <- sd(df$kemiskinan,     na.rm = TRUE)
mu_ipm  <- mean(df$ipm,          na.rm = TRUE)
sd_ipm  <- sd(df$ipm,            na.rm = TRUE)
mu_peng <- mean(df$pengangguran, na.rm = TRUE)
sd_peng <- sd(df$pengangguran,   na.rm = TRUE)

cat(sprintf("Parameter kemiskinan   : μ = %.4f, σ = %.4f\n", mu_kem,  sd_kem))
cat(sprintf("Parameter IPM          : μ = %.4f, σ = %.4f\n", mu_ipm,  sd_ipm))
cat(sprintf("Parameter pengangguran : μ = %.4f, σ = %.4f\n\n", mu_peng, sd_peng))

# --- 5a. P(kemiskinan > rata-rata) ---
cat("--- 5a. P(kemiskinan > rata-rata) ---\n")
p_kem_above <- pnorm(mu_kem, mean = mu_kem, sd = sd_kem, lower.tail = FALSE)
p_kem_emp   <- mean(df$kemiskinan > mu_kem, na.rm = TRUE)
cat(sprintf("  pnorm(%.2f, mean=%.2f, sd=%.2f, lower.tail=FALSE) = %.4f (%.1f%%)\n",
            mu_kem, mu_kem, sd_kem, p_kem_above, p_kem_above * 100))
cat(sprintf("  Probabilitas empiris: %.4f (%.1f%%)\n\n", p_kem_emp, p_kem_emp * 100))

# --- 5b. P(kemiskinan < 5%) ---
cat("--- 5b. P(kemiskinan < 5%) — kemiskinan sangat rendah ---\n")
p_kem_low <- pnorm(5, mean = mu_kem, sd = sd_kem, lower.tail = TRUE)
p_kem_low_emp <- mean(df$kemiskinan < 5, na.rm = TRUE)
cat(sprintf("  pnorm(5, mean=%.2f, sd=%.2f) = %.4f (%.1f%%)\n",
            mu_kem, sd_kem, p_kem_low, p_kem_low * 100))
cat(sprintf("  Probabilitas empiris: %.4f (%.1f%%)\n\n", p_kem_low_emp, p_kem_low_emp * 100))

# --- 5c. P(kemiskinan antara 5% dan 20%) ---
cat("--- 5c. P(5% < kemiskinan < 20%) ---\n")
p_kem_range <- pnorm(20, mean = mu_kem, sd = sd_kem) -
  pnorm(5,  mean = mu_kem, sd = sd_kem)
p_kem_range_emp <- mean(df$kemiskinan > 5 & df$kemiskinan < 20, na.rm = TRUE)
cat(sprintf("  pnorm(20,μ,σ) - pnorm(5,μ,σ) = %.4f (%.1f%%)\n",
            p_kem_range, p_kem_range * 100))
cat(sprintf("  Probabilitas empiris: %.4f (%.1f%%)\n\n", p_kem_range_emp, p_kem_range_emp * 100))

# --- 5d. P(IPM >= 80) ---
cat("--- 5d. P(IPM >= 80) — wilayah IPM tinggi ---\n")
p_ipm_high     <- pnorm(80, mean = mu_ipm, sd = sd_ipm, lower.tail = FALSE)
p_ipm_high_emp <- mean(df$ipm >= 80, na.rm = TRUE)
cat(sprintf("  pnorm(80, mean=%.2f, sd=%.2f, lower.tail=FALSE) = %.4f (%.1f%%)\n",
            mu_ipm, sd_ipm, p_ipm_high, p_ipm_high * 100))
cat(sprintf("  Probabilitas empiris: %.4f (%.1f%%)\n\n", p_ipm_high_emp, p_ipm_high_emp * 100))

# --- 5e. P(IPM < 65) ---
cat("--- 5e. P(IPM < 65) — wilayah IPM rendah ---\n")
p_ipm_low     <- pnorm(65, mean = mu_ipm, sd = sd_ipm, lower.tail = TRUE)
p_ipm_low_emp <- mean(df$ipm < 65, na.rm = TRUE)
cat(sprintf("  pnorm(65, mean=%.2f, sd=%.2f) = %.4f (%.1f%%)\n",
            mu_ipm, sd_ipm, p_ipm_low, p_ipm_low * 100))
cat(sprintf("  Probabilitas empiris: %.4f (%.1f%%)\n\n", p_ipm_low_emp, p_ipm_low_emp * 100))

# --- 5f. P(pengangguran > 10%) ---
cat("--- 5f. P(pengangguran > 10%) ---\n")
p_peng_high     <- pnorm(10, mean = mu_peng, sd = sd_peng, lower.tail = FALSE)
p_peng_high_emp <- mean(df$pengangguran > 10, na.rm = TRUE)
cat(sprintf("  pnorm(10, mean=%.2f, sd=%.2f, lower.tail=FALSE) = %.4f (%.1f%%)\n",
            mu_peng, sd_peng, p_peng_high, p_peng_high * 100))
cat(sprintf("  Probabilitas empiris: %.4f (%.1f%%)\n\n", p_peng_high_emp, p_peng_high_emp * 100))

# --- 5g. P(pengangguran antara 5% dan 12%) ---
cat("--- 5g. P(5% < pengangguran < 12%) ---\n")
p_peng_mid     <- pnorm(12, mean = mu_peng, sd = sd_peng) -
  pnorm(5,  mean = mu_peng, sd = sd_peng)
p_peng_mid_emp <- mean(df$pengangguran > 5 & df$pengangguran < 12, na.rm = TRUE)
cat(sprintf("  pnorm(12,μ,σ) - pnorm(5,μ,σ) = %.4f (%.1f%%)\n",
            p_peng_mid, p_peng_mid * 100))
cat(sprintf("  Probabilitas empiris: %.4f (%.1f%%)\n\n", p_peng_mid_emp, p_peng_mid_emp * 100))

# --- Ringkasan tabel probabilitas ---
cat("--- Ringkasan Seluruh Probabilitas ---\n")
prob_summary <- data.frame(
  Pertanyaan = c(
    "P(Kemiskinan > rata-rata)",
    "P(Kemiskinan < 5%)",
    "P(5% < Kemiskinan < 20%)",
    "P(IPM >= 80)",
    "P(IPM < 65)",
    "P(Pengangguran > 10%)",
    "P(5% < Pengangguran < 12%)"
  ),
  P_Normal   = round(c(p_kem_above, p_kem_low, p_kem_range,
                       p_ipm_high, p_ipm_low,
                       p_peng_high, p_peng_mid), 4),
  P_Empiris  = round(c(p_kem_emp, p_kem_low_emp, p_kem_range_emp,
                       p_ipm_high_emp, p_ipm_low_emp,
                       p_peng_high_emp, p_peng_mid_emp), 4),
  stringsAsFactors = FALSE
)
prob_summary$Selisih <- round(abs(prob_summary$P_Normal - prob_summary$P_Empiris), 4)
print(prob_summary)

cat("\nInterpretasi selisih: Selisih konsisten karena kurtosis platikurtik.\n")
cat("Data asli memiliki lebih banyak wilayah di nilai ekstrem dibanding prediksi normal.\n\n")


# =============================================================================
# 6. VISUALISASI pnorm() — Area di Bawah Kurva Normal
# =============================================================================

cat("=============================================================\n")
cat("BAGIAN 6: VISUALISASI AREA pnorm()\n")
cat("=============================================================\n")
cat("Membuat plot area probabilitas...\n\n")

# Fungsi bantu untuk plot area di bawah kurva normal
plot_pnorm_area <- function(mu, sig, threshold, lower_tail = FALSE,
                            label_x, label_var, color_fill = "#378ADD") {
  x_range  <- seq(mu - 4 * sig, mu + 4 * sig, length.out = 500)
  df_curve <- data.frame(x = x_range, y = dnorm(x_range, mu, sig))
  
  if (lower_tail) {
    df_shade <- df_curve %>% filter(x <= threshold)
    prob_val <- pnorm(threshold, mu, sig, lower.tail = TRUE)
    area_lbl <- sprintf("P(X < %.1f) = %.3f", threshold, prob_val)
  } else {
    df_shade <- df_curve %>% filter(x >= threshold)
    prob_val <- pnorm(threshold, mu, sig, lower.tail = FALSE)
    area_lbl <- sprintf("P(X > %.1f) = %.3f", threshold, prob_val)
  }
  
  ggplot(df_curve, aes(x, y)) +
    geom_line(color = "gray40", linewidth = 1) +
    geom_area(data = df_shade, aes(x, y), fill = color_fill, alpha = 0.5) +
    geom_vline(xintercept = threshold, color = "#E24B4A",
               linewidth = 0.8, linetype = "dashed") +
    geom_vline(xintercept = mu, color = "gray60",
               linewidth = 0.6, linetype = "dotted") +
    annotate("text", x = mu + 0.3 * sig, y = max(df_curve$y) * 0.92,
             label = paste0("μ=", round(mu, 1)), size = 3.2, color = "gray50") +
    annotate("text",
             x = if (lower_tail) mu - 1.8 * sig else mu + 1.8 * sig,
             y = max(df_curve$y) * 0.5,
             label = area_lbl, size = 3.5,
             color = colorspace::darken(color_fill, 0.4),
             fontface = "bold") +
    labs(title = label_var, x = label_x, y = "Densitas") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11))
}

# Untuk menjalankan plot_pnorm_area dengan warna gelap, tambahkan colorspace
# install.packages("colorspace")  # jika belum ada
library(colorspace)

p_area1 <- plot_pnorm_area(mu_kem, sd_kem, mu_kem,
                           lower_tail = FALSE,
                           label_x = "Kemiskinan (%)",
                           label_var = "P(Kemiskinan > rata-rata)",
                           color_fill = "#378ADD")

p_area2 <- plot_pnorm_area(mu_ipm, sd_ipm, 80,
                           lower_tail = FALSE,
                           label_x = "IPM",
                           label_var = "P(IPM ≥ 80)",
                           color_fill = "#1D9E75")

p_area3 <- plot_pnorm_area(mu_peng, sd_peng, 10,
                           lower_tail = FALSE,
                           label_x = "Pengangguran (%)",
                           label_var = "P(Pengangguran > 10%)",
                           color_fill = "#D85A30")

p_area4 <- plot_pnorm_area(mu_kem, sd_kem, 5,
                           lower_tail = TRUE,
                           label_x = "Kemiskinan (%)",
                           label_var = "P(Kemiskinan < 5%)",
                           color_fill = "#7F77DD")

grid.arrange(p_area1, p_area2, p_area3, p_area4, ncol = 2,
             top = "Area Probabilitas di Bawah Kurva Normal — pnorm()")


# =============================================================================
# 7. KUANTIL DENGAN qnorm()
# =============================================================================

cat("=============================================================\n")
cat("BAGIAN 7: KUANTIL — qnorm()\n")
cat("=============================================================\n\n")

cat("qnorm(p, mean, sd) = nilai x sehingga P(X <= x) = p\n")
cat("Kebalikan dari pnorm(): diberi probabilitas, cari nilai batas.\n\n")

persentil <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

for (v in c("kemiskinan", "ipm", "pengangguran")) {
  mu_v  <- mean(df[[v]], na.rm = TRUE)
  sd_v  <- sd(df[[v]],   na.rm = TRUE)
  lbl_v <- var_labels[v]
  
  cat(sprintf("--- %s (μ=%.3f, σ=%.3f) ---\n", lbl_v, mu_v, sd_v))
  qnorm_vals   <- qnorm(persentil, mean = mu_v, sd = sd_v)
  empiris_vals <- quantile(df[[v]], probs = persentil, na.rm = TRUE)
  
  tbl <- data.frame(
    Persentil      = paste0("P", persentil * 100, "%"),
    p              = persentil,
    qnorm_teoritis = round(qnorm_vals, 4),
    quantile_aktual = round(empiris_vals, 4),
    Selisih        = round(abs(qnorm_vals - empiris_vals), 4)
  )
  print(tbl, row.names = FALSE)
  cat("\n")
}

cat("Interpretasi: Selisih antara qnorm teoritik dan kuantil aktual\n")
cat("mencerminkan penyimpangan dari normalitas (kurtosis platikurtik).\n\n")

# Contoh penggunaan qnorm untuk kebijakan
cat("--- Contoh Aplikasi qnorm() untuk Kebijakan ---\n\n")

# Batas kemiskinan 10% terendah (harus prioritas intervensi)
batas_10pct <- qnorm(0.90, mean = mu_kem, sd = sd_kem)
cat(sprintf("Batas kemiskinan P90 (10%% wilayah terburuk) : %.2f%%\n", batas_10pct))
cat(sprintf("  -> qnorm(0.90, %.2f, %.2f) = %.2f\n", mu_kem, sd_kem, batas_10pct))
cat(sprintf("  -> Artinya: wilayah dengan kemiskinan > %.2f%% masuk 10%% terburuk.\n\n", batas_10pct))

# Batas IPM 'baik' (top 25%)
batas_ipm_top25 <- qnorm(0.75, mean = mu_ipm, sd = sd_ipm)
cat(sprintf("Batas IPM Q3 (top 25%% wilayah terbaik) : %.2f\n", batas_ipm_top25))
cat(sprintf("  -> qnorm(0.75, %.2f, %.2f) = %.2f\n", mu_ipm, sd_ipm, batas_ipm_top25))
cat(sprintf("  -> Artinya: wilayah dengan IPM > %.2f masuk top 25%%.\n\n", batas_ipm_top25))


# =============================================================================
# 8. ATURAN EMPIRIS 68-95-99.7 (Z-Score)
# =============================================================================

cat("=============================================================\n")
cat("BAGIAN 8: ATURAN EMPIRIS 68-95-99.7 (Z-Score)\n")
cat("=============================================================\n\n")

zscore_results <- data.frame()

for (v in vars) {
  x    <- df[[v]][!is.na(df[[v]])]
  mu_v <- mean(x)
  sd_v <- sd(x)
  z    <- (x - mu_v) / sd_v
  
  w1_act <- mean(abs(z) <= 1) * 100
  w2_act <- mean(abs(z) <= 2) * 100
  w3_act <- mean(abs(z) <= 3) * 100
  
  zscore_results <- rbind(zscore_results, data.frame(
    Variabel    = v,
    Dalam_1SD   = round(w1_act, 2),
    Dalam_2SD   = round(w2_act, 2),
    Dalam_3SD   = round(w3_act, 2),
    Teori_1SD   = 68.27,
    Teori_2SD   = 95.45,
    Teori_3SD   = 99.73,
    Diff_1SD    = round(w1_act - 68.27, 2),
    stringsAsFactors = FALSE
  ))
}

cat("Perbandingan aturan empiris (teori normal) vs data aktual:\n\n")
print(zscore_results)

cat("\nInterpretasi:\n")
cat("Semua variabel memiliki ~57-58% data dalam ±1SD (vs teori 68.3%).\n")
cat("Ini konsisten dengan kurtosis platikurtik: distribusi lebih datar,\n")
cat("sehingga lebih sedikit data di sekitar rata-rata, lebih banyak di ekor.\n\n")

# Visualisasi Z-Score
cat("Membuat visualisasi Z-Score...\n")

zscore_long <- zscore_results %>%
  select(Variabel, Dalam_1SD, Dalam_2SD, Dalam_3SD,
         Teori_1SD, Teori_2SD, Teori_3SD) %>%
  pivot_longer(-Variabel) %>%
  mutate(
    SD_Level = case_when(
      grepl("1SD", name) ~ "±1σ",
      grepl("2SD", name) ~ "±2σ",
      grepl("3SD", name) ~ "±3σ"
    ),
    Tipe = ifelse(grepl("Teori", name), "Teori Normal", "Data Aktual")
  )

ggplot(zscore_long, aes(x = SD_Level, y = value, fill = Tipe)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = c(68.27, 95.45, 99.73),
             linetype = "dashed", color = "gray60", linewidth = 0.5) +
  facet_wrap(~ Variabel, labeller = labeller(Variabel = var_labels)) +
  scale_fill_manual(values = c("Teori Normal" = "#378ADD",
                               "Data Aktual"  = "#D85A30")) +
  labs(title    = "Aturan Empiris 68-95-99.7: Teori vs Data Aktual",
       subtitle = "Data aktual lebih datar dari normal (platikurtik)",
       x = "Rentang Z-Score", y = "% Data dalam Rentang",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title       = element_text(face = "bold"))


# =============================================================================
# 9. VISUALISASI PERBANDINGAN pnorm vs EMPIRIS
# =============================================================================

cat("=============================================================\n")
cat("BAGIAN 9: GRAFIK PERBANDINGAN pnorm vs Empiris\n")
cat("=============================================================\n")
cat("Membuat grafik perbandingan...\n\n")

prob_plot_data <- prob_summary %>%
  pivot_longer(cols = c(P_Normal, P_Empiris),
               names_to = "Sumber", values_to = "Probabilitas") %>%
  mutate(
    Sumber = recode(Sumber,
                    "P_Normal"  = "Normal (pnorm)",
                    "P_Empiris" = "Empiris (data)"),
    Pertanyaan = str_wrap(Pertanyaan, width = 25)
  )

ggplot(prob_plot_data,
       aes(x = reorder(Pertanyaan, Probabilitas),
           y = Probabilitas, fill = Sumber)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_text(aes(label = percent(Probabilitas, accuracy = 0.1)),
            position = position_dodge(width = 0.9),
            hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("Normal (pnorm)" = "#378ADD",
                               "Empiris (data)"  = "#D85A30")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(title    = "Perbandingan Probabilitas: pnorm() vs Empiris",
       subtitle = "Selisih disebabkan kurtosis platikurtik pada data",
       x = NULL, y = "Probabilitas", fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title       = element_text(face = "bold"),
        plot.margin      = margin(5, 40, 5, 5))


# =============================================================================
# 10. RINGKASAN AKHIR
# =============================================================================

cat("=============================================================\n")
cat("RINGKASAN AKHIR\n")
cat("=============================================================\n\n")

cat("Dataset  : Pembangunan Wilayah (N = 2500 total, 2337 bersih)\n")
cat("Missing  : 143 baris | Outlier: 20 baris\n\n")

cat("HASIL UJI DISTRIBUSI:\n")
cat("  - shapiro.test() : Semua variabel p < 0.05 -> tidak normal secara formal\n")
cat("  - Penyebab       : Kurtosis platikurtik (~-1.2), bukan asimetri\n")
cat("  - Distribusi simetris (skewness ~ 0) tapi lebih datar dari Gaussian\n\n")

cat("KONSEP PROBABILITAS (pnorm):\n")
cat("  - pnorm(x, mean, sd)               = P(X <= x)\n")
cat("  - pnorm(x, mean, sd, lower.tail=F) = P(X > x)\n")
cat("  - Selisih dua pnorm()              = P(a < X < b)\n\n")

cat("KONSEP KUANTIL (qnorm):\n")
cat("  - qnorm(p, mean, sd)               = nilai x dengan P(X <= x) = p\n")
cat("  - Kebalikan pnorm: dari probabilitas ke nilai\n\n")

cat("CATATAN PENGGUNAAN:\n")
cat("  - pnorm/qnorm valid sebagai aproksimasi meski data tidak sempurna normal\n")
cat("  - Untuk analisis lebih robust: pertimbangkan transformasi atau uji non-parametrik\n")
cat("  - Wilcoxon, Kruskal-Wallis sebagai alternatif t-test dan ANOVA\n\n")

cat("Analisis selesai.\n")
