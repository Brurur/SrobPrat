library(tidyverse)   # mencakup: ggplot2, dplyr, tidyr, readr, dll.

df_raw <- read_csv("Dataset/pembangunan_wilayah_missing_outlier.csv")

# Memilih variable dan menangani missing values
var_numerik <- c("pdrb_perkapita", "kemiskinan", "pengangguran",
                 "ipm", "harapan_hidup", "rata_lama_sekolah",
                 "akses_internet", "jalan_baik", "air_bersih")

colSums(is.na(df_raw))

df <- df_raw

numerik <- c(
  "pdrb_perkapita",
  "kemiskinan",
  "pengangguran",
  "ipm",
  "akses_internet",
  "jalan_baik"
)

for(i in numerik){
  df[[i]][is.na(df[[i]])] <-
    median(df[[i]], na.rm = TRUE)
}

colSums(is.na(df))

df_summary <- df %>%
  pivot_longer(cols = where(is.numeric), names_to = "variabel", values_to = "nilai") %>%
  group_by(variabel) %>%
  summarise(
    n    = n(),
    mean = round(mean(nilai), 3),
    sd   = round(sd(nilai),   3),
    min  = round(min(nilai),  3),
    max  = round(max(nilai),  3),
    .groups = "drop"
  )

print(df_summary)

# Pearson; ganti method="spearman" jika distribusi tidak normal
mat_cor <- df %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs", method = "spearman")

print(round(mat_cor, 3))

# ------ 5. UJI SIGNIFIKANSI (cor.test) --------------------
# Fungsi untuk mencetak cor.test semua pasang variabel
uji_korelasi <- function(data) {
  data  <- data %>% select(where(is.numeric))
  vars  <- names(data)
  n_var <- length(vars)
  
  cat(sprintf("%-35s %7s %10s  %s\n",
              "Pasangan Variabel", "rho", "p-value", "Interpretasi"))
  cat(strrep("-", 75), "\n")
  
  for (i in 1:(n_var - 1)) {
    for (j in (i + 1):n_var) {
      uji <- cor.test(data[[vars[i]]], data[[vars[j]]],
                      method = "spearman", exact = FALSE)  # exact=FALSE hindari warning ties
      r  <- round(uji$estimate, 3)
      pv <- uji$p.value
      
      arah <- ifelse(r > 0, "Positif", "Negatif")
      
      kuat <- case_when(
        abs(r) >= 0.80 ~ "Sangat Kuat",
        abs(r) >= 0.60 ~ "Kuat",
        abs(r) >= 0.40 ~ "Sedang",
        abs(r) >= 0.20 ~ "Lemah",
        TRUE           ~ "Sangat Lemah"
      )
      
      sig   <- ifelse(pv < 0.05, "*", " ")
      label <- paste0(arah, ", ", kuat, sig)
      
      cat(sprintf("%-35s %7.3f %10.4f  %s\n",
                  paste0(vars[i], " vs ", vars[j]),
                  r, pv, label))
    }
  }
  cat("\nKeterangan: * = signifikan pada α = 0.05\n")
  cat("Kekuatan  : |r| ≥ .80 Sangat Kuat | ≥.60 Kuat | ≥.40 Sedang | ≥.20 Lemah\n")
}

uji_korelasi(df)

# ------ 6. ANALISIS FOKUS (sesuai contoh modul) -----------

## 6a. IPM dan Kemiskinan
uji_6a <- cor.test(df$ipm, df$kemiskinan, method = "pearson")
cat("\n[1] IPM vs Kemiskinan\n")
cat(sprintf("    r = %.3f | t = %.3f | p-value = %.4f\n",
            uji_6a$estimate, uji_6a$statistic, uji_6a$p.value))
cat("    Arah    :", ifelse(uji_6a$estimate > 0, "POSITIF", "NEGATIF"), "\n")
cat("    Makna   : Semakin tinggi IPM, kemiskinan cenderung",
    ifelse(uji_6a$estimate < 0, "MENURUN.", "MENINGKAT."), "\n")
cat("    Signifikan:", ifelse(uji_6a$p.value < 0.05, "YA (p < 0.05)", "TIDAK (p >= 0.05)"), "\n")

## 6b. Akses Internet dan Pendidikan (rata-rata lama sekolah)
uji_6b <- cor.test(df$akses_internet, df$rata_lama_sekolah, method = "pearson")
cat("\n[2] Akses Internet vs Rata-rata Lama Sekolah\n")
cat(sprintf("    r = %.3f | t = %.3f | p-value = %.4f\n",
            uji_6b$estimate, uji_6b$statistic, uji_6b$p.value))
cat("    Arah    :", ifelse(uji_6b$estimate > 0, "POSITIF", "NEGATIF"), "\n")
cat("    Makna   : Wilayah dengan rata-rata sekolah lebih tinggi cenderung",
    ifelse(uji_6b$estimate > 0, "memiliki akses internet LEBIH BAIK.", "memiliki akses internet LEBIH RENDAH."), "\n")
cat("    Signifikan:", ifelse(uji_6b$p.value < 0.05, "YA (p < 0.05)", "TIDAK (p >= 0.05)"), "\n")

## 6c. PDRB per Kapita dan Tingkat Kesejahteraan
uji_6c1 <- cor.test(df$pdrb_perkapita, df$harapan_hidup, method = "pearson")
uji_6c2 <- cor.test(df$pdrb_perkapita, df$air_bersih,   method = "pearson")

cat("\n[3] PDRB per Kapita vs Harapan Hidup\n")
cat(sprintf("    r = %.3f | t = %.3f | p-value = %.4f\n",
            uji_6c1$estimate, uji_6c1$statistic, uji_6c1$p.value))
cat("    Arah    :", ifelse(uji_6c1$estimate > 0, "POSITIF", "NEGATIF"), "\n")
cat("    Signifikan:", ifelse(uji_6c1$p.value < 0.05, "YA (p < 0.05)", "TIDAK (p >= 0.05)"), "\n")

cat("\n[4] PDRB per Kapita vs Akses Air Bersih\n")
cat(sprintf("    r = %.3f | t = %.3f | p-value = %.4f\n",
            uji_6c2$estimate, uji_6c2$statistic, uji_6c2$p.value))
cat("    Arah    :", ifelse(uji_6c2$estimate > 0, "POSITIF", "NEGATIF"), "\n")
cat("    Signifikan:", ifelse(uji_6c2$p.value < 0.05, "YA (p < 0.05)", "TIDAK (p >= 0.05)"), "\n")

