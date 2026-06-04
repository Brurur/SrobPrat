library(tidyverse)
library(e1071)  # untuk skewness & kurtosis


data_wilayah <- read.csv(
  "Dataset/pembangunan_wilayah_missing_outlier.csv"
)

pembangunan_wilayah <- data.frame(data_wilayah)
#pembangunan_wilayah

#colnames(pembangunan_wilayah)
num_vars <- c("pdrb_perkapita","kemiskinan","pengangguran","ipm",
              "harapan_hidup","rata_lama_sekolah","akses_internet",
              "jalan_baik","air_bersih")

# Statistika deskriptif lengkap
Statistik_Pembangunan_Wilayah <- data.frame(pembangunan_wilayah %>%
  select(all_of(num_vars)) %>%
  pivot_longer(everything(), names_to = "Variabel") %>%
  group_by(Variabel) %>%
  summarise(
    Total     = sum(!is.na(value)),
    rata_rata = mean(value, na.rm = TRUE),
    median    = median(value, na.rm = TRUE),
    modus     = as.numeric(names(which.max(table(value)))),
    min       = min(value, na.rm = TRUE),
    max       = max(value, na.rm = TRUE),
    sd        = sd(value, na.rm = TRUE),
    varians   = var(value, na.rm = TRUE),
    q1        = quantile(value, 0.25, na.rm = TRUE),
    q3        = quantile(value, 0.75, na.rm = TRUE),
    iqr       = IQR(value, na.rm = TRUE),
    skewness  = skewness(value, na.rm = TRUE),
    kurtosis  = kurtosis(value, na.rm = TRUE),
    cv_pct    = (sd(value,na.rm=T)/mean(value,na.rm=T))*100
  )
)

Statistik_Pembangunan_Wilayah

# ── 1. Variabel dengan nilai rata-rata tertinggi ──────────────────────────────
rata_rata_tertinggi <- Statistik_Pembangunan_Wilayah %>%
  arrange(desc(rata_rata)) %>%
  slice(1) %>%
  select(Variabel,rata_rata)

cat("Variabel dengan rata-rata tertinggi:\n")
print(rata_rata_tertinggi)

# ── 2. Variabel dengan penyebaran terbesar ────────────────────────────────────
# Menggunakan CV (%) agar perbandingan adil antar variabel berbeda satuan
penyebaran_terbesar <- Statistik_Pembangunan_Wilayah %>%
  arrange(desc(cv_pct)) %>%
  slice(1) %>%
  select(Variabel, sd, varians, cv_pct)

cat("\nVariabel dengan penyebaran terbesar (CV tertinggi):\n")
print(penyebaran_terbesar)


# ── 3. Indikasi data tidak normal ─────────────────────────────────────────────
# Kriteria:
#   Skewness  : tidak normal jika |skewness| > 0.5  (menceng)
#   Kurtosis  : tidak normal jika |kurtosis| > 1    (terlalu runcing/datar)

indikasi_tidak_normal <- Statistik_Pembangunan_Wilayah %>%
  mutate(
    skew_tidak_normal = abs(skewness) > 0.5,
    kurt_tidak_normal = abs(kurtosis) > 1,
    tidak_normal      = skew_tidak_normal | kurt_tidak_normal,
    keterangan = case_when(
      skew_tidak_normal & kurt_tidak_normal ~ "Skewness & Kurtosis tidak normal",
      skew_tidak_normal                     ~ "Skewness tidak normal",
      kurt_tidak_normal                     ~ "Kurtosis tidak normal",
      TRUE                                  ~ "Normal"
    )
  ) %>%
  select(Variabel, skewness, kurtosis, keterangan) %>%
  arrange(desc(abs(skewness)))

cat("\nIndikasi data tidak normal:\n")
print(indikasi_tidak_normal)

# Ringkasan: hanya yang tidak normal
cat("\nVariabel yang terindikasi TIDAK normal:\n")
indikasi_tidak_normal %>%
  filter(keterangan != "Normal") %>%
  print()




