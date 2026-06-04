library(tidyverse)
library(e1071)  # untuk skewness & kurtosis

df <- read_csv("Dataset/pembangunan_wilayah_missing_outlier.csv")

df
num_vars <- c("pdrb_perkapita","kemiskinan","pengangguran","ipm",
              "harapan_hidup","rata_lama_sekolah","akses_internet",
              "jalan_baik","air_bersih")

# Statistika deskriptif lengkap
df %>%
  select(all_of(num_vars)) %>%
  pivot_longer(everything(), names_to = "variabel") %>%
  group_by(variabel) %>%
  summarise(
    n_valid   = sum(!is.na(value)),
    mean      = mean(value, na.rm = TRUE),
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
