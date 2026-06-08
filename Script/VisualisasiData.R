library(readr)
library(ggplot2)
library(dplyr)

data_wilayah <- read.csv(
  "Dataset/pembangunan_wilayah_missing_outlier.csv"
)


data_wilayah_2020 <- data_wilayah %>%
  filter(tahun == 2020) %>% 
  filter(provinsi == "DKI Jakarta")

# 1. Histogram IPM dengan pengangguran
ggplot(data = data_wilayah, aes(x = ipm, y = pengangguran)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Hubungan Lama Sekolah dan Tingkat Pengangguran",
    x = "ipm",
    y = "pengangguran"
  ) +
  theme_minimal()

# 2. Ranking provinsi berdasarkan IPM
data_provinsi <- data_wilayah %>%
  group_by(provinsi) %>%
  summarise(ipm = mean(ipm, na.rm = TRUE))

ggplot(data_provinsi,
       aes(x = reorder(provinsi, ipm), y = ipm)) +
  geom_col() +
  coord_cartesian(ylim = c(69, 71)) +
  labs(
    title = "Ranking IPM Provinsi",
    x = "Provinsi",
    y = "IPM"
  ) +
  theme_minimal()

# 3. Geom Point Pendidikan dengan Harapan Hidup

ggplot(
  data_wilayah,
  aes(
    x = rata_lama_sekolah,
    y = harapan_hidup
  )
) +
  geom_point(
    alpha = 0.7,
    size = 2,
    color = "#2C7FB8"
  ) +
  labs(
    title = "Pendidikan dan Harapan Hidup",
    subtitle = "Wilayah dengan pendidikan lebih tinggi cenderung memiliki usia harapan hidup lebih tinggi",
    x = "Rata-rata Lama Sekolah (tahun)",
    y = "Harapan Hidup (tahun)"
  ) +
  theme_minimal(base_size = 12)

# 4. Tren DKI Jakarta dari Tahun ke Tahun
dki <- data_wilayah %>%
  filter(provinsi == "DKI Jakarta")

ggplot(dki, aes(x = factor(tahun), y = ipm)) +
  geom_boxplot(fill = "skyblue") +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    shape = 18
  ) +
  labs(
    title = "Distribusi IPM DKI Jakarta per Tahun",
    x = "Tahun",
    y = "IPM"
  ) +
  theme_minimal()

# 5. Hubungan Pengangguran dan Kemiskinan
ggplot(
  data_wilayah,
  aes(x = pengangguran, y = kemiskinan)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Hubungan Pengangguran dan Kemiskinan",
    x = "Pengangguran (%)",
    y = "Kemiskinan (%)"
  ) +
  theme_minimal()
