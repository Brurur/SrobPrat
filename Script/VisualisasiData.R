library(readr)
library(ggplot2)

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
