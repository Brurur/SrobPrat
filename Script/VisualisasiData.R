library(readr)
library(ggplot2)

data_wilayah <- read.csv(
  "Dataset/pembangunan_wilayah_missing_outlier.csv"
)


#data_wilayah_2020 <- data_wilayah %>%
#  filter(tahun == 2020) %>% 
#  filter(provinsi == "DKI Jakarta")

# 1. Histogram IPM
# ggplot(data = data_wilayah_2020, aes(x = ipm, y = kemiskinan)) +
#  geom_point() +
#  geom_smooth() +
#  labs(
#    title = "Hubungan Lama Sekolah dan Tingkat Pengangguran",
#    x = "ipm",
#    y = "kemiskinan"
#  ) +
#  theme_minimal()

ggplot(data_provinsi,
       aes(x = reorder(provinsi, ipm), y = harapan)) +
  geom_col() +
  coord_cartesian(ylim = c(69, 71)) +
  labs(
    title = "Ranking IPM Provinsi",
    x = "Provinsi",
    y = "IPM"
  ) +
  theme_minimal()



ranking_ipm <- data_wilayah %>%
  group_by(provinsi) %>%
  summarise(ipm = mean(ipm, na.rm = TRUE)) %>%
  arrange(desc(ipm))

ranking_ipm
