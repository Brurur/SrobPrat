data_wilayah <- read.csv(
  "Dataset/pembangunan_wilayah_missing_outlier.csv"
)

pembangunan_wilayah <- data.frame(data_wilayah)

str(pembangunan_wilayah)
# provinsi         : chr  "Jawa Barat" "Jawa Timur" "Kalimantan Timur" "DKI Jakarta" ...
# tempat observasi berdasarkan provinsi
# tahun            : int  2022 2022 2020 2022 2023 2020 2024 2023 2023 2020 ...
# tahun diobservasi
# pdrb_perkapita   : num  3.84e+07 1.66e+08 2.68e+07 1.55e+08 1.99e+08 ...
# Nilai ekonomi, nol = tidak ada pendapatan, perbandingan valid
# kemiskinan       : num  19.69 9.68 4.13 20.43 14.39 ...
# Persentase populasi, 0% = tidak ada kemiskinan
# pengangguran     : num  4.73 4.43 2.99 3.9 12.15 ...
# Persentase, 0% = tidak ada pengangguran
# ipm              : num  80.2 55 78.7 73.3 72.9 ...
# Indeks 0–100 dengan titik nol bermakna (pembangunan nol)
# harapan_hidup    : num  75.8 67.8 60.4 69.5 75 ...
# Tahun, 0 = tidak ada harapan hidup, perbandingan valid
# rata_lama_sekolah: num  8.41 5.79 13.2 12.28 9.69 ...
# Tahun, 0 = belum pernah sekolah
# akses_internet   : num  63.9 66.6 64.7 44.6 52.7 ...
# Persentase, 0% = tidak ada akses sama sekali
# jalan_baik       : num  53.9 34.9 48.6 57.9 65.7 ...
# Persentase infrastruktur, titik nol bermakna
# air_bersih       : num  43.1 58.9 90.3 86.2 90.8 ...
# Persentase akses, 0% = tidak ada sama sekali
# catatan_data     : chr  "Normal" "Normal" "Normal" "Normal" ...
# Kategori kualitas data ("Normal", dll.) tanpa urutan bermakna
#
# Nominal : provinsi, catatan_data (klasifikasi/kategori)
# Ordinal : tidak ada
# Interval : tahun (Dapat diurutkan dan mengukur perbedaan/interval antar nilai)
# Rasio : pdrb_perkapita, kemiskinan, pengangguran, ipm, harapan_hidup,
#         rata_lama_sekolah, akses_internet
#         (Memiliki semua karakteristik interval)

summary(pembangunan_wilayah)

head(pembangunan_wilayah)

dim(pembangunan_wilayah)
# 2500 baris obvservasi 13 variable