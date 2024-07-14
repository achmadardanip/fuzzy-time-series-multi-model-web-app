## Input Data
datauji <- read.csv("/cloud/project/avg_temp_sulteng.csv", header=T, sep =",") 
head(datauji)
data = datauji$tavg
n=length(data)
n

## Plot Data Aktual
library(ggplot2)
library(dplyr)
library(lubridate)

# Pastikan kolom tanggal bertipe Date (Tanggal)
datauji$date <- as.Date(datauji$date, format = "%d-%m-%Y")

# Mengagregasi data berdasarkan bulan agar lebih mudah dibaca
monthly_data <- datauji %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(monthly_avg_temp = mean(tavg, na.rm = TRUE))

# Plot data time series menggunakan ggplot2
ggplot(monthly_data, aes(x = month, y = monthly_avg_temp)) +
  geom_line(color = 'blue') +
  geom_smooth(method = "loess", span = 0.2, color = 'red', se = FALSE) + 
  labs(title = "Time Series Plot of Average Monthly Temperature\nin Kolaka Regency Southeast Sulawesi from 2001-2024",
       x = "Period",
       y = "Average Temperature (°C)") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(size = 14, hjust = 0.5, margin = margin(t = 20, b = 20))  
  )


## Mencari Data Maksimum dan Minimum
minimum = min(data)
maximum = max(data)
minimum
maximum

## Mencari Data Minimum Baru dan Data Maksimum Baru untuk Dijadikan sebagai Batas Bawah dan Batas Atas Interval Semesta Pembicaraan U
new.min = minimum-0.2
new.max = maximum+0.3
new.min
new.max

## Menentukan Banyak Interval (N) dan Panjang Interval (L)
n = round(1 +(3.3 *logb(length(data), base = 10)))
n
L = (new.max - new.min)/n
L


## Menentukan Batas-batas Interval
intrv.1 = seq(new.min,new.max,len = n+1)
intrv.1

## Pembagian Interval
box1 = data.frame(NA,nrow=length(intrv.1)-1,ncol=3)
names(box1) = c("bawah","atas","kel")

for (i in 1:length(intrv.1)-1) {
  box1[i,1]=intrv.1[i]
  box1[i,2]=intrv.1[i+1]
  box1[i,3]=i
  
}
box1

## Menentukan Nilai Tengah Interval
n.tengah = data.frame(tengah=(box1[,1]+box1[,2])/2,kel=box1[,3])
n.tengah

## Menentukan Bilangan Fuzzy
fuzifikasi=c() 
for (i in 1:length(data)){
  for (j in 1:nrow(box1)){
    if (i!=which.max(data)){
      if (data[i]>=(box1[j,1])&data[i]<(box1[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
    else {
      if (data[i]>=(box1[j,1])&data[i]<=(box1[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
  }
}
fuzifikasi

## Fuzifikasi ke Data Asal
fuzzyfy = cbind(data,fuzifikasi)
fuzzyfy

## Membuat Fuzzy Logical Relation (FLR)
FLR = data.frame(fuzzifikasi=0,left=NA,right =NA)
for (i in 1:length(fuzifikasi)) {
  FLR[i,1] = fuzifikasi[i]
  FLR[i+1,2] = fuzifikasi[i]
  FLR[i,3] = fuzifikasi[i]
}
FLR = FLR[-nrow(FLR),]
FLR = FLR[-1,]
FLR

## Membuat Fuzyy Logical Relation Group (FLRG)
FLRG = table(FLR[,2:3])
FLRG

## Membuat Matriks Transisi
bobot = round(prop.table(table(FLR[,2:3]),1),5)
bobot

## Peramalan Awal
diagonal = diag(bobot)
m.diagonal = diag(diagonal)
pinggir = bobot-m.diagonal

ramal=NULL
for (i in 1:(length(fuzifikasi))){
  for (j in 1:(nrow(bobot))){
    if (fuzifikasi[i]==j)
    {ramal[i+1]=(diagonal[j]*data[i])+sum(pinggir[j,]*n.tengah[,1]) }else
      if (fuzifikasi[i]==j)
      {ramal[i]=0}
  }
}
ramal = ramal[-length(ramal)]
ramal

## Adjusted Forecasting Value (Peramalan Tahap Kedua)
adjusted = rep(0,nrow(FLR)) 
selisih = FLR[,3]-FLR[,2] 
for(i in 1:nrow(FLR))
{
  if (FLR[i,2]!=FLR[i,3] && diagonal[FLR[i,2]]==0)
  {adjusted[i]=selisih[i]*(L/2)} else   #Untuk tidak komunicate
    if (selisih[i]==1 && diagonal[FLR[i,2]]>0)
    {adjusted[i]=(L)} else #Untuk  komunicate
      if (FLR[i,2]!=FLR[i,3] && diagonal[FLR[i,2]]>0)
      {adjusted[i]=selisih[i]*L/2} #Untuk komunicate
}
adjusted

## Peramalan Tahap Terakhir
ramal=ramal[c(2:length(ramal))]
adj.forecast=adjusted + ramal
adj.forecast

## Tabel Pembanding dengan Hasil Penyesuaian Kedua
datapakai = data[c(2:length(data))]
tanggal = data[c(1:length(data))]
galat = abs(datapakai-adj.forecast)
tabel = cbind(datapakai,ramal,adjusted,adj.forecast,galat)
tabel

## Penyesuaian Overestimate Ramalan
adj.forecast2 = rep(0,length(datapakai)) 
for(i in 1:length(datapakai)){
  if((ramal[i]-datapakai[i])<(adj.forecast[i]-datapakai[i])) {
    adj.forecast2[i]=ramal[i]+(L/2)
  } else {
    adj.forecast2[i]=adj.forecast[i]}
}
adj.forecast2

## Tabel Pembanding dengan Hasil Penyesuaian Kedua
tabel2 = cbind(datapakai,ramal,adj.forecast,adj.forecast2)
tabel2
tail(tabel2, 15)
datauji_excluded <- datauji[-1,]
tanggal = datauji_excluded$date
tabel_prediksi <- data.frame(tanggal, datapakai, ramal, adj.forecast, adj.forecast2)
tabel_prediksi

## Peramalan 7 hari kedepan sejak 6 Juli 2024
# Membuat fungsi untuk melakukan peramalan menggunakan model FTSMC
forecast_ftsmc <- function(data, n.tengah, bobot, L, box1, periode) {
  # Fuzifikasi data
  fuzifikasi <- rep(NA, length(data))
  for (i in 1:length(data)) {
    for (j in 1:nrow(box1)) {
      if (data[i] >= (box1[j,1]) & data[i] < (box1[j,2])) {
        fuzifikasi[i] = j
        break
      } else if (i == length(data) && data[i] == box1[j,2]) {
        fuzifikasi[i] = j
        break
      }
    }
  }
  
  # Memastikan tidak ada nilai NA dalam fuzifikasi
  if (any(is.na(fuzifikasi))) {
    stop("Fuzifikasi menghasilkan nilai NA. Pastikan semua data berada dalam rentang interval yang benar.")
  }

  # Inisialisasi variabel ramalan
  ramal <- numeric(length(data) + periode)
  ramal[1:length(data)] <- data
  
  # Melakukan peramalan untuk periode ke depan
  for (i in (length(data) + 1):(length(data) + periode)) {
    for (j in 1:(nrow(bobot))) {
      if (!is.na(fuzifikasi[i-1]) && fuzifikasi[i-1] == j) {
        ramal[i] = (diagonal[j] * ramal[i-1]) + sum(pinggir[j,] * n.tengah[,1])
      }
    }
    # Fuzifikasi nilai ramalan untuk periode berikutnya
    for (j in 1:nrow(box1)) {
      if (ramal[i] >= (box1[j,1]) & ramal[i] < (box1[j,2])) {
        fuzifikasi[i] = j
        break
      } else if (i == length(ramal) && ramal[i] == box1[j,2]) {
        fuzifikasi[i] = j
        break
      }
    }
  }
  
  # Mengembalikan hasil ramalan
  return(ramal[(length(data) + 1):(length(data) + periode)])
}

# Menentukan periode peramalan
periode_peramalan <- 7

# Menentukan nilai diagonal dan pinggir
diagonal <- diag(bobot)
m.diagonal <- diag(diagonal)
pinggir <- bobot - m.diagonal

# Menghitung nilai ramalan 7 hari ke depan
ramalan_7_hari <- forecast_ftsmc(data = adj.forecast2, n.tengah = n.tengah, bobot = bobot, L = L, box1 = box1, periode = periode_peramalan)

# Menampilkan hasil ramalan
ramalan_7_hari

# Gabungkan tabel2 dan data perkiraan baru, dan sertakan kolom tanggal
tabel2_new <- rbind(tabel2, data.frame(datapakai=NA, ramal=NA, adj.forecast=NA, adj.forecast2=ramalan_7_hari))

# Membuat urutan tanggal untuk baris baru
tanggal_terakhir <- max(tanggal)
tanggal_lanjut <- seq(tanggal_terakhir + 1, by = "day", length.out = nrow(tabel2_new) - length(tanggal))

# Tambahkan kolom tanggal ke tabel baru
tabel2_new$tanggal <- c(tanggal, tanggal_lanjut)

# Mengganti nama kolom
colnames(tabel2_new) <- c("Actual", "Initial Forecast", "Adjusted Forecast", "Final Adjusted Forecast", "Date")

# Hitung kolom kesalahan (error)
tabel2_new$Error <- abs(tabel2_new$Actual - tabel2_new$`Final Adjusted Forecast`)

# Susun ulang kolom untuk menempatkan 'tanggal' sebelum 'Aktual'
tabel2_new <- tabel2_new[, c("Date", "Actual", "Initial Forecast", "Adjusted Forecast", "Final Adjusted Forecast", "Error")]

# Menampilkan tabel
tabel2_new
tail(tabel2_new, 10)

## Uji Ketepatan
galat2 = abs(datapakai-adj.forecast2)
MSE = mean(galat2^2, na.rm = TRUE)
MAE = mean(abs(galat2), na.rm = TRUE)
MAPE = mean(abs(galat2/datapakai*100), na.rm=TRUE)
ketepatan = cbind(MSE,MAE,MAPE)
ketepatan

## Plot Data Aktual dan Data Prediksi
# Ubah kolom tanggal ke format Tanggal
datauji_excluded <- datauji[-1,]
datauji_excluded$date <- as.Date(datauji_excluded$date, format = "%d-%m-%Y")
tabel2_new$Date <- as.Date(tabel2_new$Date, format = "%Y-%m-%d")
# Mengagregasi data berdasarkan bulan agar lebih mudah dibaca
monthly_data_actual <- datauji_excluded %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(monthly_avg_temp = mean(tavg, na.rm = TRUE))

monthly_data_predicted <- tabel2_new %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarize(monthly_avg_temp = mean(`Final Adjusted Forecast`, na.rm = TRUE))

# Menggabungkan data aktual dan prediksi
combined_data <- bind_rows(
  monthly_data_actual %>% mutate(type = "Actual"),
  monthly_data_predicted %>% mutate(type = "Predicted")
)

# Plot data time series menggunakan ggplot2
ggplot(combined_data, aes(x = month, y = monthly_avg_temp, color = type)) +
  geom_line(aes(linetype = type)) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "orange")) +
  labs(title = "Time Series Plot of Average Monthly Temperature",
       x = "Period",
       y = "Average Temperature (°C)",
       color = "Type") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 