# 📈 Aplikasi Peramalan Simple Moving Average (SMA)

Aplikasi interaktif berbasis **R Shiny** untuk analisis tren dan peramalan data deret waktu menggunakan metode **Simple Moving Average (SMA)**. Dikembangkan untuk memenuhi Tugas Akhir Mata Kuliah Komputasi Statistika.

## 👥 Anggota Kelompok

| Nama | NIM | Branch |
|---|---|---|
| Salwa Nur Rizki Putri | 1314624018 | `salwa` |
| Rizki Annisa | 1314624051 | `rizki` |
| Adiana Vania Rahmadani | 1314624043 | `adiana` |
| Aufar Radifan | 1314624044 | `aufar` |
| Maulana Fahnur | 1314624053 | `maulana` |
| Raihan Khalish Darmawan | 1314624073 | `raihan` |

## 🗂️ Struktur Proyek
Proyek ini menggunakan **satu file utama** `app.R` yang berisi seluruh kode UI dan server. Setiap bagian kode ditandai komentar sesuai penanggung jawabnya:

```r
# ---- [Dikerjakan: Salwa]   Tab Beranda & Landasan Teori ----
# ---- [Dikerjakan: Rizki]   Tab Informasi Dataset ----
# ---- [Dikerjakan: Adiana]  Tab Eksplorasi Tren ----
# ---- [Dikerjakan: Aufar]   Tab Peramalan MA ----
# ---- [Dikerjakan: Maulana] Tab Diagnostik Model ----
# ---- [Dikerjakan: Raihan]  Integrasi & menjalankan aplikasi ----
```

## 🗂️ Fitur Aplikasi
- **Beranda** — deskripsi aplikasi & landasan teori SMA
- **Informasi Dataset** — upload CSV / gunakan data default `AirPassengers`
- **Eksplorasi Tren** — visualisasi data aktual & statistik deskriptif
- **Peramalan MA** — kontrol orde MA, horizon peramalan, MAE & MAPE
- **Diagnostik Model** — uji normalitas (Shapiro-Wilk) & autokorelasi (Ljung-Box) pada residual

## ⚙️ Cara Menjalankan
```r
install.packages(c("shiny", "shinydashboard", "ggplot2", "zoo",
                    "dplyr", "forecast", "tseries", "DT"))
shiny::runApp("app.R")
```

## 🌿 Alur Kerja Git (Branching — Kerja Bergiliran)
Karena kode berada dalam **satu file**, setiap anggota mengerjakan bagiannya **secara bergiliran** (bukan bersamaan) untuk menghindari konflik.

Alur lengkapnya:
1. Sebelum mulai Lakukan: `git clone "link github nya"` (cukup sekali aja)
2. Pindah	ke	Branch	Milik	Sendiri: `git	checkout	nama-branch nya`
3. Tarik	Update	Terbaru	dari	main: `git	pull	origin	main`
4. Edit **hanya bagian dengan komentar nama sendiri** di `app.R`
5. Save file yang sudah diedit kemudian add dan Commit: `git	add	app.R` dan `git	commit	-m	"Menambahkan	bagian	[nama	tab]	-	Nama"`
6. Push ke branch sendiri sesuai nama: `git	push	origin	nama-branch nya`
7. Ajukan Pull Request ke `main`, lalu merge
8. Anggota berikutnya baru mulai setelah Pull Request sebelumnya selesai di-merge

