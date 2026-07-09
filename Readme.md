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
Karena kode berada dalam **satu file**, setiap anggota mengerjakan bagiannya **secara bergiliran** (bukan bersamaan) untuk menghindari konflik. Urutan dan langkah lengkap ada di `ALUR_SATU_FILE.md`.

Ringkasnya:
1. Tarik versi terbaru dari `main` sebelum mulai: `git pull origin main`
2. Edit **hanya bagian dengan komentar nama sendiri** di `app.R`
3. Commit & push ke branch sendiri
4. Ajukan Pull Request ke `main`, lalu merge
5. Anggota berikutnya baru mulai setelah PR sebelumnya di-merge

## 📄 Lisensi
Proyek ini dibuat untuk keperluan akademik, Program Studi Statistika, Universitas Negeri Jakarta.
