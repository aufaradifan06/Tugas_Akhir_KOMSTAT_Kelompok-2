# ============================================================
# APLIKASI PERAMALAN SIMPLE MOVING AVERAGE (SMA)
# Tugas Akhir Komputasi Statistika — Kelompok 2
# Versi: SATU FILE, terbagi jadi FUNGSI TERPISAH per anggota
#
# ATURAN PENTING:
# - Setiap orang HANYA boleh mengedit fungsi dengan namanya sendiri
# - JANGAN mengedit fungsi milik orang lain
# - JANGAN mengedit bagian "UI UTAMA" dan "SERVER UTAMA" di paling
#   bawah file ini (itu tanggung jawab Raihan)
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(zoo)
library(dplyr)
library(forecast)
library(tseries)
library(DT)

# ==========================================
# DATA DEFAULT (AirPassengers) — jangan diubah siapa pun
# ==========================================
data_default <- data.frame(
  Waktu = as.numeric(time(AirPassengers)),
  Bulan = format(as.Date(time(AirPassengers)), "%b-%Y"),
  Penumpang = as.vector(AirPassengers)
)

# ############################################################
# FUNGSI UI & SERVER — RIZKI (Tab Informasi Dataset)
# ui_dataset_sidebar(), ui_dataset(),server_dataset()
# ############################################################
ui_dataset_sidebar <- function() {
  box(
    title = "📁 Input Data Anda", width = 12, solidHeader = TRUE,
    class = "kotak-input-kustom",
    fileInput("file_user", "Upload File CSV:", accept = c(".csv")),
    checkboxInput("header_csv", "Baris Pertama adalah Header", TRUE),
    uiOutput("pilih_kolom_x"),
    uiOutput("pilih_kolom_y")
  )
}

ui_dataset <- function() {
  fluidRow(
    box(title = "Informasi & Tampilan Data", width = 12, status = "danger", solidHeader = TRUE,
        p("Gunakan menu di sebelah kiri untuk mengunggah dataset CSV Anda sendiri. Jika kosong, sistem akan menggunakan data default maskapai penerbangan."),
        hr(),
        DTOutput("tabel_asli")
    )
  )
}

# Fungsi ini mengembalikan (return) reactive data_proses,
# karena dipake oleh fungsi Adiana & Aufar di bawah
# JANGAN ubah nama fungsi ini atau apa yang di-return
server_dataset <- function(input, output, session) {
  data_aktif <- reactive({
    if (is.null(input$file_user)) return(data_default)
    read.csv(input$file_user$datapath, header = input$header_csv, stringsAsFactors = FALSE)
  })
  
  output$pilih_kolom_x <- renderUI({
    selectInput("kolom_x", "Pilih Kolom Waktu/Bulan:", choices = colnames(data_aktif()), selected = colnames(data_aktif())[1])
  })
  
  output$pilih_kolom_y <- renderUI({
    selectInput("kolom_y", "Pilih Kolom Nilai (Y):", choices = colnames(data_aktif()), selected = colnames(data_aktif())[3])
  })
  
  data_proses <- reactive({
    req(input$kolom_x, input$kolom_y)
    df <- data_aktif()
    data.frame(
      Waktu = 1:nrow(df),
      Label_Waktu = as.character(df[[input$kolom_x]]),
      Penumpang = as.numeric(gsub(",", "", df[[input$kolom_y]]))
    )
  })
  
  output$tabel_asli <- renderDT({ data_proses()[, c("Label_Waktu", "Penumpang")] }, options = list(pageLength = 5))
  
  return(data_proses)   #JANGAN DIHAPUS
}


# ############################################################
# FUNGSI UI & SERVER — AUFAR (Tab Peramalan MA)
# ui_peramalan() & server_peramalan()
# ############################################################
ui_peramalan <- function() {
  tagList(
    fluidRow(
      valueBoxOutput("mae_box", width = 6),
      valueBoxOutput("mape_box", width = 6)
    ),
    fluidRow(
      box(title = "Kontrol Moving Average & Peramalan", width = 4, status = "warning", solidHeader = TRUE,
          numericInput("ma_order", "Tentukan Orde MA (Periode):", value = 12, min = 2, max = 24),
          numericInput("h_forecast", "Ramal Berapa Periode ke Depan?", value = 6, min = 1, max = 24),
          hr(),
          p(icon("lightbulb", class="text-warning"), "Tips: Gunakan orde 12 untuk mereduksi efek musiman data bulanan.")
      ),
      box(title = "Visualisasi Pemulusan & Peramalan ke Depan", width = 8, status = "danger", solidHeader = TRUE,
          plotOutput("plot_ma", height = "350px"),
          br(),
          downloadButton("btn_download_plot", "Unduh Grafik (PNG)", class = "btn-warning")
      )
    ),
    fluidRow(
      box(title = "Data Hasil Perhitungan", width = 12, status = "warning", solidHeader = TRUE,
          DTOutput("tabel_hasil")
      )
    )
  )
}

#parameter data_proses dari server_dataset() rizki.
#fungsi ini mengembalikan reactive data_final_ma, dipakai fungsi maulana.
server_peramalan <- function(input, output, session, data_proses) {
  data_final_ma <- reactive({
    df <- data_proses()
    k_ordo <- input$ma_order
    h <- input$h_forecast
    
    df$MA_Hasil <- rollmean(df$Penumpang, k = k_ordo, fill = NA, align = "right")
    nilai_terakhir_ma <- tail(na.omit(df$MA_Hasil), 1)
    waktu_terakhir <- max(df$Waktu)
    
    df_forecast <- data.frame(
      Waktu = (waktu_terakhir + 1):(waktu_terakhir + h),
      Label_Waktu = paste("Forecast +", 1:h),
      Penumpang = NA,
      MA_Hasil = nilai_terakhir_ma
    )
    
    rbind(df, df_forecast)
  })
  
  plot_ma_reaktif <- reactive({
    df <- data_final_ma()
    df_hist <- subset(df, !is.na(Penumpang))
    df_fcst <- subset(df, is.na(Penumpang))
    
    ggplot() +
      geom_line(data = df_hist, aes(x = Waktu, y = Penumpang, color = "Data Aktual"), size = 0.8, alpha = 0.5) +
      geom_line(data = df_hist, aes(x = Waktu, y = MA_Hasil, color = "Moving Average"), size = 1.4, na.rm = TRUE) +
      geom_line(data = df_fcst, aes(x = Waktu, y = MA_Hasil, color = "Garis Ramalan Masa Depan"), size = 1.4, linetype = "dashed") +
      scale_color_manual(values = c("Data Aktual" = "#7f8c8d", "Moving Average" = "#e67e22", "Garis Ramalan Masa Depan" = "#c0392b")) +
      theme_minimal(base_size = 14) +
      labs(x = "Periode Waktu", y = "Nilai", color = "Keterangan") +
      theme(legend.position = "top")
  })
  
  output$plot_ma <- renderPlot({ plot_ma_reaktif() })
  
  output$btn_download_plot <- downloadHandler(
    filename = function() { paste("Grafik_MA_", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = plot_ma_reaktif(), device = "png", width = 10, height = 6, dpi = 300) }
  )
  
  output$mae_box <- renderValueBox({
    df <- subset(data_final_ma(), !is.na(Penumpang))
    error <- df$Penumpang - df$MA_Hasil
    valueBox(round(mean(abs(error), na.rm = TRUE), 2), "Mean Absolute Error (MAE)", icon = icon("chart-bar"), color = "red")
  })
  
  output$mape_box <- renderValueBox({
    df <- subset(data_final_ma(), !is.na(Penumpang))
    error <- df$Penumpang - df$MA_Hasil
    mape_val <- round(mean(abs(error / df$Penumpang), na.rm = TRUE) * 100, 2)
    valueBox(paste0(mape_val, "%"), "Mean Abs. Percentage Error", icon = icon("check-circle"), color = "yellow")
  })
  
  output$tabel_hasil <- renderDT({ data_final_ma()[, c("Label_Waktu", "Penumpang", "MA_Hasil")] }, options = list(pageLength = 5))
  
  return(data_final_ma)   #JANGAN DIHAPUS
}
