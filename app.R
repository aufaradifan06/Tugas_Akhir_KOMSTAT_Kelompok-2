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
# FUNGSI UI — SALWA (Tab Beranda)
# HANYA EDIT DI DALAM ui_beranda() INI SAJA
# ############################################################
ui_beranda <- function() {
  tagList(
    fluidRow(
      box(title = "       Selamat Datang      ", width = 12, status = "danger", solidHeader = TRUE,
          h3(strong("Aplikasi Peramalan Menggunakan Simple Moving Average (SMA)"),
             style = "text-align:center; color:#c0392b;"),
          p("Aplikasi interaktif ini dikembangkan untuk memenuhi Tugas Akhir Mata Kuliah Komputasi Statistika. Aplikasi ini memungkinkan pengguna untuk melakukan analisis tren dan peramalan data deret waktu secara real-time.",
            style = "text-align:center; font-size:16px;"),
          hr(),
          h4(icon("users", class = "text-danger"), strong("Anggota Tim:")),
          tags$ul(style = "font-size:16px;",
                  tags$li("Salwa Nur Rizki Putri (1314624018)"),
                  tags$li("Rizki Annisa (1314624051)"),
                  tags$li("Adiana Vania Rahmadani (1314624043)"),
                  tags$li("Aufar Radifan (1314624044)"),
                  tags$li("Maulana Fahnur (1314624053)"),
                  tags$li("Raihan Khalish Darmawan (1314624073)")
          )
      )
    ),
    fluidRow(
      box(title = "            Landasan Teori: Simple Moving Average (SMA)", width = 12, status = "warning", solidHeader = TRUE,
          div(class = "teks-teori",
              p("Metode ", strong("Simple Moving Average (SMA)"), " adalah teknik peramalan deret waktu yang paling fundamental. Metode ini bekerja dengan cara menghitung nilai rata-rata dari sejumlah observasi masa lalu yang telah ditentukan (disebut sebagai orde atau panjang jendela)."),
              p("Setiap kali data baru muncul, rata-rata ini akan 'bergerak' dengan membuang data yang paling lama dan memasukkan data yang paling baru. Tujuannya adalah untuk menghaluskan fluktuasi acak (noise) agar tren asli dari data dapat terlihat lebih jelas."),
              h5(strong("Formulasi Matematis:")),
              p("Jika Y(t) adalah nilai observasi pada waktu ke-t, dan k adalah panjang orde yang dipilih, maka nilai pemulusan Moving Average (SMA(t)) dirumuskan sebagai:"),
              p("$$SMA_t = \\frac{Y_t + Y_{t-1} + Y_{t-2} + \\dots + Y_{t-k+1}}{k} = \\frac{1}{k} \\sum_{i=0}^{k-1} Y_{t-i}$$"),
              h5(strong("Kelebihan & Kekurangan:")),
              tags$ul(
                tags$li(strong("Kelebihan:"), " Sangat mudah dipahami, diimplementasikan, dan efektif meredam kejutan data jangka pendek."),
                tags$li(strong("Kekurangan:"), " Bersifat tertinggal (lagging) terhadap tren yang sedang berlangsung, dan hasil peramalan ke masa depan (forecasting) hanya berupa garis lurus mendatar sebesar nilai rata-rata terakhir.")
              )
          )
      )
    )
  )
}

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
# FUNGSI UI & SERVER — ADIANA (Tab Eksplorasi Tren)
# HANYA EDIT DI DALAM ui_eksplorasi() DAN server_eksplorasi() INI SAJA
# ############################################################
ui_eksplorasi <- function() {
  tagList(
    fluidRow(
      box(title = "Visualisasi Tren Pertumbuhan Data Aktual", width = 12, status = "warning", solidHeader = TRUE,
          plotOutput("plot_asli")
      )
    ),
    fluidRow(
      box(title = "Ringkasan Statistik Deskriptif", width = 12, status = "danger", solidHeader = TRUE,
          verbatimTextOutput("summary_stat")
      )
    )
  )
}

# Parameter data_proses dikirim dari server_dataset() milik Rizki — jangan diubah urutan/nama parameter
server_eksplorasi <- function(input, output, session, data_proses) {
  output$summary_stat <- renderPrint({ summary(data_proses()$Penumpang) })
  
  output$plot_asli <- renderPlot({
    df <- data_proses()
    ggplot(df, aes(x = Waktu, y = Penumpang)) +
      geom_line(color = "#e67e22", size = 1) +
      geom_point(color = "#c0392b", size = 2) +
      scale_x_continuous(breaks = seq(1, nrow(df), length.out = 10), labels = df$Label_Waktu[seq(1, nrow(df), length.out = 10)]) +
      theme_minimal(base_size = 14) +
      labs(x = "Periode Waktu", y = "Nilai Aktual")
  })
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
# ############################################################
# FUNGSI UI & SERVER — MAULANA (Tab Diagnostik Model)
# ############################################################
ui_diagnostik <- function() {
  tagList(
    fluidRow(
      box(title = "Distribusi Sisaan (Histogram)", width = 6, status = "warning", solidHeader = TRUE,
          plotOutput("plot_hist_resid")
      ),
      box(title = "Uji Autokorelasi (Plot ACF)", width = 6, status = "danger", solidHeader = TRUE,
          plotOutput("plot_acf_resid")
      )
    ),
    fluidRow(
      box(title = "Hasil Uji Statistik Formal & Asisten Sistem", width = 12, status = "warning", solidHeader = TRUE,
          verbatimTextOutput("uji_formal_resid"),
          hr(),
          uiOutput("kesimpulan_diagnostik")
      )
    )
  )
}

# Parameter data_final_ma dari server_peramalan() milik Aufar — jangan ubah nama/urutan parameter
server_diagnostik <- function(input, output, session, data_final_ma) {
  residual_data <- reactive({
    df <- subset(data_final_ma(), !is.na(Penumpang))
    na.omit(df$Penumpang - df$MA_Hasil)
  })
  
  output$plot_hist_resid <- renderPlot({
    ggplot(data.frame(err = residual_data()), aes(x = err)) +
      geom_histogram(fill = "#f39c12", color = "white", bins = 20) +
      geom_vline(xintercept = 0, linetype = "dashed", size = 1, color = "#c0392b") +
      theme_minimal() + labs(x = "Sisaan (Residual)", y = "Frekuensi")
  })
  
  output$plot_acf_resid <- renderPlot({
    ggAcf(residual_data()) + theme_minimal() + labs(title = "")
  })
  
  output$uji_formal_resid <- renderPrint({
    cat("--- 1. UJI NORMALITAS (SHAPIRO-WILK) ---\n")
    print(shapiro.test(residual_data()))
    cat("\n--- 2. UJI AUTOKORELASI (LJUNG-BOX) ---\n")
    print(Box.test(residual_data(), type = "Ljung-Box"))
  })
  
  output$kesimpulan_diagnostik <- renderUI({
    p_norm <- shapiro.test(residual_data())$p.value
    p_ljung <- Box.test(residual_data(), type = "Ljung-Box")$p.value
    
    teks_norm <- ifelse(p_norm > 0.05, "<span style='color:green'><b>Normal</b> (Memenuhi Asumsi)</span>", "<span style='color:red'><b>Tidak Normal</b> (Asumsi Dilanggar)</span>")
    teks_ljung <- ifelse(p_ljung > 0.05, "<span style='color:green'><b>Saling Bebas</b> (Memenuhi Asumsi)</span>", "<span style='color:red'><b>Ada Autokorelasi</b> (Asumsi Dilanggar)</span>")
    
    HTML(paste0(
      "<div style='background-color:#fff3e0; padding:15px; border-radius:5px;'>",
      "<h4>🧠 Asisten Kesimpulan Otomatis:</h4>",
      "<ul>",
      "<li><b>Distribusi Residual:</b> ", teks_norm, "</li>",
      "<li><b>Autokorelasi Residual:</b> ", teks_ljung, "</li>",
      "</ul>",
      "<i>Catatan: Model peramalan yang sangat baik harusnya memiliki residual yang Normal dan Saling Bebas (p-value > 0.05). Jika dilanggar, metode ini (SMA) mungkin kurang cocok untuk kerumitan data tersebut.</i>",
      "</div>"
    ))
  })
}
# ############################################################
# UI UTAMA & SERVER UTAMA — RAIHAN
# HANYA RAIHAN YANG BOLEH EDIT BAGIAN INI (integrasi & layout global)
# ############################################################
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Simple Moving Average", titleWidth = 350),
  
  dashboardSidebar(width = 350,
                   sidebarMenu(
                     menuItem("Beranda", tabName = "home", icon = icon("home")),
                     menuItem("Informasi Dataset", tabName = "dataset", icon = icon("database")),
                     menuItem("Eksplorasi Tren", tabName = "eksplorasi", icon = icon("chart-line")),
                     menuItem("Peramalan MA", tabName = "peramalan", icon = icon("calculator")),
                     menuItem("Diagnostik Model", tabName = "diagnostik", icon = icon("book-open"))
                   ),
                   hr(),
                   ui_dataset_sidebar()
  ),
  
  dashboardBody(
    withMathJax(),
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          background-image: url('https://images.unsplash.com/photo-1550895030-823330fc2551?q=80&w=687&auto=format&fit=crop&ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D');
          background-size: cover;
          background-attachment: fixed;
          background-repeat: no-repeat;
          background-position: center center;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 8px 16px rgba(0,0,0,0.3);
          background-color: rgba(255, 255, 255, 0.90) !important;
        }
        .skin-red .main-sidebar { background-color: #2c3e50; }
        .skin-red .sidebar-menu > li.active > a,
        .skin-red .sidebar-menu > li:hover > a {
          background-color: #e67e22 !important;
          color: #fff;
          border-left-color: #f39c12;
        }
        .skin-red .main-header .navbar { background-color: #e74c3c !important; }
        .skin-red .main-header .logo { background-color: #c0392b !important; color: white !important; }
        .skin-red .main-header .logo:hover { background-color: #e74c3c !important; }
        .kotak-input-kustom {
          background-color: rgba(230, 126, 34, 0.2) !important;
          border-top: 3px solid #e67e22 !important;
          border-radius: 8px;
        }
        .kotak-input-kustom .box-header {
          background-color: #e67e22 !important;
          color: white !important;
        }
        .teks-teori { font-size: 16px; line-height: 1.6; text-align: justify; }
      "))
    ),
    tabItems(
      tabItem(tabName = "home",       ui_beranda()),
      tabItem(tabName = "dataset",    ui_dataset()),
      tabItem(tabName = "eksplorasi", ui_eksplorasi()),
      tabItem(tabName = "peramalan",  ui_peramalan()),
      tabItem(tabName = "diagnostik", ui_diagnostik())
    )
  )
)

server <- function(input, output, session) {
  data_proses   <- server_dataset(input, output, session)
  server_eksplorasi(input, output, session, data_proses)
  data_final_ma <- server_peramalan(input, output, session, data_proses)
  server_diagnostik(input, output, session, data_final_ma)
}

shinyApp(ui = ui, server = server)

