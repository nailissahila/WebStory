# ============================================================
# WEB STORY UAS VISUALISASI DATA DAN INFORMASI
# "Indonesia Tidak Seragam:
#  Memetakan Karakteristik Pembangunan Antarwilayah"
#
# Tahun data: 2024
# Unit analisis: Kabupaten/Kota
# ============================================================

# ============================================================
# 1. PACKAGE
# ============================================================

packages <- c(
  "shiny",
  "bslib",
  "shinyjs",
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "plotly",
  "leaflet",
  "sf",
  "scales",
  "DT",
  "htmltools"
)

# Install package jika belum ada
new_packages <- packages[
  !(packages %in% installed.packages()[, "Package"])
]

if(length(new_packages) > 0){
  install.packages(new_packages)
}

# Load package
library(shiny)
library(bslib)
library(shinyjs)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(scales)
library(DT)
library(htmltools)
library(rsconnect)

# ============================================================
# 2. KONFIGURASI
# ============================================================

# Jumlah cluster
K_CLUSTER <- 4

# Folder data
DATA_FILE <- "data/Master Data(1).xlsx"

# File batas wilayah
GEO_FILE <- "data/kabkota_bps.geojson"


# ============================================================
# 3. MEMBACA DATA
# ============================================================

data_raw <- read_excel(
  DATA_FILE,
  sheet = "Sheet1"
)


# ============================================================
# 4. MEMBERSIHKAN DATA
# ============================================================

data <- data_raw %>%
  mutate(
    across(
      where(is.character),
      ~na_if(trimws(.x), "-")
    )
  )


# Variabel PCA
pca_vars <- c(
  "kemiskinan",
  "tpt",
  "rls",
  "hls",
  "uhh",
  "air_minum_layak",
  "sanitasi_layak",
  "pdrb_per_kapita"
)

# Kolom minimum yang harus tersedia agar aplikasi dapat berjalan.
required_data_cols <- c(
  "id_wilayah",
  "provinsi",
  "kab_kota",
  "ipm",
  "jumlah_penduduk",
  pca_vars
)

missing_data_cols <- setdiff(
  required_data_cols,
  names(data)
)

if(length(missing_data_cols) > 0){
  stop(
    paste0(
      "Kolom berikut tidak ditemukan pada data: ",
      paste(missing_data_cols, collapse = ", "),
      ". Periksa file data yang digunakan."
    )
  )
}


# Pastikan variabel numerik
data <- data %>%
  mutate(
    across(
      all_of(pca_vars),
      as.numeric
    )
  )


# ============================================================
# 5. PCA
# ============================================================

# Complete case untuk PCA
data_pca <- data %>%
  filter(
    if_all(
      all_of(pca_vars),
      ~!is.na(.x)
    )
  )

if(K_CLUSTER < 2 || K_CLUSTER >= nrow(data_pca)){
  stop(
    "K_CLUSTER harus minimal 2 dan lebih kecil dari jumlah observasi PCA."
  )
}

# Standardisasi
data_scaled <- scale(
  data_pca %>%
    select(all_of(pca_vars))
)

# PCA
pca_model <- prcomp(
  data_scaled,
  center = FALSE,
  scale. = FALSE
)


# Persentase variance
variance <- pca_model$sdev^2 /
  sum(pca_model$sdev^2) * 100

pc1_var <- round(variance[1], 1)
pc2_var <- round(variance[2], 1)


# PCA scores
scores <- as.data.frame(
  pca_model$x
)

scores$id_wilayah <- data_pca$id_wilayah

scores <- scores %>%
  left_join(
    data_pca %>%
      select(
        id_wilayah,
        provinsi,
        kab_kota
      ),
    by = "id_wilayah"
  )


# ============================================================
# 6. CLUSTERING
# ============================================================

set.seed(123)

cluster_model <- kmeans(
  data_scaled,
  centers = K_CLUSTER,
  nstart = 50
)

scores$cluster <- factor(
  cluster_model$cluster
)


# Gabungkan hasil ke data utama
data_analysis <- data %>%
  left_join(
    scores %>%
      select(
        id_wilayah,
        PC1,
        PC2,
        cluster
      ),
    by = "id_wilayah"
  )


# ============================================================
# 7. LABEL VARIABEL
# ============================================================

variable_labels <- c(
  kemiskinan = "Kemiskinan (%)",
  tpt = "TPT (%)",
  rls = "RLS (tahun)",
  hls = "HLS (tahun)",
  uhh = "UHH (tahun)",
  air_minum_layak = "Air minum layak (%)",
  sanitasi_layak = "Sanitasi layak (%)",
  pdrb_per_kapita = "PDRB per kapita (juta rupiah)"
)


# ============================================================
# 8. LOAD PETA
# ============================================================

# Fungsi kecil untuk menyamakan format kode BPS sebelum join.
normalize_bps_code <- function(x){
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

map_available <- file.exists(GEO_FILE)
wilayah_sf <- NULL

if(map_available){
  
  wilayah_sf <- st_read(
    GEO_FILE,
    quiet = TRUE
  )
  
  # Gunakan kode_bps jika tersedia.
  # Jika GeoJSON menggunakan nama KDBBPS, salin ke kode_bps.
  if("kode_bps" %in% names(wilayah_sf)){
    wilayah_sf$kode_bps <-
      normalize_bps_code(wilayah_sf$kode_bps)
    
  } else if("KDBBPS" %in% names(wilayah_sf)){
    wilayah_sf$kode_bps <-
      normalize_bps_code(wilayah_sf$KDBBPS)
    
  } else {
    warning(
      "GeoJSON tidak memiliki kolom kode_bps atau KDBBPS. ",
      "Peta tidak akan ditampilkan."
    )
    map_available <- FALSE
    wilayah_sf <- NULL
  }
  
  # Join geospasial membutuhkan kode BPS pada data statistik.
  if(map_available && !"kode_bps" %in% names(data)){
    warning(
      "Data statistik tidak memiliki kolom kode_bps. ",
      "Peta tidak akan ditampilkan sampai kode BPS tersedia."
    )
    map_available <- FALSE
    wilayah_sf <- NULL
  }
  
  if(map_available){
    
    data$kode_bps <-
      normalize_bps_code(data$kode_bps)
    
    # Pastikan satu kode hanya mewakili satu wilayah.
    if(anyDuplicated(data$kode_bps[!is.na(data$kode_bps)])){
      stop(
        "Terdapat kode_bps duplikat pada data statistik. ",
        "Periksa kode wilayah sebelum menjalankan aplikasi."
      )
    }
    
    if(anyDuplicated(wilayah_sf$kode_bps[!is.na(wilayah_sf$kode_bps)])){
      stop(
        "Terdapat kode_bps duplikat pada GeoJSON. ",
        "Periksa file batas wilayah sebelum menjalankan aplikasi."
      )
    }
    
    # Hanya fitur yang mempunyai kode BPS yang dapat digunakan untuk join.
    wilayah_sf <- wilayah_sf %>%
      filter(!is.na(kode_bps))
  }
  
} else {
  
  wilayah_sf <- NULL
  
}


# ============================================================
# 9. UI
# ============================================================

ui <- fluidPage(
  
  title = "Indonesia Tidak Seragam",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#244A7C",
    secondary = "#6C757D",
    base_font = font_google(
      "Inter"
    )
  ),
  
  useShinyjs(),
  
  tags$head(
    
    # Agar layout pas di layar handphone
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1, viewport-fit=cover"
    ),
    
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0("style.css?v=", as.integer(Sys.time()))
    ),
    
    # Logika pergantian slide (file www/story.js)
    tags$script(
      src = paste0("story.js?v=", as.integer(Sys.time()))
    )
    
  ),
  
  
  # ==========================================================
  # DECK: setiap div langsung di dalam sini menjadi 1 slide
  # ==========================================================
  
  div(
    id = "story-deck",
    
    # ==========================================================
    # HERO
    # ==========================================================
    
    div(
      class = "hero-section",
      
      div(
        class = "hero-content",
        
        h1(
          "Indonesia Tidak Seragam"
        ),
        
        h2(
          "Memetakan Karakteristik Pembangunan Antarwilayah"
        ),
        
        p(
          class = "hero-description",
          paste0(
            "Indonesia terdiri dari ratusan kabupaten/kota ",
            "dengan karakteristik pembangunan yang berbeda. ",
            "Web story ini menggunakan data BPS tahun 2024 ",
            "untuk melihat bagaimana perbedaan tersebut ",
            "terbentuk dan tersebar di seluruh Indonesia."
          )
        ),
        
        div(
          class = "hero-stat",
          
          div(
            class = "stat-box",
            h3("514"),
            p("Kabupaten/Kota")
          ),
          
          div(
            class = "stat-box",
            h3("8"),
            p("Indikator")
          ),
          
          div(
            class = "stat-box",
            h3("2024"),
            p("Tahun Data")
          )
          
        )
        
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # INTRO
    # ----------------------------------------------------------
    
    div(
      class = "story-section",
      
      h2(
        "Apakah pembangunan Indonesia memiliki satu wajah?"
      ),
      
      p(
        "Indeks Pembangunan Manusia memberikan gambaran ",
        "penting mengenai pembangunan manusia. Namun satu ",
        "angka tidak cukup untuk menggambarkan kompleksitas ",
        "suatu wilayah."
      ),
      
      p(
        "Karena itu, analisis ini melihat pembangunan melalui ",
        "delapan indikator yang mencakup kemiskinan, ",
        "ketenagakerjaan, pendidikan, kesehatan, akses air ",
        "minum, sanitasi, dan kondisi ekonomi."
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # MAP IPM
    # ----------------------------------------------------------
    
    div(
      class = "story-section",
      
      h2(
        "Pembangunan tidak tersebar secara merata"
      ),
      
      p(
        "Peta berikut menunjukkan persebaran Indeks ",
        "Pembangunan Manusia (IPM) antar kabupaten/kota."
      ),
      
      if(map_available){
        
        leafletOutput(
          "map_ipm",
          height = "60vh"
        )
        
      } else {
        
        div(
          class = "warning-box",
          icon("triangle-exclamation"),
          " File batas wilayah belum tersedia. ",
          "Simpan GeoJSON kabupaten/kota dengan kode BPS ",
          "di folder data/ dengan nama ",
          "kabkota_bps.geojson."
        )
        
      },
      
      br(),
      
      div(
        class = "source-text",
        "Sumber: BPS, data tahun 2024."
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # ONE NUMBER IS NOT ENOUGH
    # ----------------------------------------------------------
    
    div(
      class = "story-section",
      
      h2(
        "Satu angka tidak cukup"
      ),
      
      p(
        "Wilayah dengan IPM yang sama belum tentu memiliki ",
        "karakteristik pembangunan yang sama. Delapan indikator ",
        "digunakan secara bersamaan untuk menangkap dimensi ",
        "pembangunan yang lebih luas."
      ),
      
      fluidRow(
        
        column(
          width = 6,
          
          plotlyOutput(
            "correlation",
            height = "52vh"
          )
          
        ),
        
        column(
          width = 6,
          
          div(
            class = "info-card",
            
            h3(
              "Delapan indikator"
            ),
            
            tags$ul(
              
              tags$li(
                "Persentase penduduk miskin"
              ),
              
              tags$li(
                "Tingkat pengangguran terbuka"
              ),
              
              tags$li(
                "Rata-rata lama sekolah"
              ),
              
              tags$li(
                "Harapan lama sekolah"
              ),
              
              tags$li(
                "Umur harapan hidup"
              ),
              
              tags$li(
                "Akses air minum layak"
              ),
              
              tags$li(
                "Akses sanitasi layak"
              ),
              
              tags$li(
                "PDRB per kapita"
              )
              
            )
            
          )
          
        )
        
      ),
      
      div(
        class = "source-text",
        "Sumber: BPS, data tahun 2024."
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # PCA
    # ----------------------------------------------------------
    
    div(
      class = "story-section",
      
      h2(
        "Jika indikator digabung, pola apa yang muncul?"
      ),
      
      p(
        "Principal Component Analysis (PCA) digunakan untuk ",
        "meringkas delapan indikator menjadi beberapa dimensi ",
        "utama. Titik yang berdekatan menunjukkan kabupaten/kota ",
        "dengan karakteristik multivariat yang relatif mirip."
      ),
      
      fluidRow(
        
        column(
          width = 9,
          
          plotlyOutput(
            "pca_plot",
            height = "60vh"
          )
          
        ),
        
        column(
          width = 3,
          
          div(
            class = "info-card",
            
            h3("PCA"),
            
            p(
              paste0(
                "PC1 menjelaskan ",
                pc1_var,
                "% variasi data."
              )
            ),
            
            p(
              paste0(
                "PC2 menjelaskan ",
                pc2_var,
                "% variasi data."
              )
            ),
            
            hr(),
            
            p(
              "Gunakan fitur select pada grafik untuk ",
              "memilih beberapa kabupaten/kota."
            )
            
          )
          
        )
        
      ),
      
      div(
        class = "source-text",
        "Sumber: BPS, diolah menggunakan PCA."
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # CLUSTER
    # ----------------------------------------------------------
    
    div(
      class = "story-section",
      
      h2(
        "Indonesia memiliki beberapa tipe karakteristik"
      ),
      
      p(
        "Hasil clustering mengelompokkan kabupaten/kota ",
        "berdasarkan kemiripan delapan indikator pembangunan."
      ),
      
      fluidRow(
        
        column(
          width = 8,
          
          plotlyOutput(
            "parallel",
            height = "56vh"
          )
          
        ),
        
        column(
          width = 4,
          
          selectInput(
            "cluster_profile",
            "Pilih cluster:",
            choices = paste0(
              "Cluster ",
              seq_len(K_CLUSTER)
            )
          ),
          
          plotlyOutput(
            "cluster_profile_plot",
            height = "48vh"
          )
          
        )
        
      ),
      
      div(
        class = "source-text",
        "Sumber: BPS, diolah menggunakan PCA dan K-Means."
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # LINKED MAP
    # ----------------------------------------------------------
    
    div(
      class = "story-section",
      
      h2(
        "Di mana kelompok-kelompok tersebut berada?"
      ),
      
      p(
        "Hasil pengelompokan tidak hanya dapat dilihat ",
        "sebagai titik dalam grafik. Kelompok tersebut juga ",
        "memiliki pola spasial. Klik suatu wilayah pada peta ",
        "atau pilih titik pada grafik PCA untuk melihat ",
        "keterkaitannya."
      ),
      
      if(map_available){
        
        leafletOutput(
          "map_cluster",
          height = "60vh"
        )
        
      } else {
        
        div(
          class = "warning-box",
          "Peta cluster membutuhkan file GeoJSON ",
          "kabupaten/kota dengan kode BPS."
        )
        
      },
      
      br(),
      
      plotlyOutput(
        "selected_pca",
        height = "48vh"
      ),
      
      div(
        class = "selected-info",
        textOutput(
          "selected_region"
        )
      ),
      
      div(
        class = "source-text",
        "Sumber: BPS, diolah menggunakan clustering."
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # SECOND MAP TYPE
    # ----------------------------------------------------------
    
    div(
      class = "story-section",
      
      h2(
        "Besarnya ekonomi tidak selalu berarti pembangunan seragam"
      ),
      
      p(
        "Peta kedua menggunakan simbol proporsional untuk ",
        "menunjukkan PDRB per kapita. Ukuran lingkaran ",
        "merepresentasikan nilai PDRB per kapita."
      ),
      
      if(map_available){
        
        leafletOutput(
          "map_pdrb",
          height = "60vh"
        )
        
      } else {
        
        div(
          class = "warning-box",
          "Peta membutuhkan file GeoJSON kabupaten/kota."
        )
        
      },
      
      div(
        class = "source-text",
        "Sumber: BPS, data tahun 2024."
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # HIERARCHY
    # ----------------------------------------------------------
    
    div(
      class = "story-section",
      
      h2(
        "Melihat pembangunan sebagai sebuah struktur"
      ),
      
      p(
        "Karakteristik pembangunan juga dapat dilihat secara ",
        "hierarkis: provinsi → kabupaten/kota → kelompok pembangunan",
        "Ukuran area menunjukkan jumlah penduduk, sedangkan ",
        "warna menunjukkan tingkat kemiskinan."
      ),
      
      fluidRow(
        
        column(
          width = 6,
          
          h4(
            "Treemap"
          ),
          
          plotlyOutput(
            "treemap",
            height = "60vh"
          )
          
        ),
        
        column(
          width = 6,
          
          h4(
            "Sunburst"
          ),
          
          plotlyOutput(
            "sunburst",
            height = "60vh"
          )
          
        )
        
      ),
      
      div(
        class = "source-text",
        "Sumber: BPS, data tahun 2024."
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # DATA EXPLORER
    # ----------------------------------------------------------
    
    div(
      class = "story-section",
      
      h2(
        "Jelajahi kabupaten/kota"
      ),
      
      p(
        "Gunakan tabel berikut untuk menemukan wilayah ",
        "tertentu dan membandingkan karakteristiknya."
      ),
      
      fluidRow(
        
        column(
          width = 4,
          
          selectInput(
            "province_filter",
            "Provinsi:",
            choices = c(
              "Semua",
              sort(unique(data$provinsi))
            )
          )
          
        ),
        
        column(
          width = 4,
          
          selectInput(
            "cluster_filter",
            "Cluster:",
            choices = c(
              "Semua",
              paste0(
                "Cluster ",
                seq_len(K_CLUSTER)
              )
            )
          )
          
        )
        
      ),
      
      DTOutput(
        "data_table"
      ),
      
      div(
        class = "source-text",
        "Sumber: BPS, data tahun 2024."
      )
      
    ),
    
    
    # ----------------------------------------------------------
    # CLOSING
    # ----------------------------------------------------------
    
    div(
      class = "closing-section",
      
      h2(
        "Indonesia tidak memiliki satu wajah pembangunan."
      ),
      
      p(
        "Perbedaan pembangunan antarwilayah tidak hanya ",
        "terlihat dari satu indikator. Ketika berbagai ",
        "indikator dianalisis secara bersamaan, muncul ",
        "kelompok-kelompok wilayah dengan karakteristik ",
        "yang berbeda."
      ),
      
      p(
        "Dengan menggabungkan analisis multivariat, ",
        "pemetaan geospasial, dan struktur hierarkis, ",
        "perbedaan tersebut dapat dilihat secara lebih ",
        "utuh."
      ),
      
      hr(),
      
      p(
        class = "source-text",
        "Sumber utama: Badan Pusat Statistik (BPS). ",
        "Data tahun 2024."
      )
      
    )
    
  )
  
)

# ============================================================
# 10. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # CORRELATION
  # ==========================================================
  
  output$correlation <- renderPlotly({
    
    cor_data <- data_pca %>%
      select(all_of(pca_vars)) %>%
      cor(
        use = "pairwise.complete.obs"
      )
    
    colnames(cor_data) <-
      variable_labels[colnames(cor_data)]
    
    rownames(cor_data) <-
      variable_labels[rownames(cor_data)]
    
    plot_ly(
      x = colnames(cor_data),
      y = rownames(cor_data),
      z = cor_data,
      type = "heatmap",
      zmin = -1,
      zmax = 1,
      colorscale = "RdBu",
      reversescale = TRUE,
      text = round(cor_data, 2),
      texttemplate = "%{text}",
      hovertemplate =
        "%{y}<br>%{x}<br>Korelasi: %{z:.2f}<extra></extra>"
    ) %>%
      
      layout(
        
        title = "Korelasi antar indikator",
        
        xaxis = list(
          tickangle = -45
        ),
        
        yaxis = list(
          automargin = TRUE
        )
        
      )
    
  })
  
  
  # ==========================================================
  # PCA PLOT
  # ==========================================================
  
  output$pca_plot <- renderPlotly({
    
    plot_ly(
      
      data = scores,
      
      x = ~PC1,
      y = ~PC2,
      
      type = "scatter",
      mode = "markers",
      
      source = "pca",
      
      key = ~id_wilayah,
      
      color = ~cluster,
      
      colors = "Set2",
      
      text = ~paste0(
        "<b>",
        kab_kota,
        "</b>",
        "<br>Provinsi: ",
        provinsi,
        "<br>Cluster: ",
        cluster,
        "<br>PC1: ",
        round(PC1, 2),
        "<br>PC2: ",
        round(PC2, 2)
      ),
      
      hoverinfo = "text",
      
      marker = list(
        size = 9,
        opacity = 0.75
      )
      
    ) %>%
      
      layout(
        
        title = "Peta karakteristik multivariat",
        
        xaxis = list(
          title = paste0(
            "PC1 (",
            pc1_var,
            "%)"
          )
        ),
        
        yaxis = list(
          title = paste0(
            "PC2 (",
            pc2_var,
            "%)"
          )
        ),
        
        legend = list(
          title = list(
            text = "Cluster"
          )
        ),
        
        dragmode = "select"
        
      ) %>%
      
      config(
        displaylogo = FALSE
      )
    
  })
  
  
  # ==========================================================
  # PARALLEL COORDINATES
  # ==========================================================
  
  output$parallel <- renderPlotly({
    
    parallel_data <- data_analysis %>%
      
      filter(
        !is.na(cluster)
      ) %>%
      
      select(
        kab_kota,
        cluster,
        all_of(pca_vars)
      ) %>%
      
      mutate(
        across(
          all_of(pca_vars),
          ~as.numeric(
            scale(.x)
          )
        )
      )
    
    plot_ly(
      type = "parcoords",
      
      line = list(
        color = as.numeric(
          parallel_data$cluster
        ),
        colorscale = "Viridis"
      ),
      
      dimensions = lapply(
        seq_along(pca_vars),
        function(i){
          
          list(
            label = variable_labels[
              pca_vars[i]
            ],
            
            values =
              parallel_data[[pca_vars[i]]]
          )
          
        }
      )
      
    ) %>%
      
      layout(
        title =
          "Profil relatif setiap kabupaten/kota"
      )
    
  })
  
  
  # ==========================================================
  # CLUSTER PROFILE
  # ==========================================================
  
  output$cluster_profile_plot <- renderPlotly({
    
    req(
      input$cluster_profile
    )
    
    selected_cluster <- as.numeric(
      gsub(
        "Cluster ",
        "",
        input$cluster_profile
      )
    )
    
    profile <- data_analysis %>%
      
      filter(
        as.character(cluster) == as.character(selected_cluster)
      ) %>%
      
      summarise(
        across(
          all_of(pca_vars),
          ~mean(
            .x,
            na.rm = TRUE
          )
        )
      ) %>%
      
      pivot_longer(
        everything(),
        names_to = "variable",
        values_to = "value"
      )
    
    plot_ly(
      profile,
      
      x = ~value,
      y = ~reorder(
        variable,
        value
      ),
      
      type = "bar",
      
      orientation = "h",
      
      text = ~round(
        value,
        2
      ),
      
      hovertemplate =
        "%{y}<br>Nilai: %{x:.2f}<extra></extra>"
    ) %>%
      
      layout(
        title =
          paste(
            "Profil",
            input$cluster_profile
          ),
        
        xaxis = list(
          title = "Nilai rata-rata"
        ),
        
        yaxis = list(
          title = ""
        )
      )
    
  })
  
  
  # ==========================================================
  # MAP IPM
  # ==========================================================
  
  if(map_available){
    
    map_data <- wilayah_sf %>%
      
      left_join(
        data_analysis,
        by = "kode_bps"
      )
    
    
    output$map_ipm <- renderLeaflet({
      
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = map_data$ipm,
        na.color = "#eeeeee"
      )
      
      leaflet(
        map_data
      ) %>%
        
        addProviderTiles(
          providers$CartoDB.Positron
        ) %>%
        
        addPolygons(
          
          fillColor =
            ~pal(ipm),
          
          fillOpacity = 0.75,
          
          color = "white",
          
          weight = 0.7,
          
          layerId = ~id_wilayah,
          
          label = ~htmltools::HTML(
            paste0(
              "<b>",
              kab_kota,
              "</b><br>",
              "IPM: ",
              round(ipm, 2)
            )
          ),
          
          popup = ~paste0(
            
            "<b>",
            kab_kota,
            "</b><br>",
            
            "Provinsi: ",
            provinsi,
            "<br>",
            
            "IPM: ",
            round(ipm, 2),
            "<br>",
            
            "Kemiskinan: ",
            round(kemiskinan, 2),
            "%<br>",
            
            "PDRB per kapita: ",
            round(
              pdrb_per_kapita,
              2
            ),
            " juta"
            
          ),
          
          highlightOptions =
            highlightOptions(
              weight = 3,
              color = "#222222",
              bringToFront = TRUE
            )
          
        ) %>%
        
        addLegend(
          pal = pal,
          values = ~ipm,
          title = "IPM",
          position = "bottomright"
        ) %>%
        
        addLayersControl(
          baseGroups = c(
            "CartoDB"
          ),
          options =
            layersControlOptions(
              collapsed = FALSE
            )
        )
      
    })
    
  }
  
  
  # ==========================================================
  # SELECTED REGION
  # ==========================================================
  
  selected_region_id <-
    reactiveVal(NULL)
  
  
  # MAP → PCA
  if(map_available){
    
    observeEvent(
      input$map_cluster_shape_click,
      {
        
        click <-
          input$map_cluster_shape_click
        
        if(!is.null(click$id)){
          
          selected_region_id(
            click$id
          )
          
        }
        
      }
    )
    
  }
  
  
  # PCA → MAP
  observeEvent(
    
    event_data(
      "plotly_selected",
      source = "pca"
    ),
    
    {
      
      event <- event_data(
        "plotly_selected",
        source = "pca"
      )
      
      if(
        !is.null(event) &&
        nrow(event) > 0
      ){
        
        selected_region_id(
          event$key[1]
        )
        
      }
      
    }
    
  )
  
  
  # ==========================================================
  # SELECTED PCA
  # ==========================================================
  
  output$selected_pca <- renderPlotly({
    
    selected_id <-
      selected_region_id()
    
    plot_data <-
      scores
    
    plot_ly(
      
      data = plot_data,
      
      x = ~PC1,
      y = ~PC2,
      
      type = "scatter",
      
      mode = "markers",
      
      color = ~cluster,
      
      colors = "Set2",
      
      text = ~paste0(
        "<b>",
        kab_kota,
        "</b><br>",
        provinsi
      ),
      
      hoverinfo = "text",
      
      marker = list(
        size = 8,
        opacity = 0.55
      )
      
    ) %>%
      
      layout(
        
        title =
          "Posisi kabupaten/kota pada PCA",
        
        xaxis = list(
          title = paste0(
            "PC1 (",
            pc1_var,
            "%)"
          )
        ),
        
        yaxis = list(
          title = paste0(
            "PC2 (",
            pc2_var,
            "%)"
          )
        )
        
      )
    
  })
  
  
  # ==========================================================
  # SELECTED REGION TEXT
  # ==========================================================
  
  output$selected_region <- renderText({
    
    req(
      selected_region_id()
    )
    
    d <-
      data_analysis %>%
      filter(
        id_wilayah ==
          selected_region_id()
      )
    
    if(nrow(d) == 0){
      
      return(
        "Belum ada wilayah yang dipilih."
      )
      
    }
    
    paste0(
      
      "Wilayah terpilih: ",
      d$kab_kota,
      
      " | Provinsi: ",
      d$provinsi,
      
      " | Cluster: ",
      d$cluster,
      
      " | IPM: ",
      round(
        d$ipm,
        2
      ),
      
      " | Kemiskinan: ",
      round(
        d$kemiskinan,
        2
      ),
      "%"
      
    )
    
  })
  
  
  # ==========================================================
  # CLUSTER MAP
  # ==========================================================
  
  if(map_available){
    
    output$map_cluster <- renderLeaflet({
      
      map_cluster_data <-
        map_data
      
      pal_cluster <-
        colorFactor(
          palette = "Set2",
          domain =
            map_cluster_data$cluster,
          na.color = "#eeeeee"
        )
      
      leaflet(
        map_cluster_data
      ) %>%
        
        addProviderTiles(
          providers$CartoDB.Positron
        ) %>%
        
        addPolygons(
          
          fillColor =
            ~pal_cluster(cluster),
          
          fillOpacity = 0.75,
          
          color = "white",
          
          weight = 0.7,
          
          layerId =
            ~id_wilayah,
          
          label = ~htmltools::HTML(
            paste0(
              "<b>",
              kab_kota,
              "</b><br>",
              "Cluster: ",
              cluster
            )
          ),
          
          popup = ~paste0(
            "<b>",
            kab_kota,
            "</b><br>",
            "Provinsi: ",
            provinsi,
            "<br>",
            "Cluster: ",
            cluster,
            "<br>",
            "IPM: ",
            round(ipm, 2),
            "<br>",
            "PDRB per kapita: ",
            round(
              pdrb_per_kapita,
              2
            ),
            " juta"
          ),
          
          highlightOptions =
            highlightOptions(
              weight = 3,
              color = "#222222",
              bringToFront = TRUE
            )
          
        ) %>%
        
        addLegend(
          pal = pal_cluster,
          values =
            ~cluster,
          title = "Cluster",
          position = "bottomright"
        )
      
    })
    
    
    # Highlight wilayah terpilih
    observe({
      
      req(
        selected_region_id()
      )
      
      leafletProxy(
        "map_cluster"
      ) %>%
        
        clearGroup(
          "selected"
        ) %>%
        
        addPolygons(
          
          data =
            map_data %>%
            filter(
              id_wilayah ==
                selected_region_id()
            ),
          
          group = "selected",
          
          fill = FALSE,
          
          color = "#111111",
          
          weight = 5,
          
          opacity = 1
          
        )
      
    })
    
  }
  
  
  # ==========================================================
  # PDRB PROPORTIONAL SYMBOL MAP
  # ==========================================================
  
  if(map_available){
    
    output$map_pdrb <- renderLeaflet({
      
      map_pdrb_data <-
        map_data %>%
        filter(
          !is.na(pdrb_per_kapita)
        )
      
      # Titik simbol dihitung sekali agar koordinat konsisten.
      point_geom <- st_point_on_surface(st_geometry(map_pdrb_data))
      point_coords <- st_coordinates(point_geom)
      
      map_pdrb_data$lng <- point_coords[, 1]
      map_pdrb_data$lat <- point_coords[, 2]
      
      leaflet(
        map_pdrb_data
      ) %>%
        
        addProviderTiles(
          providers$CartoDB.Positron
        ) %>%
        
        addPolygons(
          
          fill = FALSE,
          
          color = "#999999",
          
          weight = 0.5,
          
          opacity = 0.7
          
        ) %>%
        
        addCircleMarkers(
          
          lng = ~lng,
          
          lat = ~lat,
          
          radius =
            ~sqrt(
              pdrb_per_kapita
            ) / 2,
          
          stroke = TRUE,
          
          weight = 1,
          
          fillOpacity = 0.55,
          
          label = ~paste0(
            kab_kota,
            ": ",
            round(
              pdrb_per_kapita,
              2
            ),
            " juta rupiah"
          ),
          
          popup = ~paste0(
            
            "<b>",
            kab_kota,
            "</b><br>",
            
            "PDRB per kapita: ",
            
            round(
              pdrb_per_kapita,
              2
            ),
            
            " juta rupiah"
            
          )
          
        ) %>%
        
        addLegend(
          
          position = "bottomright",
          
          title = "PDRB per kapita",
          
          colors = "#3182bd",
          
          labels =
            c(
              "Semakin besar lingkaran = semakin tinggi"
            )
          
        )
      
    })
    
  }
  
  
  # ==========================================================
  # HIERARCHICAL DATA
  # ==========================================================
  
  hierarchy_data <-
    data_analysis %>%
    
    filter(
      !is.na(cluster),
      !is.na(pdrb_per_kapita),
      !is.na(kemiskinan)
    ) %>%
    
    mutate(
      
      level_cluster =
        paste0(
          "Cluster ",
          cluster
        )
      
    )
  
  
  # ==========================================================
  # TREEMAP
  # ==========================================================
  
  output$treemap <- renderPlotly({
    
    # Buat node provinsi, cluster, dan kabupaten/kota secara eksplisit.
    # Dengan demikian setiap parent benar-benar mempunyai node pada hierarchy.
    tree_province <- hierarchy_data %>%
      group_by(provinsi) %>%
      summarise(
        values = sum(jumlah_penduduk, na.rm = TRUE),
        poverty = weighted.mean(
          kemiskinan,
          w = jumlah_penduduk,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      transmute(
        ids = provinsi,
        labels = provinsi,
        parents = "",
        values = values,
        poverty = poverty
      )
    
    tree_cluster <- hierarchy_data %>%
      group_by(provinsi, cluster) %>%
      summarise(
        population = sum(jumlah_penduduk, na.rm = TRUE),
        poverty = weighted.mean(
          kemiskinan,
          w = jumlah_penduduk,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      mutate(
        ids = paste(provinsi, cluster, sep = " / "),
        labels = paste0("Cluster ", cluster),
        parents = provinsi,
        values = population
      ) %>%
      select(ids, labels, parents, values, poverty)
    
    tree_region <- hierarchy_data %>%
      transmute(
        ids = paste(
          provinsi,
          cluster,
          kab_kota,
          sep = " / "
        ),
        labels = kab_kota,
        parents = paste(
          provinsi,
          cluster,
          sep = " / "
        ),
        values = jumlah_penduduk,
        poverty = kemiskinan
      )
    
    tree_data <- bind_rows(
      tree_province,
      tree_cluster,
      tree_region
    )
    
    plot_ly(
      data = tree_data,
      type = "treemap",
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      branchvalues = "total",
      marker = list(
        colors = ~poverty,
        colorscale = "YlOrRd",
        colorbar = list(
          title = "Kemiskinan (%)"
        )
      ),
      textinfo = "label+value",
      hovertemplate =
        paste0(
          "<b>%{label}</b><br>",
          "Jumlah penduduk: %{value:,.0f}<br>",
          "Kemiskinan: %{color:.2f}%<extra></extra>"
        )
    ) %>%
      
      layout(
        title =
          "Struktur wilayah berdasarkan provinsi dan cluster"
      )
    
  })
  
  
  # ==========================================================
  # SUNBURST
  # ==========================================================
  
  output$sunburst <- renderPlotly({
    
    # Struktur hierarchy yang sama dengan treemap.
    sun_province <- hierarchy_data %>%
      group_by(provinsi) %>%
      summarise(
        values = sum(jumlah_penduduk, na.rm = TRUE),
        poverty = weighted.mean(
          kemiskinan,
          w = jumlah_penduduk,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      transmute(
        ids = provinsi,
        labels = provinsi,
        parents = "",
        values = values,
        poverty = poverty
      )
    
    sun_cluster <- hierarchy_data %>%
      group_by(provinsi, cluster) %>%
      summarise(
        values = sum(jumlah_penduduk, na.rm = TRUE),
        poverty = weighted.mean(
          kemiskinan,
          w = jumlah_penduduk,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      transmute(
        ids = paste(provinsi, cluster, sep = " / "),
        labels = paste0("Cluster ", cluster),
        parents = provinsi,
        values = values,
        poverty = poverty
      )
    
    sun_region <- hierarchy_data %>%
      transmute(
        ids = paste(
          provinsi,
          cluster,
          kab_kota,
          sep = " / "
        ),
        labels = kab_kota,
        parents = paste(
          provinsi,
          cluster,
          sep = " / "
        ),
        values = jumlah_penduduk,
        poverty = kemiskinan
      )
    
    sun_data <- bind_rows(
      sun_province,
      sun_cluster,
      sun_region
    )
    
    plot_ly(
      data = sun_data,
      type = "sunburst",
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      branchvalues = "total",
      marker = list(
        colors = ~poverty,
        colorscale = "YlOrRd",
        colorbar = list(
          title = "Kemiskinan (%)"
        )
      ),
      hovertemplate =
        paste0(
          "<b>%{label}</b><br>",
          "Jumlah penduduk: %{value:,.0f}<br>",
          "Kemiskinan: %{color:.2f}%<extra></extra>"
        )
    ) %>%
      
      layout(
        title =
          "Hierarki pembangunan wilayah"
      )
    
  })
  
  
  # ==========================================================
  # DATA TABLE
  # ==========================================================
  
  filtered_data <-
    reactive({
      
      d <- data_analysis
      
      if(
        input$province_filter !=
        "Semua"
      ){
        
        d <-
          d %>%
          filter(
            provinsi ==
              input$province_filter
          )
        
      }
      
      if(
        input$cluster_filter !=
        "Semua"
      ){
        
        selected_cluster <-
          as.numeric(
            gsub(
              "Cluster ",
              "",
              input$cluster_filter
            )
          )
        
        d <-
          d %>%
          filter(
            as.character(cluster) ==
              as.character(selected_cluster)
          )
        
      }
      
      d
      
    })
  
  
  output$data_table <-
    renderDT({
      
      filtered_data() %>%
        
        select(
          provinsi,
          kab_kota,
          ipm,
          kemiskinan,
          tpt,
          rls,
          hls,
          uhh,
          air_minum_layak,
          sanitasi_layak,
          pdrb_per_kapita,
          cluster
        ) %>%
        
        rename(
          
          Provinsi =
            provinsi,
          
          `Kabupaten/Kota` =
            kab_kota,
          
          IPM =
            ipm,
          
          `Kemiskinan (%)` =
            kemiskinan,
          
          `TPT (%)` =
            tpt,
          
          `RLS (tahun)` =
            rls,
          
          `HLS (tahun)` =
            hls,
          
          `UHH (tahun)` =
            uhh,
          
          `Air minum layak (%)` =
            air_minum_layak,
          
          `Sanitasi layak (%)` =
            sanitasi_layak,
          
          `PDRB per kapita (juta)` =
            pdrb_per_kapita,
          
          Cluster =
            cluster
          
        ) %>%
        
        datatable(
          
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            language = list(
              search = "Cari:",
              lengthMenu =
                "Tampilkan _MENU_ wilayah",
              info =
                "Menampilkan _START_–_END_ dari _TOTAL_ wilayah"
            )
          )
          
        ) %>%
        
        formatRound(
          columns = c(
            "IPM",
            "Kemiskinan (%)",
            "TPT (%)",
            "RLS (tahun)",
            "HLS (tahun)",
            "UHH (tahun)",
            "Air minum layak (%)",
            "Sanitasi layak (%)",
            "PDRB per kapita (juta)"
          ),
          digits = 2
        )
      
    })
  
}


# ============================================================
# 11. RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)