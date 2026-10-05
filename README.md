# Indonesia Tidak Seragam: Memetakan Karakteristik Pembangunan Antarwilayah

Web story interaktif yang memvisualisasikan perbedaan karakteristik
pembangunan pada tingkat kabupaten/kota di Indonesia menggunakan data
Badan Pusat Statistik (BPS) tahun 2024.

## 1. Tentang Proyek

Proyek ini dibuat untuk memenuhi tugas **Visualisasi Data dan
Informasi** dengan menggabungkan tiga topik visualisasi:

-   **Multivariate / High-dimensional**
-   **Geospatial**
-   **Hierarchical**

Pertanyaan utama yang ingin dijawab:

> **Apakah seluruh kabupaten/kota di Indonesia memiliki karakteristik
> pembangunan yang sama?**

Web story disajikan dalam satu halaman yang dapat di-scroll sehingga
pengguna dapat mengikuti alur cerita dari gambaran umum pembangunan,
eksplorasi karakteristik multivariat, hingga struktur hierarki wilayah.

## 2. Tujuan

Proyek ini bertujuan untuk:

1.  Memvisualisasikan keragaman karakteristik pembangunan antar
    kabupaten/kota.
2.  Menggabungkan beberapa indikator pembangunan dalam analisis
    multivariat.
3.  Mengelompokkan kabupaten/kota berdasarkan kemiripan karakteristik
    pembangunan.
4.  Menampilkan hasil pengelompokan secara geografis.
5.  Menyajikan struktur wilayah melalui visualisasi hierarki.
6.  Menyediakan visualisasi interaktif yang mudah dieksplorasi.

## 3. Data

Data utama berasal dari **Badan Pusat Statistik (BPS)** dengan periode
data 2024. Unit analisis adalah kabupaten/kota di Indonesia.

Dataset akhir terdiri dari **514 kabupaten/kota**.

### Variabel

  Variabel            Keterangan                     Satuan
  ------------------- ------------------------------ -------------
  `jumlah_penduduk`   Jumlah penduduk                jiwa
  `ipm`               Indeks Pembangunan Manusia     indeks
  `kemiskinan`        Persentase penduduk miskin     persen
  `tpt`               Tingkat Pengangguran Terbuka   persen
  `rls`               Rata-rata Lama Sekolah         tahun
  `hls`               Harapan Lama Sekolah           tahun
  `uhh`               Umur Harapan Hidup             tahun
  `air_minum_layak`   Akses air minum layak          persen
  `sanitasi_layak`    Akses sanitasi layak           persen
  `pdrb_per_kapita`   PDRB per kapita                juta rupiah

Delapan variabel yang digunakan dalam PCA dan clustering adalah:

-   Kemiskinan
-   TPT
-   RLS
-   HLS
-   UHH
-   Air minum layak
-   Sanitasi layak
-   PDRB per kapita

IPM tidak digunakan sebagai input PCA/clustering karena merupakan indeks
komposit. Jumlah penduduk juga tidak digunakan sebagai input
PCA/clustering.

## 4. Metodologi

Alur analisis:

``` text
Data BPS 2024
      ↓
Data Cleaning & Integration
      ↓
Exploratory Data Analysis
      ↓
Standardisasi Variabel
      ↓
Principal Component Analysis (PCA)
      ↓
K-Means Clustering
      ↓
Profil Cluster
      ↓
Integrasi Data Geospasial
      ↓
Visualisasi Peta
      ↓
Visualisasi Hierarki
      ↓
Web Story Interaktif
```

### 4.1 PCA

PCA digunakan untuk mereduksi delapan indikator pembangunan menjadi
beberapa komponen utama.

Sebelum PCA, variabel distandardisasi agar perbedaan skala tidak
mendominasi hasil.

Hasil utama:

-   PC1 menjelaskan sekitar **45,93%** variasi data.
-   PC2 menjelaskan sekitar **12,12%** variasi data.
-   PC1 dan PC2 secara kumulatif menjelaskan sekitar **58,06%** variasi
    data.

### 4.2 K-Means Clustering

K-Means digunakan untuk mengelompokkan wilayah berdasarkan karakteristik
indikator pembangunan.

Implementasi menggunakan **4 cluster**.

  Cluster       Jumlah Wilayah
  ----------- ----------------
  Cluster 1                137
  Cluster 2                114
  Cluster 3                244
  Cluster 4                 15

Cluster tidak secara langsung diberi label "baik" atau "buruk".
Interpretasi dilakukan berdasarkan profil indikator masing-masing
kelompok.

### 4.3 Geospatial

Batas administrasi kabupaten/kota digunakan untuk menghubungkan hasil
analisis dengan lokasi geografis.

Penggabungan data statistik dan geospasial dilakukan menggunakan **kode
wilayah BPS** sebagai join key.

Dataset geospasial akhir memiliki:

-   514 fitur kabupaten/kota
-   514 kode BPS unik
-   514 kode BPS yang cocok dengan master data

### 4.4 Hierarchical

Struktur hierarki yang digunakan adalah:

``` text
Provinsi
   └── Cluster
         └── Kabupaten/Kota
```

Pada visualisasi hierarki, ukuran digunakan untuk menunjukkan **jumlah
penduduk**, sedangkan warna menunjukkan **persentase penduduk miskin**.
PDRB per kapita ditampilkan sebagai informasi tambahan pada tooltip.

## 5. Visualisasi

Web story mencakup:

### Peta IPM

Choropleth digunakan untuk menunjukkan persebaran IPM antar
kabupaten/kota.

### PCA Scatter Plot

Scatter plot PC1 dan PC2 digunakan untuk melihat posisi kabupaten/kota
dalam ruang multivariat. Warna menunjukkan cluster.

### Profil Cluster

Profil indikator digunakan untuk membandingkan karakteristik
masing-masing cluster.

### Peta Cluster

Hasil clustering ditampilkan secara geografis untuk melihat pola
persebaran kelompok wilayah.

### Treemap / Sunburst

Visualisasi hierarki menunjukkan struktur:

**Provinsi → Cluster → Kabupaten/Kota**

## 6. Interaksi

Web story menyediakan interaksi seperti:

-   tooltip,
-   pemilihan wilayah,
-   penyorotan cluster,
-   zoom dan pan pada peta,
-   eksplorasi scatter plot PCA,
-   eksplorasi struktur hierarki.

Interaksi digunakan untuk menghubungkan hasil analisis multivariat
dengan persebaran geografis.

## 7. Teknologi

Proyek menggunakan:

-   R
-   R Shiny
-   shinydashboard
-   ggplot2
-   plotly
-   leaflet
-   dplyr
-   readxl
-   sf
-   tidyr
-   scales
-   RColorBrewer

## 8. Struktur Repository

Contoh struktur:

``` text
WebStory/
├── app.R
├── data/
│   ├── Master_Data_Final_Geospatial.xlsx
│   └── kab_kota_bps.geojson
├── README.md
└── assets/
    └── ...
```

## 9. Menjalankan Aplikasi

Pastikan R dan RStudio telah terpasang.

Install package:

``` r
install.packages(c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "plotly",
  "leaflet",
  "dplyr",
  "readxl",
  "sf",
  "tidyr",
  "scales",
  "RColorBrewer"
))
```

Kemudian buka `app.R` di RStudio dan jalankan:

``` r
shiny::runApp()
```

## 10. Sumber Data

### Badan Pusat Statistik

Data statistik utama bersumber dari BPS tahun 2024, meliputi:

-   Indeks Pembangunan Manusia
-   Persentase Penduduk Miskin
-   Tingkat Pengangguran Terbuka
-   Rata-rata Lama Sekolah
-   Harapan Lama Sekolah
-   Umur Harapan Hidup
-   Akses air minum layak
-   Akses sanitasi layak
-   PDRB per kapita
-   Jumlah penduduk

**Sumber: Badan Pusat Statistik (BPS).**

### Data Geospasial

Data batas administrasi kabupaten/kota digunakan sebagai data pendukung
untuk visualisasi geospasial.

**Sumber: Badan Informasi Geospasial (BIG).**

## 11. Hasil Utama

Analisis menunjukkan adanya keragaman karakteristik pembangunan yang
kuat antar kabupaten/kota.

Beberapa hasil utama:

-   PC1 menjadi komponen utama dengan kontribusi variasi terbesar,
    sekitar 45,93%.
-   Kabupaten/kota terbagi menjadi empat kelompok berdasarkan
    karakteristik indikator pembangunan.
-   Cluster 3 merupakan kelompok terbesar dengan 244 wilayah.
-   Cluster 4 merupakan kelompok terkecil dengan 15 wilayah dan memiliki
    karakteristik indikator pembangunan yang relatif rendah pada
    beberapa dimensi.
-   Cluster 2 memiliki profil pendidikan, kesehatan, akses dasar, dan
    ekonomi yang relatif tinggi dibandingkan beberapa kelompok lainnya.
-   Persebaran cluster menunjukkan bahwa perbedaan pembangunan memiliki
    pola geografis.

Temuan tersebut mendukung gagasan utama web story:

> **Indonesia tidak memiliki satu wajah pembangunan yang seragam.**

## 12. Reproduksibilitas

Konfigurasi clustering:

``` text
Metode         : K-Means
Jumlah cluster : 4
n_init         : 50
random_state   : 123
```

PCA dilakukan terhadap delapan variabel setelah standardisasi.

## 13. Catatan Data

Nilai `-` pada data sumber diperlakukan sebagai **missing value (NA)**,
bukan sebagai nilai nol.

PCA dan clustering menggunakan observasi yang memiliki data lengkap pada
delapan variabel analisis.

Dataset master mempertahankan cakupan 514 kabupaten/kota.

Meskipun proyek menggunakan label periode **2024**, periode referensi
masing-masing indikator mengikuti definisi dan periode pengukuran BPS
pada sumber aslinya.
