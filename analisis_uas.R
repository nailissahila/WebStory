# ============================================================
# ANALISIS UAS VISUALISASI DATA DAN INFORMASI
# Indonesia Tidak Seragam
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(factoextra)
library(cluster)
library(writexl)

# ============================================================
# 1. MEMBACA DATA
# ============================================================

data <- read_excel(
  "data/Master Data(1).xlsx",
  sheet = "Sheet1"
)

# Pemeriksaan awal
dim(data)
names(data)
glimpse(data)

# Validasi identitas wilayah
cat("Jumlah baris:", nrow(data), "\n")
cat("Jumlah kab/kota unik:", n_distinct(data$kab_kota), "\n")
cat("Jumlah id wilayah unik:", n_distinct(data$id_wilayah), "\n")
cat("Duplikat id wilayah:", anyDuplicated(data$id_wilayah), "\n")

# ============================================================
# 2. PEMBERSIHAN DATA
# ============================================================

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

data <- data %>%
  mutate(
    across(
      all_of(pca_vars),
      ~ na_if(as.character(.x), "-")
    )
  ) %>%
  mutate(
    across(
      all_of(pca_vars),
      as.numeric
    )
  )

# ============================================================
# 3. EDA
# ============================================================

data_pca <- data %>%
  select(all_of(pca_vars))

# Statistik deskriptif
statistik_deskriptif <- data_pca %>%
  summarise(
    across(
      everything(),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE)
      )
    )
  )

print(statistik_deskriptif)

# Missing value
missing_summary <- data_pca %>%
  summarise(
    across(
      everything(),
      ~sum(is.na(.x))
    )
  )

print(missing_summary)

# ============================================================
# 4. COMPLETE CASE
# ============================================================

data_pca_complete <- data %>%
  select(
    id_wilayah,
    provinsi,
    kab_kota,
    all_of(pca_vars)
  ) %>%
  drop_na()

cat(
  "Jumlah observasi lengkap untuk PCA:",
  nrow(data_pca_complete),
  "\n"
)

cat(
  "Jumlah observasi yang tidak digunakan:",
  nrow(data) - nrow(data_pca_complete),
  "\n"
)

# ============================================================
# 5. DATA ANALISIS
# ============================================================

X <- data_pca_complete %>%
  select(all_of(pca_vars))

# Standardisasi
X_scaled <- scale(X)

# Cek standardisasi
round(colMeans(X_scaled), 3)
round(apply(X_scaled, 2, sd), 3)

# ============================================================
# 6. PCA
# ============================================================

pca <- prcomp(
  X,
  center = TRUE,
  scale. = TRUE
)

summary(pca)

# Variance
pca_variance <- data.frame(
  Komponen = paste0("PC", seq_along(pca$sdev)),
  Eigenvalue = pca$sdev^2,
  Persen = pca$sdev^2 /
    sum(pca$sdev^2) * 100
) %>%
  mutate(
    Kumulatif_Persen = cumsum(Persen)
  )

print(pca_variance)

# ============================================================
# 7. PCA LOADINGS
# ============================================================

loadings <- as.data.frame(
  pca$rotation
)

print(
  round(loadings, 3)
)

# ============================================================
# 8. PCA SCORE
# ============================================================

pca_scores <- as.data.frame(
  pca$x
)

hasil_pca <- data_pca_complete %>%
  select(
    id_wilayah,
    provinsi,
    kab_kota
  ) %>%
  bind_cols(pca_scores)

head(hasil_pca)

# ============================================================
# 9. MENENTUKAN JUMLAH CLUSTER
# ============================================================

set.seed(123)

hasil_k <- data.frame(
  k = 2:8,
  silhouette = NA_real_
)

for(i in 2:8){
  
  km_temp <- kmeans(
    X_scaled,
    centers = i,
    nstart = 50
  )
  
  sil <- silhouette(
    km_temp$cluster,
    dist(X_scaled)
  )
  
  hasil_k$silhouette[
    hasil_k$k == i
  ] <- mean(sil[, 3])
}

print(hasil_k)

# k terbaik berdasarkan silhouette
k_final <- hasil_k$k[
  which.max(hasil_k$silhouette)
]

cat(
  "Jumlah cluster terbaik berdasarkan silhouette:",
  k_final,
  "\n"
)

# ============================================================
# 10. K-MEANS FINAL
# ============================================================

set.seed(123)

km_final <- kmeans(
  X_scaled,
  centers = k_final,
  nstart = 50
)

print(km_final)

# Ukuran cluster
cluster_size <- table(
  km_final$cluster
)

print(cluster_size)

# Silhouette final
sil_final <- silhouette(
  km_final$cluster,
  dist(X_scaled)
)

mean_silhouette <- mean(
  sil_final[, 3]
)

cat(
  "Average silhouette:",
  round(mean_silhouette, 3),
  "\n"
)

# ============================================================
# 11. HASIL CLUSTER
# ============================================================

cluster_result <- data_pca_complete %>%
  select(id_wilayah) %>%
  mutate(
    cluster = factor(
      km_final$cluster
    )
  )

# Gabungkan ke hasil PCA
hasil_pca <- hasil_pca %>%
  left_join(
    cluster_result,
    by = "id_wilayah"
  )

# ============================================================
# 12. DATA CLUSTER
# ============================================================

data_cluster <- data_pca_complete %>%
  left_join(
    cluster_result,
    by = "id_wilayah"
  )

# ============================================================
# 13. PROFIL CLUSTER
# ============================================================

profil_cluster <- data_cluster %>%
  group_by(cluster) %>%
  summarise(
    jumlah_wilayah = n(),
    
    across(
      all_of(pca_vars),
      ~mean(.x, na.rm = TRUE)
    ),
    
    .groups = "drop"
  )

print(profil_cluster)

# ============================================================
# 14. PROFIL RELATIF CLUSTER
# ============================================================

profil_scaled <- profil_cluster %>%
  select(
    -jumlah_wilayah
  ) %>%
  mutate(
    across(
      all_of(pca_vars),
      ~as.numeric(scale(.x))
    )
  )

print(profil_scaled)

# ============================================================
# 15. PCA SCORE PLOT
# ============================================================

ggplot(
  hasil_pca,
  aes(
    x = PC1,
    y = PC2,
    color = cluster
  )
) +
  
  geom_point(
    alpha = 0.7,
    size = 2.5
  ) +
  
  labs(
    title =
      "Pengelompokan Kabupaten/Kota",
    subtitle =
      "K-Means berdasarkan 8 indikator pembangunan",
    x = "PC1",
    y = "PC2",
    color = "Cluster"
  ) +
  
  theme_minimal()

# ============================================================
# 16. PCA BIPLOT
# ============================================================

fviz_pca_biplot(
  pca,
  repel = TRUE,
  col.var = "black",
  col.ind = "grey50"
)

# ============================================================
# 17. PARALLEL COORDINATES
# ============================================================

parallel_data <- profil_cluster %>%
  select(
    cluster,
    all_of(pca_vars)
  )

ggparcoord(
  parallel_data,
  columns = 2:ncol(parallel_data),
  groupColumn = 1,
  scale = "globalminmax"
) +
  
  labs(
    title =
      "Profil Cluster Berdasarkan 8 Indikator",
    x = "Indikator",
    y = "Skala Relatif"
  ) +
  
  theme_minimal()

# ============================================================
# 18. CORRELATION
# ============================================================

cor_matrix <- cor(
  X,
  use = "complete.obs",
  method = "pearson"
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)

# ============================================================
# 19. HASIL AKHIR
# ============================================================

data_final <- data %>%
  left_join(
    hasil_pca %>%
      select(
        id_wilayah,
        PC1,
        PC2,
        PC3,
        PC4,
        cluster
      ),
    by = "id_wilayah"
  )

# ============================================================
# 20. EXPORT
# ============================================================

write_xlsx(
  list(
    
    Master_Final =
      data_final,
    
    PCA_Variance =
      pca_variance,
    
    PCA_Loadings =
      loadings,
    
    Cluster_Profile =
      profil_cluster,
    
    Silhouette =
      hasil_k
    
  ),
  
  "Hasil_EDA_PCA_Clustering.xlsx"
)

cat(
  "\nAnalisis selesai.\n"
)