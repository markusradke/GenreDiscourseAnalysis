rm(list = ls())
gc()
library(dplyr)
library(data.table)
library(ggcorrplot)
devtools::load_all()

# SETUP ----
MIN_ARTISTS <- 10

# LOAD DATA ----
poptrag_selected <- readRDS("data/poptrag_selected.rds")

mb <- readRDS("data/mb_long_artist_treshholded.rds")
ab <- fread("data/filtered_ab_long.csv")
dc <- fread("data/filtered_dc_long.csv")
dz <- fread("data/filtered_dz_long.csv")

fold_1_artists <- readRDS("data/fold1.rds") |>
  distinct(track.s.firstartist.id) |>
  pull(track.s.firstartist.id)
fold_2_artists <- readRDS("data/fold2.rds") |>
  distinct(track.s.firstartist.id) |>
  pull(track.s.firstartist.id)

Pfull <- readr::read_csv("data/Pfull.csv") |> as.matrix()
cat_states_full <- readRDS("models/dag/cat_states_full.rds")

# FUNCTIONS ----

filter_min_artists_long <- function(
  long_df,
  min_artists,
  fold_1_artists,
  fold_2_artists
) {
  tags_fold_1 <- long_df |>
    filter(track.s.firstartist.id %in% fold_1_artists) |>
    group_by(tag_name) |>
    summarize(n_artists = n_distinct(track.s.firstartist.id)) |>
    arrange(n_artists) |>
    filter(n_artists >= MIN_ARTISTS)
  tags_fold_2 <- long_df |>
    filter(track.s.firstartist.id %in% fold_2_artists) |>
    group_by(tag_name) |>
    summarize(n_artists = n_distinct(track.s.firstartist.id)) |>
    arrange(n_artists) |>
    filter(n_artists >= MIN_ARTISTS)
  common_tags <- intersect(tags_fold_1$tag_name, tags_fold_2$tag_name)
  long_df |> filter(tag_name %in% common_tags)
}


# DETERMINE # OF GENRES ----
ab_filtered <- ab
dc_filtered <- filter_min_artists_long(
  dc,
  MIN_ARTISTS,
  fold_1_artists,
  fold_2_artists
)
dz_filtered <- filter_min_artists_long(
  dz,
  MIN_ARTISTS,
  fold_1_artists,
  fold_2_artists
)

# # of genres
ab_filtered$tag_name |> unique() |> length() # 7
dc_filtered$tag_name |> unique() |> length() # 12
dz_filtered$tag_name |> unique() |> length() # 34


# INSPECT MB SOLUTION FOR MAX # OF GENRES ----
chosen_k <- 34
G <- ncol(Pfull)
final_genres <- tail(cat_states_full$processing_order, chosen_k)
k_weights <- cat_states_full$weights[[G - chosen_k + 1]][
  final_genres,
  final_genres
]
k_sizes <- cat_states_full$sizes[[G - chosen_k + 1]][final_genres]
# make graph and add sizes as node attributes
k_graph <- igraph::graph_from_adjacency_matrix(
  t(k_weights),
  mode = "directed",
  weighted = TRUE
)
igraph::V(k_graph)$size <- k_sizes

export_graph_for_gephi_import(
  k_graph,
  sprintf("mb_k%d", chosen_k)
)


# ANALYSE ASSOCIATION WITH EXTERNAL TAXONOMIES ----
# SPEARMAN CORRELATION BETWEEN MB AND AB PROBABILITIES
chosen_k <- 7
mb_track_map <- get_track_category_probabilities(
  Pfull,
  cat_states_full,
  chosen_k
)
mb_cat <- add_single_genre_categories_to_tracks(mb, mb_track_map) |>
  select(
    colnames(mb_track_map),
    id
  ) |>
  distinct(id, .keep_all = TRUE)
mb_wide <- select(mb_cat, -cat, -cat_prob) |>
  rename_with(~ paste0("mb.", .), -id) |>
  as.data.frame()

ab_wide <- dcast(
  ab_filtered,
  id ~ tag_name,
  value.var = "tag_count",
  fill = 0
) |>
  rename_with(~ paste0("ab.", .), -id) |>
  as.data.frame()

gini_mb <- DescTools::Gini(table(
  inner_join(mb_cat, select(ab_wide, id), by = "id")$cat
))
ab_mat <- ab_wide |>
  select(-id) |>
  as.matrix()
# get colname of highest probability for each row
ab_cat <- apply(ab_mat, 1, function(x) {
  if (all(x == 0)) {
    return(NA)
  }
  colnames(ab_mat)[which.max(x)]
})
gini_ab <- DescTools::Gini(table(ab_cat))

x <- inner_join(ab_wide, mb_wide, by = "id")
gini_label <- sprintf(
  "N=%s<br>Gini<sub>G7</sub> = %.3f<br>Gini<sub>%s</sub> = %.3f",
  format(nrow(x), big.mark = ","),
  gini_mb,
  "ROSA",
  gini_ab
)


c1 <- setdiff(names(ab_wide), "id")
c2 <- setdiff(names(mb_wide), "id")

spearman_mat <- cor(
  x[c1],
  x[c2],
  method = "spearman",
  use = "pairwise.complete.obs"
)
spearman_tab <- as.data.frame(as.table(spearman_mat)) |>
  rename(ab_col = Var1, mb_col = Var2, spearman = Freq) |>
  mutate(
    ab_genre = stringr::str_remove(ab_col, "ab."),
    mb_genre = stringr::str_remove(mb_col, "mb.")
  )
colnames(spearman_mat) <- stringr::str_remove(colnames(spearman_mat), "mb.")
rownames(spearman_mat) <- stringr::str_remove(row.names(spearman_mat), "ab.")
# get the same ordering corrplot uses
ord <- corrplot:::corrMatOrder(
  spearman_mat,
  order = "hclust",
  hclust.method = "ward.D2"
)

mat_ord <- spearman_mat[ord, ord]

# long format for ggplot
df <- reshape2::melt(mat_ord, varnames = c("Var1", "Var2"), value.name = "corr")

# keep factor levels in the plotted order
df$Var1 <- factor(df$Var1, levels = rownames(mat_ord))
df$Var2 <- factor(df$Var2, levels = colnames(mat_ord))

ggplot(df, aes(Var2, Var1, fill = corr)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(corr, 2)), size = 5, color = "black") +
  scale_fill_gradient2(
    low = "#c40d20",
    mid = "white",
    high = "#28720aff",
    midpoint = 0,
    limits = c(-1, 1),
  ) +
  scale_x_discrete(position = "top") +
  coord_equal() +
  labs(
    title = gini_label,
    x = "MUSICMAP-G7",
    y = "Rosamerica (AcousticBrainz)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = ggtext::element_markdown(
      size = 16,
      face = "bold",
      color = "black",
      hjust = 0,
      vjust = -0.5,
      lineheight = 1.5,
      margin = ggplot2::margin(b = -72)
    ),
    plot.title.position = "plot",
    legend.position = "none",
    axis.text.x = element_text(angle = 70, hjust = 0, vjust = 0, size = 14),
    axis.text.y = element_text(hjust = 1, vjust = 0, size = 14),
    panel.grid = element_blank(),
    axis.title = element_text(size = 16, face = "bold", color = "grey45"),
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 1)
  )
ggsave(
  "models/dag/spearman_mb_ab.png",
  width = 10,
  height = 7,
  dpi = 600
)

# MEAN PROB DC ----
chosen_k <- 12
mb_track_map <- get_track_category_probabilities(
  Pfull,
  cat_states_full,
  chosen_k
)
mb_cat <- add_single_genre_categories_to_tracks(mb, mb_track_map) |>
  select(
    colnames(mb_track_map),
    id
  ) |>
  distinct(id, .keep_all = TRUE)
mb_wide <- select(mb_cat, -cat, -cat_prob) |>
  as.data.frame()

dc_genres_ordered <- table(dc_filtered$tag_name) |> rank()
order <- data.frame(
  tag_name = names(dc_genres_ordered),
  rank = dc_genres_ordered
)
dc_cat <- left_join(as.data.frame(dc_filtered), order, by = "tag_name") |>
  group_by(id) |>
  slice_min(rank, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(id, tag_name) |>
  rename(dc_cat = tag_name)


mb_genres <- colnames(mb_wide)[colnames(mb_wide) != "id"]
mean_probabilities <- inner_join(dc_cat, mb_wide, by = "id") |>
  group_by(dc_cat) |>
  summarise(across(all_of(mb_genres), function(x) mean(x, na.rm = TRUE))) |>
  tidyr::pivot_longer(
    cols = all_of(mb_genres),
    names_to = "mb_cat",
    values_to = "mean_prob"
  ) |>
  mutate(mb_cat = stringr::str_remove(mb_cat, "cat_prob_"))
gini_mb <- DescTools::Gini(table(
  inner_join(mb_cat, distinct(dc_filtered, id), by = "id")$cat
))
gini_dc <- DescTools::Gini(table(inner_join(dc_cat, mb_wide, by = "id")$dc_cat))
gini_label <- sprintf(
  "N=%s<br>Gini<sub>G12</sub> = %.3f<br>Gini<sub>%s</sub> = %.3f",
  format(nrow(inner_join(dc_cat, mb_wide, by = "id")), big.mark = ","),
  gini_mb,
  "Discogs",
  gini_dc
)

# make a heatmap of mean probabilities
ggplot(mean_probabilities, aes(x = mb_cat, y = dc_cat, fill = mean_prob)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_prob * 100)), size = 5, color = "black") +
  scale_fill_gradient(low = "white", high = "#28720aff") +
  scale_x_discrete(position = "top") +
  labs(
    title = gini_label,
    x = "MUSICMAP-G12",
    y = "Discogs",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = ggtext::element_markdown(
      size = 16,
      face = "bold",
      color = "black",
      hjust = 0,
      vjust = -0.5,
      lineheight = 1.5,
      margin = ggplot2::margin(b = -72)
    ),
    plot.title.position = "plot",
    legend.position = "none",
    axis.text.x = element_text(angle = 70, hjust = 0, vjust = 0, size = 14),
    axis.text.y = element_text(hjust = 1, vjust = 0, size = 14),
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 1),
    panel.grid = element_blank(),
    axis.title = element_text(size = 16, face = "bold", color = "grey45")
  )
ggsave(
  "models/dag/mean_probabilities_mb_dc.png",
  width = 10,
  height = 7,
  dpi = 600
)


# MEAN PROB DZ ----
chosen_k <- 34
mb_track_map <- get_track_category_probabilities(
  Pfull,
  cat_states_full,
  chosen_k
)
mb_cat <- add_single_genre_categories_to_tracks(mb, mb_track_map) |>
  select(
    colnames(mb_track_map),
    id
  ) |>
  distinct(id, .keep_all = TRUE)
mb_wide <- select(mb_cat, -cat, -cat_prob) |>
  as.data.frame()

dz_genres_ordered <- table(dz_filtered$tag_name) |> rank()
order <- data.frame(
  tag_name = names(dz_genres_ordered),
  rank = dz_genres_ordered
)
dz_cat <- left_join(as.data.frame(dz_filtered), order, by = "tag_name") |>
  group_by(id) |>
  slice_min(rank, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(id, tag_name) |>
  rename(dz_cat = tag_name)

mb_genres <- colnames(mb_wide)[colnames(mb_wide) != "id"]
mean_probabilities <- inner_join(dz_cat, mb_wide, by = "id") |>
  group_by(dz_cat) |>
  summarise(across(all_of(mb_genres), function(x) mean(x, na.rm = TRUE))) |>
  tidyr::pivot_longer(
    cols = all_of(mb_genres),
    names_to = "mb_cat",
    values_to = "mean_prob"
  ) |>
  mutate(mb_cat = stringr::str_remove(mb_cat, "cat_prob_"))

gini_mb <- DescTools::Gini(table(
  inner_join(mb_cat, distinct(dz_filtered, id), by = "id")$cat
))
gini_dz <- DescTools::Gini(table(inner_join(dz_cat, mb_wide, by = "id")$dz_cat))
gini_label <- sprintf(
  "N=%s<br>Gini<sub>G34</sub> = %.3f<br>Gini<sub>%s</sub> = %.3f",
  format(nrow(inner_join(dz_cat, mb_wide, by = "id")), big.mark = ","),
  gini_mb,
  "Deezer",
  gini_dz
)

# make a heatmap of mean probabilities
ggplot(mean_probabilities, aes(x = mb_cat, y = dz_cat, fill = mean_prob)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_prob * 100)), size = 5, color = "black") +
  scale_fill_gradient(low = "white", high = "#28720aff") +
  scale_x_discrete(position = "top") +
  labs(
    title = gini_label,
    x = "MUSICMAP-G34",
    y = "Deezer",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = ggtext::element_markdown(
      size = 16,
      face = "bold",
      color = "black",
      hjust = 0,
      vjust = -0.5,
      lineheight = 1.5,
      margin = ggplot2::margin(b = -72)
    ),
    plot.title.position = "plot",
    legend.position = "none",
    axis.text.x = element_text(angle = 70, hjust = 0, vjust = 0, size = 14),
    axis.text.y = element_text(hjust = 1, vjust = 0, size = 14),
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 1),
    panel.grid = element_blank(),
    axis.title = element_text(size = 16, face = "bold", color = "grey45")
  )
ggsave(
  "models/dag/mean_probabilities_mb_dz.png",
  width = 12,
  height = 10,
  dpi = 600
)
