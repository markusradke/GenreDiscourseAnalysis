get_robustness_check <- function(states_1, states_2, Pval, k) {
  G <- ncol(Pval)
  final_genres <- tail(states_1$processing_order, k)
  mapping1 <- states_1$mapping[[G - k + 1]]
  mapping2 <- states_2$mapping[[G - k + 1]]

  # flat mapping differences
  rownames(mapping1) <- colnames(mapping1)
  mapping1 <- mapping1[final_genres, ]
  rownames(mapping2) <- colnames(mapping2)
  mapping2 <- mapping2[final_genres, ]
  flatdiff <- c(mapping1 - mapping2)
  flatdiff.hist <- hist(
    flatdiff,
    breaks = 100,
    main = "Histogram flat differences genre mappings"
  )
  flatdiff.t.test <- t.test(flatdiff)

  # differnces between genre mapping per genre
  genre.jsd <- col_jsd(mapping1, mapping2)
  names(genre.jsd) <- colnames(mapping1)
  genre.jsd <- genre.jsd[!names(genre.jsd) %in% final_genres]
  genre.jsd |> sort(decreasing = TRUE)
  genre.jsd.hist <- hist(
    genre.jsd,
    breaks = 100,
    main = "Histogram of JS-Divergences for subgenre mappings"
  )
  genre.jsd.mean <- mean(genre.jsd)

  # dominant genre F1
  trackmap_1 <- get_track_category_probabilities(
    Pval,
    states_1,
    k
  )
  trackmap_2 <- get_track_category_probabilities(
    Pval,
    states_2,
    k
  )
  fac1 <- factor(trackmap_1$cat)
  fac2 <- factor(trackmap_2$cat, levels = levels(fac1))
  trackmap.macrof1 <- yardstick::f_meas_vec(fac1, fac2, estimator = "macro")
  return(
    list(
      flatdiff = flatdiff,
      flatdiff.hist = flatdiff.hist,
      flatdiff.t.test = flatdiff.t.test,
      genre.jsd = genre.jsd,
      genre.jsd.hist = genre.jsd.hist,
      genre.jsd.mean = genre.jsd.mean,
      trackmap.macrof1 = trackmap.macrof1
    )
  )
}


col_jsd <- function(W1, W2) {
  stopifnot(all(dim(W1) == dim(W2)))
  apply(cbind(1:ncol(W1)), 1, function(i) jsd(W1[, i], W2[, i]))
}

jsd <- function(p, q, base = 2) {
  p <- p / sum(p)
  q <- q / sum(q)
  m <- 0.5 * (p + q)

  kl <- function(a, b) {
    idx <- a > 0
    sum(a[idx] * log(a[idx] / b[idx], base = base))
  }

  0.5 * kl(p, m) + 0.5 * kl(q, m)
}
