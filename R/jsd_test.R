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

row_jsd <- function(W1, W2) {
  stopifnot(all(dim(W1) == dim(W2)))
  apply(cbind(1:nrow(W1)), 1, function(i) jsd(W1[i, ], W2[i, ]))
}
