rm(list = ls())
gc()
devtools::load_all()


# MAIN ----
cat_states_1_order1 <- readRDS("models/dag/cat_states_1_order1.rds")
cat_states_2_order1 <- readRDS("models/dag/cat_states_2_order1.rds")
cat_states_1_order2 <- readRDS("models/dag/cat_states_1_order2.rds")
cat_states_2_order2 <- readRDS("models/dag/cat_states_2_order2.rds")
P_holdout <- readr::read_csv("data/P_holdout.csv") |> as.matrix()

# conduct checks ----
k <- 7
check_order1 <- get_robustness_check(
  cat_states_1_order1,
  cat_states_2_order1,
  P_holdout,
  k
)

check_order2 <- get_robustness_check(
  cat_states_1_order2,
  cat_states_2_order2,
  P_holdout,
  k
)

# inspect ----
plot(check_order1$flatdiff.hist)
plot(check_order2$flatdiff.hist)
check_order1$flatdiff.t.test
check_order2$flatdiff.t.test

plot(check_order1$genre.jsd.hist)
plot(check_order2$genre.jsd.hist)
check_order1$genre.jsd.mean
check_order2$genre.jsd.mean

check_order1$trackmap.macrof1
check_order2$trackmap.macrof1
