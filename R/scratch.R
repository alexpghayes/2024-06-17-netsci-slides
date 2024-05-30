library(gdim)
library(here)
library(igraph)
library(Matrix)
library(tidygraph)
library(tidyverse)
library(vsp)

theme_set(theme_classic(18))

raw <- read_csv(here("data", "df_cahmped_alex.csv"))

# can iterate on this later
reverse_code_likert17 <- function(x) {
  case_match(
    x,
    7 ~ 1,
    6 ~ 2,
    5 ~ 3,
    4 ~ 4,
    3 ~ 5,
    2 ~ 6,
    1 ~ 7,
  )
}

# can iterate on this later
reverse_code_likert05 <- function(x) {
  case_match(
    x,
    5 ~ 0,
    4 ~ 1,
    3 ~ 2,
    2 ~ 3,
    1 ~ 4,
    0 ~ 5
  )
}

data <- raw |>
  select(
    id,
    meta_oa_info_group, # treatment assignment
    contains("timestamp"),
    redcap_event_name,
    contains("nih_lonliness_q0"), # mediator indicators
    contains("mlq_10_q"), # mediator indicators
    contains("ffmq_8"), # 8 item-level mediators,
    contains("dds_10_q"), # mediator
    # contains("nih_perstr_q"), # mediator,
    promis_bank_v10_depression_tscore, # outcome 1
    promis_bank_v10_anxiety_tscore # outcome 2
  ) |>
  filter(!is.na(ffmq_awareness8_timestamp)) |>
  group_by(id) |>
  mutate(
    time = mdy_hm(ffmq_awareness8_timestamp),
    since_baseline = hms::as_hms(time - min(time)),
    condition = meta_oa_info_group,
    promis_eddep = promis_bank_v10_depression_tscore,
    promis_edanx = promis_bank_v10_anxiety_tscore
  ) |>
  ungroup() |>
  select(
    id, redcap_event_name, since_baseline, time,
    everything(),
    -ffmq_awareness8_timestamp, -meta_oa_info_group, -contains("bank_v10")
  ) |>
  mutate(
    across(
      c(mlq_10_q01, mlq_10_q04:mlq_10_q06),
      reverse_code_likert17
    ),
    across(
      contains("dds"),
      reverse_code_likert05
    )
  )

nested_by_event <- data |>
  select(-id, -since_baseline, -time, -condition, -contains("promis")) |>
  na.omit() |>
  nest(data = -redcap_event_name) |>
  mutate(
    dense = map(data, as.matrix),
    A = map(dense, as, "sparseMatrix")
  )

data

tbl_to_sparse <- function(tbl) {
  no_id <- select(tbl, -id)
  dense <- as.matrix(no_id)
  sparse <- as(dense, "sparseMatrix")
  rownames(sparse) <- tbl$id
  sparse
}

nested_by_event <- data |>
  select(-since_baseline, -time, -condition, -contains("promis")) |>
  na.omit() |>
  nest(data = -redcap_event_name) |>
  mutate(
    A = map(data, tbl_to_sparse),
    cveigs = map(A, eigcv, k_max = 8, laplacian = FALSE, regularize = FALSE),
    est_rank = map_int(cveigs, pluck, "estimated_dimension"),
    fa = map(A, vsp, rank = 3, degree_normalize = FALSE)
  )

fa_baseline <- nested_by_event$fa[[4]]

baseline_factors <- function(A, fa_baseline) {
  Y <- as.matrix(fa$Y)
  A <- as.matrix(A)
  fit <- lm(t(A) ~ Y + 0)
  baseline <- t(coef(fit))
  colnames(baseline) <- paste0("ZB", 1:ncol(baseline))
  baseline_df <- as.data.frame(baseline)
  as_tibble(baseline_df, rownames = "name")
}

make_node_data <- function(tbl) {
  tbl |>
    select(id, since_baseline, time, condition, contains("promis")) |>
    mutate(
      name = as.character(id)
    ) |>
    select(-id)
}

node_data <- data |>
  na.omit() |>
  nest(data = -redcap_event_name) |>
  mutate(
    node_data = map(data, make_node_data)
  ) |>
  select(-data)

make_tbl_graph <- function(node_data, A, baseline_factors) {
  graph_from_incidence_matrix(A, weighted = TRUE) |>
    as_tbl_graph() |>
    left_join(
      node_data, by = "name"
    ) |>
    left_join(
      baseline_factors,
      by = "name"
    )
}

nested_graph <- nested_by_event |>
  left_join(node_data, by = "redcap_event_name") |>
  mutate(
    bf = map(A, baseline_factors, fa_baseline),
    tbl_graph = pmap(list(node_data, A, bf), make_tbl_graph)
  )

controls <- nested_graph$bf[[1]] |>
  set_names(
    c("name", "C1", "C2", "C3")
  )

# TODO: check for positivity violations

nd <- nested_graph$node_data[[4]] |>
  left_join(
    nested_graph$bf[[4]],
    by = "name"
  ) |>
  left_join(
    controls,
    by = "name"
  )

o_fit <- lm(
  promis_eddep ~ condition + ZB1 + ZB2 + ZB3,
  data = nd
)

o_fit2 <- lm(
  promis_eddep ~ condition + ZB1 + ZB2 + ZB3 + C1 + C2 + C3,
  data = nd
)

m_fit <- lm(
  cbind(ZB1, ZB2, ZB3) ~ condition,
  data = nd
)

m_fit2 <- lm(
  cbind(ZB1, ZB2, ZB3) ~ condition * (C1 + C2 + C3),
  data = nd
)

library(GGally)

nd |>
  select(contains("ZB"), condition) |>
  ggpairs(
    aes(
      color = condition
    )
  )



nd |>
  select(contains("ZB"), condition, matches("C[1-3]")) |>
  ggpairs(
    aes(
      color = condition
    )
  )
