library(targets)
library(tarchetypes)
library(crew)
library(here)

tar_option_set(
  controller = crew_controller_local(workers = 8),
  packages = c(
    "broom",
    "estimatr",
    "forcats",
    "gdim",
    "ggdag", 
    "ggraph", 
    "glue",
    "here", 
    "hms",
    "igraph",
    "lubridate", 
    "latentnetmediate",
    "marginaleffects",
    "Matrix", 
    "methods",
    "tidygraph", 
    "tidyverse",
    "vsp"
  )
)

tar_source()

get_sparse_survey_responses <- function(df) {
  A <- df |> 
    select(
      contains("nih_lonliness"),
      contains("mlq_10"),
      contains("ffmq_8"),
      contains("dds_10")
    ) |> 
    as.matrix() |> 
    as("sparseMatrix")
  rownames(A) <- df$id
  A
}

get_node_data <- function(df) {
  df |> 
    select(
      -contains("nih_lonliness"),
      -contains("mlq_10"),
      -contains("ffmq_8"),
      -contains("dds_10")
    ) |>
    mutate(
      name = as.character(id)
    ) |> 
    select(-id)
}

make_tbl_graph <- function(A, node_data) {
  graph_from_biadjacency_matrix(A, weighted = TRUE) |> 
    as_tbl_graph() |> 
    left_join(
      node_data, by = "name"
    )
}

nest_by_event <- function(data, rank = 2) {
  data |> 
    na.omit() |> 
    nest(
      data = -c(redcap_event_name, event_name, event_name_fct)
    ) |> 
    mutate(
      A = map(data, get_sparse_survey_responses),
      node_data = map(data, get_node_data),
      tbl_graph = map2(A, node_data, make_tbl_graph),
      fa = map(A, vsp, rank = rank),
      ecv = map(A, eigcv, k_max = 10, laplacian = FALSE, regularize = FALSE),
      meddep = map(tbl_graph, netmediate, depression ~ intervention + age + I(age^2) + sex, rank = 2),
      medanx = map(tbl_graph, netmediate, anxiety ~ intervention + age + I(age^2) + sex, rank = 2)
    )
}

list(
  
  tar_target(
    redcap_data_path,
    here("data", "df_camped_alex2.csv"),
    format = "file"
  ),
  
  tar_target(
    data,
    clean_cahmped_data(redcap_data_path)
  ),
  
  tar_target(
    ate_figure,
    make_ate_figure(data),
    format = "file"
  ),
  
  tar_target(
    nested_by_event,
    nest_by_event(data)
  ),
  
  tar_group_by(
    grouped_by_event,
    nested_by_event,
    event_name
  ),
  
  tar_target(
    eigcv_plots,
    make_eigcv_plots(grouped_by_event$ecv[[1]], grouped_by_event$redcap_event_name),
    pattern = map(grouped_by_event)
  ),
  
  tar_target(
    mediation_dag,
    make_mediation_dag(),
    format = "file"
  ),
  
  tar_target(
    week4_scatter_figure,
    make_week4_scatter_figure(data),
    format = "file"
  ),
  
  tar_target(
    week4_responses_figure,
    make_week4_responses_figure(nested_by_event$A[[4]]),
    format = "file"
  ),
  
  tar_target(
    bipartite_mediation_figure,
    make_bipartite_mediation_figure(),
    format = "file"
  ),
  
  tar_target(
    xhat_figure,
    make_xhat_figure(nested_by_event$fa[[4]]),
    format = "file"
  ),

  tar_target(
    vhat_figure,
    make_vhat_figure(nested_by_event$fa[[4]]),
    format = "file"
  ),
  
  tar_target(
    rank_estimate_figures,
    make_rank_estimate_figures(nested_by_event$A[[4]], nested_by_event$ecv[[4]]),
    format = "file"
  ),
  
  tar_target(
    regression_figures,
    make_regression_figures(nested_by_event),
    format = "file"
  ),
  
  tar_target(
    confounding_homophily_interference_figure,
    make_confounding_homophily_interference_figure(),
    format = "file"
  ),
  
  tar_target(
    confounding_homophily_figure,
    make_confounding_homophily_figure(),
    format = "file"
  ),
  
  tar_target(
    homophily_mediating_figure,
    make_homophily_mediating_figure(),
    format = "file"
  )
)
