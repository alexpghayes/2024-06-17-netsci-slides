library(targets)
library(tarchetypes)
library(crew)
library(here)

tar_option_set(
  controller = crew_controller_local(workers = 8),
  packages = c(
    "broom",
    "forcats",
    "gdim",
    "ggdag", 
    "ggraph", 
    "glue",
    "here", 
    "hms",
    "lubridate", 
    "latentnetmediate",
    "Matrix", 
    "methods",
    "tidygraph", 
    "tidyverse",
    "vsp"
  )
)

tar_source()

nest_by_event <- function(data, rank = 2) {
  data |> 
    select(-id, -since_baseline, -time, -condition, -intervention, -contains("promis")) |> 
    na.omit() |> 
    nest(data = -c(redcap_event_name, event_name, event_name_fct)) |> 
    mutate(
      dense = map(data, as.matrix),
      A = map(dense, as, "sparseMatrix"),
      fa = map(A, vsp, rank = rank),
      ecv = map(A, eigcv, k_max = 10, laplacian = FALSE, regularize = FALSE),
      Uhat = map(A, US, rank = rank),
      Vhat = map(A, VS, rank = rank)
    )
}

make_eigcv_plots <- function(ecv, event) {
  
  plot <- plot(ecv) +
    labs(
      title = glue("Z-scores for cross-validated eigs for {event}")
    )
  
  path <- here("figures", glue("ecv-{event}.png"))
  
  ggsave(
    path,
    plot = plot,
    height = 3.5,
    width = 3.5,
    dpi = 500
  )
  
  path
}

list(
  
  tar_target(
    redcap_data_path,
    here("data", "df_cahmped_alex.csv"),
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
  )
)
