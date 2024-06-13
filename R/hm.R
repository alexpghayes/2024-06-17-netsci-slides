clean_cahmped_data <- function(raw_path) {
  raw_path |>
    read_csv() |>
    select(
      id, 
      group, # treatment assignment
      contains("timestamp"),
      redcap_event_name,
      contains("nih_lonliness_q0"), # mediator indicators
      contains("mlq_10_q"), # mediator indicators
      contains("ffmq_8"), # 8 item-level mediators,
      contains("dds_10_q"), # mediator
      promis_bank_v10_depression_tscore, # outcome 1
      promis_bank_v10_anxiety_tscore, # outcome 2,
      dem_sex_assigned_birth,
      pre_age
    ) |> 
    mutate(
      intervention = case_match(
        group,
        1 ~ "Control",
        2 ~ "Meditation"
      ),
      depression = promis_bank_v10_depression_tscore,
      anxiety = promis_bank_v10_anxiety_tscore,
      event_name = case_match(
        redcap_event_name,
        "pre_arm_1" ~ "Pre-screen",
        "w1_arm_1" ~ "Week 1",
        "w2_arm_1" ~ "Week 2",
        "w3_arm_1" ~ "Week 3",
        "post_arm_1" ~ "Week 4",
        "3mo_arm_1" ~ "Post-screen"
      ),
      event_name_fct = fct_inorder(event_name),
      age = pre_age,
      sex = dem_sex_assigned_birth
    ) |> 
    group_by(id) |> 
    fill(
      age, sex, intervention,
      .direction = "downup"
    ) |> 
    ungroup() |> 
    filter(!is.na(event_name)) |> 
    select(
      id, contains("event_name"), intervention, age, sex, anxiety, depression, everything(),
      -group, -contains("bank_v10"), -pre_age, -dem_sex_assigned_birth
    )
}

make_ate_figure <- function(data) {
  
  plot <- data |> 
    lm(anxiety ~ intervention * event_name_fct, data = _) |> 
    augment(interval = "confidence") |> 
    ggplot() +
    aes(
      x = event_name_fct,
      ymin = .lower,
      y = anxiety,
      ymax = .upper,
      color = intervention,
      fill = intervention,
      group = intervention
    ) +
    geom_jitter(
      position = position_dodge2(width = 0.3),
      # width = 0.25,
      # alpha = 0.5
    ) +
    geom_ribbon(
      alpha = 0.75
    ) + 
    # annotate(
    #   geom = "text",
    #   x = 2,
    #   y = 30.2,
    #   label = "High anxiety",
    #   color = "#444444"
    # ) +
    # annotate(
    #   geom = "text",
    #   x = 2.1,
    #   y = 83,
    #   label = "Low anxiety",
    #   color = "#444444"
    # ) +
    labs(
      y = "Anxiety level (PROMIS)",
      fill = NULL,
      color = NULL,
      title = NULL
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal(
      base_size = 12,
      base_family = "Fira Sans"
    ) +
    theme(
      axis.title.x = element_blank()
    )
  
  path <- here("figures", "ate.png")
  
  ggsave(
    path,
    plot = plot,
    height = 2.75,
    width = 5.75,
    dpi = 300
  )
  
  path
}


make_week4_scatter_figure <- function(data) {
  
  week4 <- data |> 
    filter(event_name == "Week 4")
  
  plot <- week4 |> 
    ggplot() +
    aes(
      x = intervention,
      y = anxiety,
      color = intervention
    ) +
    geom_jitter(
      position = position_dodge2(width = 0.3),
    ) +
    labs(
      title = "Week 4",
      y = "Anxiety level (PROMIS)"
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal(
      base_size = 12,
      base_family = "Fira Sans"
    ) +
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )
  
  plot
  
  path <- here("figures", "week4_scatter.png")
  
  ggsave(
    path,
    plot = plot,
    height = 2.5,
    width = 2.25,
    dpi = 300
  )
  
  path
}


make_week4_responses_figure <- function(A4) {
  plot <- A4 |> 
    as.matrix() |> 
    as_tibble(rownames = "name") |> 
    pivot_longer(
      -name,
      names_to = "item",
      values_to = "response"
    ) |> 
    ggplot(aes(item, name, fill = as.factor(response))) +
    geom_raster() +
    scale_fill_viridis_d() +
    labs(
      fill = "Response\nat Week 4",
      x = "Survey question",
      y = "Participant"
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  path <- here("figures", "week4-responses.png")
  
  ggsave(
    path,
    plot = plot,
    height = 2.75,
    width = 5.75,
    dpi = 300
  )
  
  path
}


make_xhat_figure <- function(fa) {
  
  plot <- fa |> 
    get_svd_u() |> 
    set_names(nm = c("participant", paste0("Xhat", 1:fa$rank))) |> 
    ggplot(aes(Xhat1 * sqrt(fa$d[1]), Xhat2 * sqrt(fa$d[2]))) +
    geom_point() +
    theme_minimal(
      base_size = 11,
      base_family = "Fira Sans"
    ) +
    labs(
      x = "Xhat1",
      y = "Xhat2"
    ) +
    theme(
      axis.text = element_blank()
    )
  
  path <- here("figures", "xhat.png")
  
  ggsave(
    path,
    plot = plot,
    height = 3.5,
    width = 4 * 16/9,
    dpi = 300
  )
  
  path
}


make_vhat_figure <- function(fa) {
  
  plot <- fa |> 
    get_svd_v() |> 
    set_names(nm = c("item", paste0("Vhat", 1:fa$rank))) |> 
    pivot_longer(
      contains("Vhat")
    ) |> 
    ggplot(aes(name, item, fill = value)) +
    geom_raster() +
    scale_fill_gradient2() +
    labs(
      fill = "Loading"
    ) +
    theme_classic(
      base_size = 9.5,
      base_family = "Fira Sans"
    ) +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
  
  path <- here("figures", "vhat.png")
  
  ggsave(
    path,
    plot = plot,
    height = 2.9,
    width = 5.35,
    dpi = 300
  )
  
  path
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

make_rank_estimate_figures <- function(A, ecv) {

  pca <- prcomp(A)
  
  vars <- pca$sdev^2
  prop <- cumsum(vars / sum(vars))
  
  plot <- ecv$summary |> 
    mutate(
      cum_var_prop = prop[1:10]
    ) |> 
    filter(k <= 6) |> 
    pivot_longer(
      cols = c(z, cum_var_prop)
    ) |> 
    mutate(
      name = if_else(name == "z", "Z-statistic", "Variance\nexplained"),
      name = as.factor(name),
      name = relevel(name, "Z-statistic")
    ) |> 
    ggplot() +
    aes(k, value, color = name) +
    geom_line() +
    geom_point() +
    scale_x_continuous(
      breaks = scales::pretty_breaks()
    ) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      caption = "Cross-validated eigenvalue method selects d = 2",
      x = "Rank"
    ) +
    facet_grid(
      rows = vars(name),
      scales = "free"
    ) +
    theme_minimal(
      base_size = 12,
      base_family = "Fira Sans"
    ) +
    theme(
      axis.title.y = element_blank(),
      legend.position = "none"
    )
    
  
  path <- here("figures", "rank-determination.png")
  
  ggsave(
    path,
    plot = plot,
    height = 2.75,
    width = 5.25,
    dpi = 400
  )
  
  path
}

make_regression_figures <- function(nested_by_event) {
  
  A <- nested_by_event$A[[4]] 
  
  fa <- vsp(A, rank = 5, degree_normalize = FALSE)
  
  Xhat <- fa$Z %*% fa$B |> 
    as.data.frame() |>
    set_names(paste0("Xhat", 1:5)) |>
    mutate(
      name = rownames(A)
    )
  
  # Xhat <- Z(A, rank = 5) |> 
  #   as.data.frame() |> 
  #   set_names(paste0("Xhat", 1:5)) |> 
  #   mutate(
  #     name = rownames(A)
  #   )
  # 
  node_data <- nested_by_event$node_data[[4]]
  
  merged <- left_join(node_data, Xhat, by = "name") |> 
    mutate(
      age2 = age^2
    )
  
  o_fit <- merged |> 
    select(anxiety, intervention, age, sex, Xhat1:Xhat5) |> 
    lm_robust(
      anxiety ~ .,
      data = _
    )
  
  summary(o_fit)
  
  o_fit <- lm(
    anxiety ~ intervention + Xhat1 + Xhat2 + Xhat3 + Xhat4 + Xhat5 + age + age2 + sex,
    data = merged
  )
  
  summary(o_fit)
  
  plot <- tidy(o_fit, conf.int = TRUE) |> 
    mutate(
      term = str_replace(term, "interventionMeditation", "meditation")
    ) |> 
    filter(term %in% c("Xhat1", "Xhat2", "meditation")) |> 
    ggplot() +
    aes(
      x = term,
      ymin = conf.low,
      y = estimate,
      ymax = conf.high,
      color = estimate > 0
    ) +
    geom_pointrange() +
    scale_color_manual(
      values = c(
        `TRUE` = "firebrick",
        `FALSE` = "steelblue"
      ),
      guide = "none"
    ) +
    coord_flip() +
    labs(
      title = "Outcome regression",
      x = "Coefficient",
      caption = "Control coefficients not displayed"
    ) +
    theme_minimal(
      base_size = 11,
      base_family = "Fira Sans"
    ) +
    theme(
      axis.title.x = element_blank()
    )
  
  path <- here("figures", "outcome-coefficients.png")
  
  ggsave(
    path,
    plot = plot,
    height = 2.75,
    width = 5.25,
    dpi = 300
  )
  
  m_fit <- lm_robust(
    cbind(Xhat1, Xhat2, Xhat3, Xhat4, Xhat5) ~ intervention + age + I(age^2) + sex,
    data = merged
  )
  
  # lil hack because {marginaleffects} doesn't like multi-outcome regression
  m_fit1 <- lm_robust(
    Xhat1 ~ intervention + age + I(age^2) + sex,
    data = merged
  )
  
  m_fit2 <- lm_robust(
    Xhat2 ~ intervention + age + I(age^2) + sex,
    data = merged
  )
  
  m1m <- avg_predictions(
    m_fit1,
    by = "intervention"
  ) |> 
    as.data.frame() |> 
    mutate(
      var = "Xhat1"
    )
  
  m2m <- avg_predictions(
    m_fit2,
    by = "intervention"
  ) |> 
    as.data.frame() |> 
    mutate(
      var = "Xhat2"
    )
  
  marginals <- bind_rows(m1m, m2m) |> 
    pivot_wider(
      id_cols = intervention,
      names_from = var, 
      values_from = estimate
    )
  
  plot2 <- merged |> 
    ggplot() +
    aes(
      x = Xhat1,
      y = Xhat2
    ) +
    geom_point(color = "lightgrey") +
    annotate(
      geom = "text",
      x = -0.7,
      y = 1.25,
      label = "Higher anxiety",
      color = "#444444",
      size = 3
    ) +
    annotate(
      geom = "text",
      x = -1.1,
      y = -0.9,
      label = "Lower anxiety",
      color = "#444444",
      size = 3
    ) +
    geom_line(
      data = marginals,
      arrow = arrow(length = unit(0.15, "centimeters"), ends = "first", type = "closed"),
      linejoin = "mitre",
      size = 0.5
    ) +
    theme_minimal(
      base_size = 9.5,
      base_family = "Fira Sans"
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  path2 <- here("figures", "latent-intervention.png")
  
  ggsave(
    path2,
    plot = plot2,
    height = 2.75,
    width = 5.25,
    dpi = 300
  )
  
  c(path, path2)
}


make_causal_effect_figures <- function(nested_by_event) {
  
  nested_by_event$medanx[[4]] |> 
    pluck("effects") |> 
    filter(str_detect(term, "intervention"))
  
}




