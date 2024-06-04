
clean_cahmped_data <- function(raw_path) {
  raw_path |>
    read_csv() |> 
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
    mutate(
      time = mdy_hm(ffmq_awareness8_timestamp),
      condition = meta_oa_info_group,
      intervention = case_match(
        condition,
        "control" ~ "Control",
        "intervention" ~ "Meditation"
      ),
      promis_eddep = promis_bank_v10_depression_tscore,
      promis_edanx = promis_bank_v10_anxiety_tscore,
      event_name = case_match(
        redcap_event_name,
        "pre_arm_1" ~ "Pre-screen",
        "w1_arm_1" ~ "Week 1",
        "w2_arm_1" ~ "Week 2",
        "w3_arm_1" ~ "Week 3",
        "post_arm_1" ~ "Week 4",
        "3mo_arm_1" ~ "Post-screen"
      ),
      event_name_fct = fct_inorder(event_name)
    ) |> 
    mutate(
      since_baseline = as_hms(time - min(time)),
      .by = id
    ) |>
    select(
      id, redcap_event_name, event_name, event_name_fct, since_baseline, time,
      everything(),
      -ffmq_awareness8_timestamp, -meta_oa_info_group, -contains("bank_v10")
    )
}

make_ate_figure <- function(data) {
  
  plot <- data |> 
    lm(promis_edanx ~ intervention * event_name_fct, data = _) |> 
    augment(interval = "confidence") |> 
    ggplot() +
    aes(
      x = event_name_fct,
      ymin = .lower,
      y = promis_edanx,
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
      y = promis_edanx,
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
    ggplot(aes(Xhat1, Xhat2)) +
    geom_point() +
    labs(
      title = "Participant embeddings"
    ) +
    theme_minimal(
      base_size = 14,
      base_family = "Fira Sans"
    )
  
  path <- here("figures", "xhat.png")
  
  ggsave(
    path,
    plot = plot,
    height = 2.75,
    width = 5.75,
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
    theme_classic(
      base_size = 12,
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
    width = 5.75,
    dpi = 300
  )
  
  path
}
