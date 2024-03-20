# Preliminaries -----------------------------------------------------------

library(groundhog)
groundhog.library(
  c(
    "tidyverse",
    "cowplot",
    "broom",
    "janitor"
  ),
  "2021-07-01"
)

if(!interactive()) pdf(NULL)

green <- "#007935"
purple <- "#9370DB"
pink <- "#e64173"
grey <- "#808080"

voting_data <- readRDS("Data/cces.rds") %>% 
  filter(weight_cumulative > 0)


# Summary statistics ------------------------------------------------------

n_obs <- voting_data %>% 
  group_by(race) %>% 
  summarize(n = n())

turnout_pre_post <- voting_data %>%
  group_by(st_coverage, race, post, year) %>%
  summarize(vote = weighted.mean(vote, w = weight_cumulative, na.rm = T)) %>% 
  pivot_wider(names_from = race, values_from = vote) %>%
  mutate(vote_gap = Black - White) %>% 
  clean_names()

registration_pre_post <- voting_data %>%
  group_by(st_coverage, race, post, year) %>%
  summarize(registered = weighted.mean(registered, w = weight_cumulative, na.rm = T)) %>% 
  pivot_wider(names_from = race, values_from = registered) %>%
  mutate(registered_gap = Black - White) %>% 
  clean_names()

partisanship <- voting_data %>%
  group_by(st_coverage, race) %>%
  summarize(
    lean_r = weighted.mean(i_republican, w = weight_cumulative, na.rm = T),
    lean_d = weighted.mean(i_democrat, w = weight_cumulative, na.rm = T)
  )

summary_statistics <- tibble(
  obs = list(n_obs),
  turnout = list(turnout_pre_post),
  registration = list(registration_pre_post),
  partisanship = list(partisanship)
)  

saveRDS(summary_statistics, "Results/summary_statistics.rds")


# Turnout panels ----------------------------------------------------------

absolute_turnout <- voting_data %>%
  group_by(st_coverage, race, year) %>%
  summarize(vote = weighted.mean(vote * 100, w = weight_cumulative, na.rm = T)) %>%
  ungroup()

relative_turnout <- absolute_turnout %>%
  pivot_wider(names_from = race, values_from = vote) %>%
  mutate(vote = Black - White,
         race = "Panel C: Relative turnout (Black - White)") %>%
  select(st_coverage, race, year, vote)

turnout <- bind_rows(absolute_turnout, relative_turnout) %>%
  mutate(
    race = case_when(
      race == "Black" ~ "Panel A: Black turnout",
      race == "White" ~ "Panel B: White turnout",
      T ~ race
    ),
    election = case_when(
      year %in% c(2008, 2012, 2016) ~ "Presidential elections",
      T ~ "Midterm elections"
    ) %>% as.factor() %>% fct_relevel("Presidential elections", "Midterm elections")
  )

rm(absolute_turnout, relative_turnout)

panel_a_b <- turnout %>% 
  filter(race != "Panel C: Relative turnout (Black - White)") %>% 
  ggplot(aes(
    x = year,
    y = vote,
    color = st_coverage,
    shape = st_coverage,
    linetype = election
  )) +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  annotate(
    "label",
    x = 2013,
    y = 45,
    label = "Shelby",
    fontface = "italic",
    size = 7 / .pt
  ) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(
    size = 1.5,
    fill = "white",
    stroke = 2,
    alpha = 1
  ) +
  facet_wrap( ~ race, scales = "free", ncol = 2) +
  scale_y_continuous(limits = c(28, 69), breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_color_manual(
    breaks = c(
      "Full",
      "Partial",
      "Uncovered"
    ),
    labels = c(
      "Fully covered states",
      "Partially covered states",
      "Uncovered states"
    ),
    values = c(pink, purple, green)
  ) +
  scale_shape_manual(
    breaks = c(
      "Full",
      "Partial",
      "Uncovered"
    ),
    labels = c(
      "Fully covered states",
      "Partially covered states",
      "Uncovered states"
    ),
    values = c(21, 24, 22)
  ) +
  scale_linetype_manual(
    values = c("solid", "dotdash")
  ) +
  labs(y = "Turnout rate (%)") +
  guides(linetype = guide_legend(keywidth = 2.5)) +
  theme_cowplot(font_size = 9) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 9),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = 'cm')),
    legend.title = element_blank()
  )

panel_c <- turnout %>% 
  filter(race == "Panel C: Relative turnout (Black - White)") %>% 
  ggplot(aes(
    x = year,
    y = vote,
    color = st_coverage,
    shape = st_coverage,
    linetype = election
  )) +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  annotate(
    "label",
    x = 2013,
    y = -3,
    label = "Shelby",
    fontface = "italic",
    size = 7 / .pt
  ) +
  annotate(
    "segment",
    x = 2016,
    xend = 2016,
    y = 0,
    yend = -5.75,
    size = 1,
    arrow = arrow(type = "closed", length = unit(0.2, "cm"))
  ) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(
    size = 1.5,
    fill = "white",
    stroke = 2,
    alpha = 1
  ) +
  facet_wrap( ~ race, scales = "free", ncol = 1) +
  scale_y_continuous(limits = c(-25, 0), breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_color_manual(
    breaks = c(
      "Full",
      "Partial",
      "Uncovered"
    ),
    labels = c(
      "Fully covered states",
      "Partially covered states",
      "Uncovered states"
    ),
    values = c(pink, purple, green)
  ) +
  scale_shape_manual(
    breaks = c(
      "Full",
      "Partial",
      "Uncovered"
    ),
    labels = c(
      "Fully covered states",
      "Partially covered states",
      "Uncovered states"
    ),
    values = c(21, 24, 22)
  ) +
  scale_linetype_manual(
    values = c("solid", "dotdash")
  ) +
  labs(y = "Turnout differential") +
  guides(linetype = guide_legend(keywidth = 2.5)) +
  theme_cowplot(font_size = 9) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 9),
    axis.title.x = element_blank(),
    legend.position = "none"
  )


# Registration panels -----------------------------------------------------

absolute_registration <- voting_data %>%
  group_by(st_coverage, race, year) %>%
  summarize(registered = weighted.mean(registered * 100, w = weight_cumulative, na.rm = T)) %>%
  ungroup()

relative_registration <- absolute_registration %>%
  pivot_wider(names_from = race, values_from = registered) %>%
  mutate(registered = Black - White,
         race = "Panel F: Relative registration (Black - White)") %>%
  select(st_coverage, race, year, registered)

registration <- bind_rows(absolute_registration, relative_registration) %>%
  mutate(
    race = case_when(
      race == "Black" ~ "Panel D: Black registration",
      race == "White" ~ "Panel E: White registration",
      T ~ race
    ),
    election = case_when(
      year %in% c(2008, 2012, 2016) ~ "Presidential elections",
      T ~ "Midterm elections"
    ) %>% as.factor() %>% fct_relevel("Presidential elections", "Midterm elections")
  )

rm(absolute_registration, relative_registration)

panel_d_e <- registration %>% 
  filter(race != "Panel F: Relative registration (Black - White)") %>% 
  ggplot(aes(
    x = year,
    y = registered,
    color = st_coverage,
    shape = st_coverage,
    linetype = election
  )) +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  annotate(
    "label",
    x = 2013,
    y = 60,
    label = "Shelby",
    fontface = "italic",
    size = 7 / .pt
  ) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(
    size = 1.5,
    fill = "white",
    stroke = 2,
    alpha = 1
  ) +
  facet_wrap( ~ race, scales = "free", ncol = 2) +
  scale_y_continuous(limits = c(49, 77), breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_color_manual(
    breaks = c(
      "Full",
      "Partial",
      "Uncovered"
    ),
    labels = c(
      "Fully covered states",
      "Partially covered states",
      "Uncovered states"
    ),
    values = c(pink, purple, green)
  ) +
  scale_shape_manual(
    breaks = c(
      "Full",
      "Partial",
      "Uncovered"
    ),
    labels = c(
      "Fully covered states",
      "Partially covered states",
      "Uncovered states"
    ),
    values = c(21, 24, 22)
  ) +
  scale_linetype_manual(
    values = c("solid", "dotdash")
  ) +
  labs(y = "Registration rate (%)") +
  guides(linetype = guide_legend(keywidth = 2.5)) +
  theme_cowplot(font_size = 9) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 9),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = 'cm')),
    legend.title = element_blank()
  )

panel_f <- registration %>% 
  filter(race == "Panel F: Relative registration (Black - White)") %>% 
  ggplot(aes(
    x = year,
    y = registered,
    color = st_coverage,
    shape = st_coverage,
    linetype = election
  )) +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  annotate(
    "label",
    x = 2013,
    y = -1,
    label = "Shelby",
    fontface = "italic",
    size = 7 / .pt
  ) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(
    size = 1.5,
    fill = "white",
    stroke = 2,
    alpha = 1
  ) +
  facet_wrap( ~ race, scales = "free", ncol = 1) +
  scale_y_continuous(limits = c(-23, 2), breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_color_manual(
    breaks = c(
      "Full",
      "Partial",
      "Uncovered"
    ),
    labels = c(
      "Fully covered states",
      "Partially covered states",
      "Uncovered states"
    ),
    values = c(pink, purple, green)
  ) +
  scale_shape_manual(
    breaks = c(
      "Full",
      "Partial",
      "Uncovered"
    ),
    labels = c(
      "Fully covered states",
      "Partially covered states",
      "Uncovered states"
    ),
    values = c(21, 24, 22)
  ) +
  scale_linetype_manual(
    values = c("solid", "dotdash")
  ) +
  labs(y = "Registration differential") +
  guides(linetype = guide_legend(keywidth = 2.5)) +
  theme_cowplot(font_size = 9) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 9),
    axis.title.x = element_blank(),
    legend.position = "none"
  )


# Combine panels ----------------------------------------------------------

legend <- get_legend(panel_a_b)

turnout_panels <- plot_grid(
  panel_a_b + theme(legend.position = "none"),
  panel_c,
  align = "h",
  axis = "l",
  rel_widths = c(0.655, 0.345)
)

registration_panels <- plot_grid(
  panel_d_e + theme(legend.position = "none"),
  panel_f,
  align = "h",
  axis = "l",
  rel_widths = c(0.655, 0.345)
)

plot_grid(
  legend,
  turnout_panels,
  registration_panels,
  ncol = 1,
  align = "v",
  axis = "l",
  rel_heights = c(0.11, 0.445, 0.445)
)

ggsave(
  "Results/figure2.png",
  width = 9.75,
  height = 5.125,
  units = "in"
)