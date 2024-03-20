# Preliminaries -----------------------------------------------------------

library(groundhog)
groundhog.library(
  c(
    "tidyverse",
    "data.table",
    "broom",
    "fixest",
    "cowplot"
  ),
  "2021-07-01"
)

if(!interactive()) pdf(NULL) 

cces <- readRDS("Data/cces.rds")


# Event-study regressions -------------------------------------------------

# Full sample (w/ full set of controls and fixed effects)
ev <- feols(
  c(vote, registered) ~ i(year, black_full, ref = 2012) + i(year, black_partial, ref = 2012) + gender + age + age_sq |
    race_state + race_year + state_year,
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Pre-trend tests
pretrend_tests <-
  wald(
    ev[lhs = "vote"],
    "year::2008:black_full|year::2010:black_full|year::2008:black_partial|year::2010:black_partial",
    se = "cluster",
    cluster = ~ state
  ) %>%
  as_tibble() %>%
  mutate(test = "all covered", lhs = "vote") %>% 
  add_row(
    wald(
      ev[lhs = "registered"],
      "year::2008:black_full|year::2010:black_full|year::2008:black_partial|year::2010:black_partial",
      se = "cluster",
      cluster = ~ state
    ) %>%
      as_tibble() %>%
      mutate(test = "all covered", lhs = "registered")
  ) %>% 
  add_row(
    wald(
      ev[lhs = "vote"],
      "year::2008:black_full|year::2010:black_full",
      se = "cluster",
      cluster = ~ state
    ) %>%
      as_tibble() %>%
      mutate(test = "fully covered", lhs = "vote")
  ) %>% 
  add_row(
    wald(
      ev[lhs = "registered"],
      "year::2008:black_full|year::2010:black_full",
      se = "cluster",
      cluster = ~ state
    ) %>%
      as_tibble() %>%
      mutate(test = "fully covered", lhs = "registered")
  ) %>% 
  add_row(
    wald(
      ev[lhs = "vote"],
      "year::2008:black_partial|year::2010:black_partial",
      se = "cluster",
      cluster = ~ state
    ) %>%
      as_tibble() %>%
      mutate(test = "partially covered", lhs = "vote")
  )  %>% 
  add_row(
    wald(
      ev[lhs = "registered"],
      "year::2008:black_partial|year::2010:black_partial",
      se = "cluster",
      cluster = ~ state
    ) %>%
      as_tibble() %>%
      mutate(test = "partially covered", lhs = "registered")
  )

# Tidy results
ev <- bind_rows(
  ev[lhs = "vote"] %>% 
    tidy(conf.int = T) %>% 
    mutate(lhs = "vote"),
  ev[lhs = "registered"] %>% 
    tidy(conf.int = T) %>% 
    mutate(lhs = "registered")
) %>% 
  filter(str_detect(term, "year"))

# Export results
saveRDS(ev, "Results/event_studies.rds")
saveRDS(pretrend_tests, "Results/pretrend_tests.rds")


# Triple-difference regressions -------------------------------------------

# Full sample (w/ fixed effects)
triple_1 <- feols(
  c(vote, registered) ~ black_full_shelby + black_partial_shelby |
    race_state + race_year + state_year,
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Full sample (w/ controls and fixed effects)
triple_2 <- feols(
  c(vote, registered) ~ black_full_shelby + black_partial_shelby + gender + age + age_sq |
    race_state + race_year + state_year,
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Full sample (w/ controls, fixed effects, and race-state time trends)
triple_3 <- feols(
  c(vote, registered) ~ black_full_shelby + black_partial_shelby + gender + age + age_sq |
    race_state + race_year + state_year + race_state[year],
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Heterogeneity (w/ fixed effects)
triple_4 <- feols(
  c(vote, registered) ~ black_full_shelby_pres + black_full_shelby_mid + black_partial_shelby_pres + black_partial_shelby_mid |
    race_state + race_year + state_year,
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces 
)

# Heterogeneity (w/ controls and fixed effects)
triple_5 <- feols(
  c(vote, registered) ~ black_full_shelby_pres + black_full_shelby_mid + black_partial_shelby_pres + black_partial_shelby_mid + gender + age + age_sq |
    race_state + race_year + state_year,
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Heterogeneity (w/ controls, fixed effects, and race-state time trends)
triple_6 <- feols(
  c(vote, registered) ~ black_full_shelby_pres + black_full_shelby_mid + black_partial_shelby_pres + black_partial_shelby_mid + gender + age + age_sq |
    race_state + race_year + state_year + race_state[year],
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Tidy results
triple_differences <- bind_rows(
  triple_1[lhs = "vote"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = F, race_state_trends = F, lhs = "vote"),
  triple_2[lhs = "vote"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = T, race_state_trends = F, lhs = "vote"),
  triple_3[lhs = "vote"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = T, race_state_trends = T, lhs = "vote"),
  triple_4[lhs = "vote"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = F, race_state_trends = F, lhs = "vote"),
  triple_5[lhs = "vote"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = T, race_state_trends = F, lhs = "vote"),
  triple_6[lhs = "vote"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = T, race_state_trends = T, lhs = "vote"),
  triple_1[lhs = "registered"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = F, race_state_trends = F, lhs = "registered"),
  triple_2[lhs = "registered"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = T, race_state_trends = F, lhs = "registered"),
  triple_3[lhs = "registered"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = T, race_state_trends = T, lhs = "registered"),
  triple_4[lhs = "registered"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = F, race_state_trends = F, lhs = "registered"),
  triple_5[lhs = "registered"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = T, race_state_trends = F, lhs = "registered"),
  triple_6[lhs = "registered"] %>% 
    tidy(conf.int = T) %>% 
    mutate(controls = T, race_state_trends = T, lhs = "registered")
) %>% 
  filter(str_detect(term, "shelby"))

# Export results
saveRDS(triple_differences, "Results/triple_differences.rds")


# Regression table --------------------------------------------------------

# Effective observations
eff_obs <- cces %>% 
  filter(weight_cumulative > 0) %>% 
  select(race, state, year) %>% 
  distinct() %>% 
  nrow()

setFixest_dict(
  c(
    vote = "Turnout",
    registered = "Registration",
    black_full_shelby = "Black $\\times$ fully covered $\\times$ \\textit{Shelby}",
    black_partial_shelby = "Black $\\times$ partially covered $\\times$ \\textit{Shelby}",
    black_full_shelby_mid =  "Black $\\times$ fully covered $\\times$ \\textit{Shelby} $\\times$ midterm",
    black_full_shelby_pres = "Black $\\times$ fully covered $\\times$ \\textit{Shelby} $\\times$ presidential",
    black_partial_shelby_mid = "Black $\\times$ partially covered $\\times$ \\textit{Shelby} $\\times$ midterm",
    black_partial_shelby_pres = "Black $\\times$ partially covered $\\times$ \\textit{Shelby} $\\times$ presidential",
    race_state = "Race $\\times$ state",
    race_year = "Race $\\times$ year",
    state_year = "State $\\times$ year"
  )
)

etable(
  triple_1[lhs = "vote"],
  triple_2[lhs = "vote"],
  triple_3[lhs = "vote"],
  triple_4[lhs = "vote"],
  triple_5[lhs = "vote"],
  triple_6[lhs = "vote"],
  drop = c("age", "age_sq", "gender"),
  drop.section = "slopes",
  order = c("fully"),
  extraline = list(
    "-Demographic controls" = c(F, T, T, F, T, T),
    "-Race $\\times$ state time trends" = c(F, F, T, F, F, T),
    "_Effective observations (race $\\times$ state $\\times$ year)" = c(rep(eff_obs, times = 6))
  ), 
  digits = 3,
  digits.stats = 2,
  fitstat = ~ n,
  tex = T,
  replace = T,
  style.tex = style.tex("aer", fixef.title = "\\midrule"),
  file = "Results/table1.tex"
)

etable(
  triple_1[lhs = "registered"],
  triple_2[lhs = "registered"],
  triple_3[lhs = "registered"],
  triple_4[lhs = "registered"],
  triple_5[lhs = "registered"],
  triple_6[lhs = "registered"],
  drop = c("age", "age_sq", "gender"),
  drop.section = "slopes",
  order = c("fully"),
  extraline = list(
    "-Demographic controls" = c(F, T, T, F, T, T),
    "-Race $\\times$ state time trends" = c(F, F, T, F, F, T),
    "_Effective observations (race $\\times$ state $\\times$ year)" = c(rep(eff_obs, times = 6))
  ), 
  digits = 3,
  digits.stats = 2,
  fitstat = ~ n,
  tex = T,
  replace = T,
  style.tex = style.tex("aer", fixef.title = "\\midrule"),
  file = "Results/table2.tex"
)


# Event study plot --------------------------------------------------------

green <- "#007935"
purple <- "#9370DB"
pink <- "#e64173"
grey <- "#808080"

options(
  tikzLatexPackages = c(
    getOption("tikzLatexPackages"),
    "\\usepackage{libertine}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage{textcomp}",
    "\\usepackage{bbold}"
  )
)

full_label <- '"Black" %*% "fully covered" %*% bold(1)(t == tau)~~~~~~~""'
partial_label <- '"Black" %*% "partially covered" %*% bold(1)(t == tau)'

test <- ev %>%
  mutate(
    coverage = case_when(
      str_detect(term, coll("full")) ~ full_label,
      str_detect(term, coll("partial")) ~ partial_label
    ),
    year = case_when(
      str_detect(term, coll("2008")) ~ 2008,
      str_detect(term, coll("2010")) ~ 2010,
      str_detect(term, coll("2014")) ~ 2014,
      str_detect(term, coll("2016")) ~ 2016,
      str_detect(term, coll("2018")) ~ 2018
    )
  ) %>%
  filter(!is.na(coverage)) %>%
  select(coverage, year, estimate, conf.low, conf.high, lhs) %>%
  add_row(
    coverage = full_label,
    year = 2012,
    estimate = 0,
    conf.low = NA,
    conf.high = NA,
    lhs = "vote"
  ) %>%
  add_row(
    coverage = full_label,
    year = 2012,
    estimate = 0,
    conf.low = NA,
    conf.high = NA,
    lhs = "registered"
  ) %>%
  add_row(
    coverage = partial_label,
    year = 2012,
    estimate = 0,
    conf.low = NA,
    conf.high = NA,
    lhs = "vote"
  ) %>%
  add_row(
    coverage = partial_label,
    year = 2012,
    estimate = 0,
    conf.low = NA,
    conf.high = NA,
    lhs = "registered"
  )

test %>%
  mutate(lhs = case_when(
    lhs == "vote" ~ "Panel A: Turnout",
    lhs == "registered" ~ "Panel B: Registration"
  )) %>% 
  ggplot(aes(
    x = year,
    y = estimate,
    color = coverage,
    shape = coverage
  )) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  annotate(
    "label",
    x = 2013,
    y = 0.14,
    label = "Shelby",
    fontface = "italic",
    size = 7 / .pt
  ) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    size = 1.5,
    alpha = 0.5,
    position = position_dodge2(preserve = "single", width = 1.25)
  ) +
  geom_point(
    size = 1.5,
    fill = "white",
    stroke = 2,
    position = position_dodge2(preserve = "single", width = 1.25)
  ) +
  facet_wrap( ~ lhs, scales = "free", ncol = 2) +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) + 
  scale_y_continuous(limits = c(-0.10, 0.15), breaks = scales::pretty_breaks()) +
  labs(y = "Estimate", x = expression(tau), color = "Coefficient: ", shape = "Coefficient: ") + 
  guides(color = guide_legend(
    title.position = "left"
  )) +
  scale_color_manual(values = c(pink, purple), labels = scales::parse_format()) +
  scale_shape_manual(values = c(21, 24), labels = scales::parse_format()) +
  theme_cowplot(font_size = 9) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 9),
    plot.title = element_text(size = 9, face = "plain", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, margin = margin(l = 0.3, unit = 'cm')),
    legend.position = "top",
    legend.justification = "left", 
    legend.direction = "vertical",
    legend.box.just = "center"
  )

ggsave(
  "Results/figure3.png",
  width = 6.325,
  height = 3.5,
  units = "in"
)