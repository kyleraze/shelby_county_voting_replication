# Preliminaries -----------------------------------------------------------

library(groundhog)
groundhog.library(c("tidyverse", "haven"), "2021-07-01")

# "not in" operator
`%nin%` = Negate(`%in%`)


# Import data -------------------------------------------------------------

cces <-
  read_dta("Data/cumulative_2006-2019.dta") %>%
  # Retain Stata labels
  as_factor() %>%
  mutate(fips = as.numeric(county_fips)) %>%
  # Election years only
  filter(year %% 2 == 0 & !is.na(fips)) %>%
  select(
    case_id,
    year,
    weight_cumulative,
    state,
    st,
    fips,
    gender,
    age,
    race,
    pid3_leaner,
    vv_turnout_gvm,
    vv_regstatus
  ) %>%
  rename(state_po = st) %>%
  mutate(fips = as.factor(fips))


# Outcomes and treatment status -------------------------------------------

cces <- cces %>%
  mutate(
    # Demographics
    i_black = case_when(race == "Black" ~ 1, T ~ 0),
    age_sq = age ^ 2,
    # Partisanship
    pid3_leaner = as.factor(
      case_when(
        pid3_leaner == "Democrat (Including Leaners)" ~ "Democrat",
        pid3_leaner == "Republican (Including Leaners)" ~ "Republican",
        pid3_leaner == "Independent (Excluding Leaners)" ~ "Independent"
      )
    ),
    i_democrat = case_when(pid3_leaner == "Democrat" ~ 1, T ~ 0),
    i_independent = case_when(pid3_leaner == "Independent" ~ 1, T ~ 0),
    i_republican = case_when(pid3_leaner == "Republican" ~ 1, T ~ 0),
    # Turnout
    vote = case_when(
      vv_turnout_gvm == "Voted" ~ 1,
      vv_turnout_gvm %in% c("No Record of Voting", "No Voter File") ~ 0
    ), 
    ## Remove invalid data. See:
    ##   Grimmer et al. (2018)
    ##   2008 guide: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/YUYIVB/ODKLI3
    ##   2010 guide: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/VKKRWA/CO1QSD
    vote = case_when(year == 2006 ~ NA_real_,
                     state == "Virginia" & year %in% c(2008, 2010) ~ NA_real_,
                     T ~ vote), 
    # Registration
    registered = case_when(
      vv_regstatus %in% c("Active", "Inactive") ~ 1,
      vv_regstatus %in% c(
        "Unregistered",
        "No Record of Registration",
        "Dropped",
        ## From the vote validation guides: "Some matched respondents...are
        ## listed as 'multipleAppearances.' The multiple appearances code means
        ## that the individual was located by Catalist in a [sic] one state but
        ## was registered to vote in a different state. This typically happens
        ## because somebody has filed a change of address form with the post
        ## office but has not yet changed their voter registration to a new
        ## state" (e.g., see
        ## https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/XFXJVY/ECKEP7&version=5.0).
        ## This suggests that those with the "Multiple Appearances" response
        ## were not registered in the state in which they were living. If the
        ## goal of the registration analysis is to measure changes in the
        ## proportion of eligible voters who could show up to the polls and vote
        ## on Election Day in their current state of residence, then "Multiple
        ## Appearances" ==> not registered.
        "Multiple Appearances"
      ) ~ 0
    ), 
    ## Remove invalid data. See:
    ##   Grimmer et al. (2018)
    ##   2008 guide: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/YUYIVB/ODKLI3
    ##   2010 guide: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/VKKRWA/CO1QSD
    registered = case_when(
      year == 2006 ~ NA_real_,
      state == "Virginia" & year %in% c(2008, 2010) ~ NA_real_,
      T ~ registered
    ),
    # Timing
    post = case_when(year > 2013 ~ 1, TRUE ~ 0),
    election = as.factor(case_when(year %% 4 == 0 ~ "Presidential", TRUE ~ "Midterm")),
    # State-level coverage
    st_coverage = as.factor(
      case_when(
        state_po %in% c(
          "AL",
          "AK",
          "AZ",
          "GA",
          "LA",
          "MS",
          "SC",
          "TX",
          "VA"
        ) ~ "Full",
        state_po %in% c(
          "CA",
          "MI",
          "FL",
          "SD",
          "NC",
          "NY"
        ) ~ "Partial",
        TRUE ~ "Uncovered"
      )
    ),
    st_coverage_full = case_when(st_coverage == "Full" ~ 1, T ~ 0),
    st_coverage_partial = case_when(st_coverage == "Partial" ~ 1, T ~ 0),
    # State-level treatment indicators
    black_full_shelby = i_black * st_coverage_full * post,
    black_partial_shelby = i_black * st_coverage_partial * post,
    black_full = i_black * st_coverage_full,
    black_partial = i_black * st_coverage_partial,
    black_full_shelby_mid = case_when(black_full_shelby == 1 & election == "Midterm" ~ 1, T ~ 0),
    black_full_shelby_pres = case_when(black_full_shelby == 1 & election == "Presidential" ~ 1, T ~ 0),
    black_partial_shelby_mid = case_when(black_partial_shelby == 1 & election == "Midterm" ~ 1, T ~ 0),
    black_partial_shelby_pres = case_when(black_partial_shelby == 1 & election == "Presidential" ~ 1, T ~ 0),
    # Fixed effects
    state_year = paste(state, year, sep = "_"),
    race_state = paste(race, state, sep = "_"),
    race_year = paste(race, year, sep = "_")
  )


# Sample Selection --------------------------------------------------------

cces <- cces %>%
  filter(
    race %in% c("Black", "White"),
    # Spotty validation in 2006 (see Grimmer et al., 2018)
    year != 2006,
    # No validation in VA until 2012 (see 2008 and 2010 guides)
    !(state == "Virginia" & year %in% c(2008, 2010))
  )


# Export data -------------------------------------------------------------

saveRDS(cces, "Data/cces.rds")