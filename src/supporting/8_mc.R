##########################################################################
# File: 8_mc.R
# Description: Estimate ATT using matrix completion
#              Report Table S1
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/3_estimation.RData"))

# set constants
nboots <- 2000

# estimate ATT using matrix completion
mc_models <-
  expand_grid(formula = c(complaints ~ trained,
                          sustained  ~ trained,
                          force      ~ trained)) %>%
  mutate(
    outcome = map_chr(formula, ~ as_string(lhs(.))),
    fit     = map(formula, ~ estimate_model(y, formula = .x, method = "mc",
                                            nboots = nboots, seed = 29590))
  )

# compute officers per period relative to training
n <-
  filter(y, trained == 1) %>%
  mutate(end = interval(trained_in_month, month) %/% months(1)) %>%
  group_by(end) %>%
  summarize(officers = sum(officers), clusters = n_distinct(cluster))

mc_catt <-
  mc_models %>%
  mutate(catt = map(fit, cumulative_att, units = n)) %>%
  transmute(outcome = str_to_title(outcome), adoption = "All", catt) %>%
  unnest(cols = c(catt))

# table s1
(
  table_s1 <-
    bind_cols(
      mc_catt %>%
        filter(end == 24) %>%
        transmute(
          outcome, catt, s_e, ci_lower, ci_upper, p_value,
          month_FE     = "yes",
          cluster_FE   = "yes",
          officers     = sum(y$officers[y$period == 1]),
        ) %>%
        mutate_if(is.numeric, round, 2),
      mc_models %>%
        transmute(
          months   = map_dbl(fit, ~ nrow(.$D.dat)),
          clusters = map_dbl(fit, ~ length(.$id)),
          trained  = map_dbl(fit, ~ sum(.$unit.type == 3)),
          control  = map_dbl(fit, ~ sum(.$unit.type == 1)),
          observations = clusters * months
        )
    )
)

# save
save.image(here("products/rdata/8_mc.RData"))
