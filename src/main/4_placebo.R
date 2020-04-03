##########################################################################
# File: 4_placebo.R
# Description: Run placebo test at 3-mo and 5-mo before true adoption
#              Estimate training effect on early- and late-adopters
#              Report Table S2
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/3_estimation.RData"))

# set constants
nboots <- 2000

# run placebo tests for IFE models
models <-
  mutate(
    models,
    placebo3 = pmap(list(formula, fit),
                    ~ placebo_test(y, formula = .x, period = c(-3, 0),
                                   tuning = .y$r.cv, nboots = nboots,
                                   seed = 29590)),
    placebo5 = pmap(list(formula, fit),
                    ~ placebo_test(y, formula = .x, period = c(-5, 0),
                                   tuning = .y$r.cv, nboots = nboots,
                                   seed = 29590))
  )

# Table S2
(
  table_s2 <-
    models %>%
    transmute(outcome, placebo3, placebo5) %>%
    pivot_longer(cols = c(placebo3, placebo5)) %>%
    transmute(outcome, period = parse_number(name),
              report = map(value, ~ as_tibble(.x$est.placebo))) %>%
    unnest(cols = c(report)) %>%
    clean_names() %>%
    mutate_at(
      vars(att_placebo, s_e, ci_lower, ci_upper),
      function(x)
        (x * n_distinct(y$cluster[y$month == y$trained_in_month])) /
        sum(y$officers[y$month == y$trained_in_month]) * 100
    )
)

# save
save.image(here("products/rdata/4_placebo.RData"))

