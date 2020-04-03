##########################################################################
# File: 5_report.R
# Description: Report Table 1
#              Report total reduction
#              Report reduction per 100 officers per month
#              Report cumulative reduction after 18 months
#              Report miscellaneous information relating to exclusions and
#              retirement
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/4_placebo.RData"))

# compute officers per period relative to training
n <-
  filter(y, trained == 1) %>%
  mutate(end = interval(trained_in_month, month) %/% months(1)) %>%
  group_by(end)

n_all <-
  summarize(n, officers = sum(officers), clusters = n_distinct(cluster))

n_early <- 
  filter(n, cluster %in% early_adopters) %>%
  summarize(officers = sum(officers), clusters = n_distinct(cluster))

n_late <- 
  filter(n, cluster %in% late_adopters) %>%
  summarize(officers = sum(officers), clusters = n_distinct(cluster))

# compute cumulative ATT
catt <-
  models %>%
  mutate(catt = map(fit, cumulative_att, units = n_all)) %>%
  transmute(outcome = str_to_title(outcome), adoption = "All", catt) %>%
  unnest(cols = c(catt))

# Table 1
(
  table_1 <-
    bind_cols(
      catt %>%
        filter(end == 24) %>%
        transmute(
          outcome, catt, s_e, ci_lower, ci_upper, p_value,
          month_FE     = "yes",
          cluster_FE   = "yes",
          officers     = sum(y$officers[y$period == 1]),
        ) %>%
        mutate_if(is.numeric, round, 2),
      models %>%
        transmute(
          months   = map_dbl(fit, ~ nrow(.$D.dat)),
          clusters = map_dbl(fit, ~ length(.$id)),
          trained  = map_dbl(fit, ~ sum(.$unit.type == 3)),
          control  = map_dbl(fit, ~ sum(.$unit.type == 1)),
          observations = clusters * months
        )
    )
)

# total officers
sum(y$officers[y$period == 1])

# total trained officers
y %>%
  group_by(cluster) %>%
  filter(any(trained == 1), officers == max(officers)) %>%
  distinct(officers, cluster) %>%
  ungroup() %>%
  summarize(total_trained = sum(officers))

# total clusters
n_distinct(y$cluster[y$period == max(y$period)])

# total trained clusters
n_distinct(y$cluster[y$period == max(y$period) & y$trained == 1])

# start date
glue(
  "Pilot date: {min(training$assigned)}
  Roll-out end: {max(training$assigned)}
  Last complaints and force data: {max(y$month)}
  Last sustained and settled data: {max(y$month[grepl('sfc', y$coverage)])}"
)

# total reduction and percentage reduction
left_join(
  catt %>%
    filter(end == 24) %>%
    transmute(outcome, catt, officers, clusters,
              reduction = catt / 100 * officers),
  models %>%
    transmute(
      outcome = str_to_title(outcome),
      observed = map_dbl(
        fit,
        ~ sum(.$Y.dat[between(apply(.$D.dat, 2, cumsum), 1, 24)])
      )
    ),
  by = "outcome"
) %>%
  mutate(counterfactual = observed + abs(reduction),
         pct = reduction / (observed + abs(reduction)) * 100)

# reduction per 100 officers per month
models %>%
  mutate(ct = pmap(list(fit, outcome),
                   ~ tidy_counterfactual(.x, outcome = .y))) %>%
  transmute(ct) %>%
  unnest(cols = c(ct)) %>%
  filter(x >= 1) %>%
  group_by(outcome, name) %>%
  summarize(mean = mean(value))

# reduction per 100 officers after 18 months by adoption time
adoption_catt <-
  models %>%
  mutate(early = map(early, cumulative_att, unit = n_early),
         late  = map(late,  cumulative_att, unit = n_late)) %>%
  transmute(outcome = str_to_title(outcome), early, late) %>%
  pivot_longer(cols = c(early, late),
               names_to = "adoption", values_to = "catt") %>%
  unnest(cols = c(catt)) %>%
  mutate(adoption = str_to_title(adoption))

filter(adoption_catt, end == 18)

# total complaints
sum(y$complaints)

# total use of force
sum(y$force)

# total sustained and settled complaints
sum(y$sustained)

# retirements during study
training %>%
  remove_new_officers() %>%
  filter(resigned < max(y$month))

# new officers exclusion
mutate(training,
       a = ifelse(min(assigned) %m-% months(12) >= appointed, 0, 1)) %>%
  filter(a == 1) %>%
  mutate(tenure = interval(appointed, assigned) %/% days(1)) %>%
  ggplot(aes(tenure)) +
  geom_histogram(binwidth = 30, color = "white")

mutate(training,
       a = ifelse(min(assigned) %m-% months(12) >= appointed, 0, 1)) %>%
  filter(a == 1) %>%
  mutate(tenure = interval(appointed, assigned) %/% days(1)) %>%
  summarize(percent_academy = mean(tenure <= 180))


