##########################################################################
# File: 10_termination.R
# Description: Estimate ATT for all officers; including officers who
#              terminated employment at CPD during observation period
#              Plot Figure S7
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/4_placebo.RData"))

# set constants
nboots <- 2000

# build training matrix
y_terminated <-
  generate_tscs(
    filter(training, resigned >= ymd("2016-04-01") | is.na(resigned)),
    complaints = complaints,
    sustained  = filter(complaints, !is.na(settlement) | sustained == 1),
    force      = force,
    cluster_by = "day"
  ) %>%
  mutate(coverage = ifelse(month <= settlement_terminus, "sfc", "fc")) %>%
  filter(month <= discontinuity)

# estimate ATT
termination_models <-
  expand_grid(
    formula = c(complaints ~ trained,
                sustained  ~ trained,
                force      ~ trained)
  ) %>%
  mutate(
    outcome = map_chr(formula, ~ as_string(lhs(.))),
    fit     = map(formula,
                  ~ estimate_model(y_terminated, formula = .x, method = "ife",
                                   nboots = nboots, seed = 29590))
  )

# compute cumulative att
source(here("src/aux/cumulative.R"))

n <-
  filter(y, trained == 1) %>%
  mutate(end = interval(trained_in_month, month) %/% months(1)) %>%
  group_by(end) %>%
  summarize(officers = sum(officers), clusters = n_distinct(cluster))

catt <-
  models %>%
  mutate(catt = map(fit, cumulative_att, unit = n)) %>%
  transmute(outcome = str_to_title(outcome), adoption = "All", catt) %>%
  unnest(cols = c(catt))

termination_n <-
  filter(y_terminated, trained == 1) %>%
  mutate(end = interval(trained_in_month, month) %/% months(1)) %>%
  group_by(end) %>%
  summarize(officers = sum(officers), clusters = n_distinct(cluster))

termination_catt <-
  termination_models %>%
  mutate(catt = map(fit, cumulative_att, unit = termination_n)) %>%
  transmute(outcome = str_to_title(outcome), adoption = "All", catt) %>%
  unnest(cols = c(catt))

figure_s7 <-
  bind_rows(
    mutate(catt,
           sample = "All Trained Officers"),
    mutate(termination_catt,
           sample = paste("Excluding Officers whose Employment Ended",
                          "Between Training and Study End"))
  ) %>%
  mutate(outcome = fct_relevel(factor(outcome), "Complaints", "Sustained")) %>%
  ggplot(aes(end, catt, ymin = ci_lower, ymax = ci_upper,
             color = sample, fill = sample, shape = sample)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.1, color = "transparent") +
  geom_line() +
  geom_point() +
  scale_x_continuous("Months Since Training",
                     breaks = seq(0, 24, by = 6)) +
  scale_y_continuous("Cumulative ATT per 100 Officers") +
  scale_color_manual(NULL, values = c(flat[5], flat[6])) +
  scale_fill_manual(NULL,  values = c(flat[5], flat[6])) +
  scale_shape_manual(NULL, values = c(1, 3)) +
  facet_wrap(~ outcome, ncol = 3, scales = "free_y") +
  theme(axis.line  = element_line(size = 0.3),
        axis.text  = element_text(color = "black"),
        axis.ticks = element_line(size = 0.3),
        legend.position = "top",
        legend.margin = margin(b = 0, unit = "lines"))

# write
figure_s7

ggsave(figure_s7, filename = here("products/figures/figure_s7.pdf"),
       width = 10, height = 4.2, device = cairo_pdf)

# save
save.image(here("products/rdata/10_termination.RData"))
