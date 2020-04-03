##########################################################################
# File: 9_force.R
# Description: Estimate ATT by force type using IFE
#              Plot Figure S6
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/4_placebo.RData"))

# set constants
nboots <- 2000

# build training matrix
force_models <-
  tibble(category = unique(force$category)) %>%
  filter(!is.na(category), category != "other") %>%
  mutate(y = map(category,
                 ~ generate_tscs(training,
                                 force = filter(force, category == .x)) %>%
                   filter(month <= discontinuity)))

# estimate ATT
force_models <-
  mutate(force_models,
         fit = map(y,
                   ~ estimate_model(.x, formula = force ~ trained,
                                    method = "ife", nboots = nboots,
                                    seed = 29590)))

# tidy ATT
force_models <-
  mutate(
    force_models,
    cf  = pmap(list(fit, category), ~ tidy_counterfactual(.x, outcome = .y)),
    att = pmap(list(cf, fit, category),
               function(a, b, c) tidy_att(a, b, outcome = c)),
    category = str_to_title(gsub("response", "action",
                                 gsub("_", " ", category)))
  )

# plot ATT
figure_s6_top <-
  bind_rows(force_models$att) %>%
  mutate(outcome = str_to_title(gsub(outcome,
                                     pattern = "_", replacement = " ")),
         outcome = fct_relevel(outcome,
                               "Force Mitigation",
                               "Control Tactics",
                               "Action Without Weapons")) %>%
  ggplot(aes(x, att, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
  geom_ribbon(alpha = 0.2) +
  geom_line(color = "grey60", size = 0.4) +
  geom_point(aes(fill = fill_att, group = outcome),
             shape = 21, stroke = 0.4, color = "grey10", show.legend = FALSE) +
  scale_x_continuous("Month Relative to Training",
                     expand = expand_scale(mult = 0.05),
                     breaks = seq(-48, 36, by = 12)) +
  scale_y_continuous("ATT on Frequency per 100 Officers",
                     limits = function(x) c(-max(abs(x)), max(abs(x))),
                     expand = expand_scale(add = c(0, 0)),
                     breaks = pretty_breaks()) +
  scale_fill_continuous_diverging(palette = "Purple-Brown",
                                  rev = FALSE, mid = 0.63) +
  facet_wrap(~ outcome) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(size = 0.3),
        axis.text  = element_text(color = "black"),
        axis.ticks = element_line(size = 0.3))

# officer counts by period
n <-
  filter(y, trained == 1) %>%
  mutate(end = interval(trained_in_month, month) %/% months(1)) %>%
  group_by(end) %>%
  summarize(officers = sum(officers), clusters = n_distinct(cluster))

# compute cumulative att
force_catt <-
  force_models %>%
  transmute(category, catt = map(fit, cumulative_att)) %>%
  unnest(cols = c(catt))

limiter <-
  tibble(category = rep(factor(unique(force_catt$category)), each = 2),
         catt = c(2, -6.5, 2, -6.5, 1.5, -1, 1.5, -1))

figure_s6_bottom <-
  force_catt %>%
  mutate(category = fct_relevel(factor(category),
                               "Force Mitigation",
                               "Control Tactics",
                               "Action Without Weapons")) %>%
  ggplot(aes(end, catt, ymin = ci_lower, ymax = ci_upper)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.1, color = "transparent") +
  geom_line() +
  geom_point(size = 1) +
  geom_point(data = limiter, inherit.aes = FALSE, aes(x = 1, y = catt),
             color = "transparent") +
  scale_x_continuous("Months Since Training",
                     breaks = seq(0, 24, by = 6)) +
  scale_y_continuous("Cumulative ATT per 100 Officers",
                     breaks = pretty_breaks(n = 4)) +
  scale_color_manual("Adoption", values = c(flat[5], flat[6], flat[1])) +
  scale_fill_manual("Adoption",  values = c(flat[5], flat[6], flat[1])) +
  scale_shape_manual("Adoption", values = c(1, 3, 5)) +
  facet_wrap(~ category, scales = "free_y") +
  theme(axis.line  = element_line(size = 0.3),
        axis.text  = element_text(color = "black"),
        axis.ticks = element_line(size = 0.3),
        legend.position = "top",
        legend.margin = margin(b = 0, unit = "lines"))

# write graph
figure_s6 <- wrap_plots(figure_s6_top, figure_s6_bottom, ncol = 1)
figure_s6

ggsave(figure_s6, filename = here("products/figures/figure_s6.pdf"),
       width = 8, height = 9, device = cairo_pdf)

# save
save.image(here("products/rdata/9_force.RData"))





