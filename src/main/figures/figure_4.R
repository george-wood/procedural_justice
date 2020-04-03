##########################################################################
# File: figure_4.R
# Description: Produce Figure 4
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/4_placebo.RData"))

# officer counts by period
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

# compute cumulative att
catt <-
  models %>%
  mutate(catt = map(fit, cumulative_att, units = n_all)) %>%
  transmute(outcome = str_to_title(outcome), adoption = "All", catt) %>%
  unnest(cols = c(catt))

# phase models
adoption_catt <-
  models %>%
  mutate(early = map(early, cumulative_att, units = n_early),
         late  = map(late,  cumulative_att, units = n_late)) %>%
  transmute(outcome = str_to_title(outcome), early, late) %>%
  pivot_longer(cols = c(early, late),
               names_to = "adoption", values_to = "catt") %>%
  unnest(cols = c(catt)) %>%
  mutate(adoption = str_to_title(adoption))

figure_4 <-
  bind_rows(catt, adoption_catt) %>%
  mutate(outcome = fct_relevel(factor(outcome), "Complaints", "Sustained")) %>%
  ggplot(aes(end, catt, ymin = ci_lower, ymax = ci_upper,
             color = adoption, fill = adoption, shape = adoption)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.1, color = "transparent") +
  geom_line() +
  geom_point() +
  scale_x_continuous("Months Since Training",
                     breaks = seq(0, 24, by = 6)) +
  scale_y_continuous("Cumulative ATT per 100 Officers") +
  scale_color_manual("Adoption", values = c(flat[5], flat[6], flat[1])) +
  scale_fill_manual("Adoption",  values = c(flat[5], flat[6], flat[1])) +
  scale_shape_manual("Adoption", values = c(1, 3, 5)) +
  facet_wrap(~ outcome, ncol = 3, scales = "free_y") +
  theme(axis.line  = element_line(size = 0.3),
        axis.text  = element_text(color = "black"),
        axis.ticks = element_line(size = 0.3),
        legend.position = "top",
        legend.margin = margin(b = 0, unit = "lines"))

# write figure
figure_4

ggsave(figure_4, filename = here("products/figures/figure_4.pdf"),
       width = 10, height = 4.2, device = cairo_pdf)
