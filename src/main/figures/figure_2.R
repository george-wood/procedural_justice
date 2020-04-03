##########################################################################
# File: figure_2.R
# Description: Produce Figure 2
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/4_placebo.RData"))

# tidy results for figure 2
figure_2_data <-
  models %>%
  mutate(
    top    = pmap(list(fit, outcome), ~ tidy_counterfactual(.x, outcome = .y)),
    bottom = pmap(list(top, fit, outcome),
                  function(a, b, c) tidy_att(cf = a, fit = b, outcome = c))
  )

# figure 2 top panel
figure_2_top <-
  bind_rows(figure_2_data$top) %>%
  mutate(outcome = fct_relevel(factor(outcome), "Complaints", "Sustained")) %>%
  ggplot(aes(x, value, linetype = name)) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
  geom_line(aes(color = name)) +
  geom_point(aes(color = name), alpha = 0.2) +
  scale_x_continuous(NULL,
                     expand = expand_scale(mult = 0.05),
                     breaks = seq(-48, 36, by = 12)) +
  scale_y_continuous("Frequency per 100 Officers",
                     limits = c(0, NA),
                     expand = expand_scale(mult = c(0.02, 0.07)),
                     breaks = pretty_breaks()) +
  scale_color_manual(NULL, values = c("grey20", flat[2])) +
  scale_linetype_manual(NULL, values = c("longdash", "solid")) +
  facet_wrap(~ outcome, scales = "free_y") +
  theme(axis.line        = element_line(size = 0.3),
        axis.text        = element_text(color = "black"),
        axis.ticks       = element_line(size = 0.3), 
        axis.title.y     = element_text(margin = margin(r = 0.8, unit = "lines")),
        strip.text       = element_text(size = 9),
        legend.direction = "vertical",
        legend.justification = c(0, 0), 
        legend.position = c(0.005, 0.05))

# figure 2 bottom panel
figure_2_bottom <-
  bind_rows(figure_2_data$bottom) %>%
  mutate(outcome = fct_relevel(factor(outcome), "Complaints", "Sustained")) %>%
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
  scale_y_continuous("ATT on Frequency",
                     limits = function(x) c(-max(abs(x)), max(abs(x))),
                     expand = expand_scale(mult = 0.05),
                     breaks = pretty_breaks()) +
  scale_fill_continuous_diverging(palette = "Purple-Brown",
                                  rev = FALSE, mid = 0.63) +
  facet_wrap(~ outcome, scales = "free_y") +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(size = 0.3),
        axis.text  = element_text(color = "black"),
        axis.ticks = element_line(size = 0.3), 
        strip.text = element_blank())

# compose figure and write
figure_2 <- ggarrange(figure_2_top, figure_2_bottom,
                      ncol = 1, common.legend = FALSE)
figure_2

ggsave(figure_2, filename = here("products/figures/figure_2.pdf"),
       width = 10, height = 5, device = cairo_pdf)

