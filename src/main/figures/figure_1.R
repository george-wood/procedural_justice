##########################################################################
# File: figure_1.R
# Description: Produce Figure 1
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/4_placebo.RData"))

# figure 1 top panel, officers per cluster
annotation_layer <-
  tibble(x = c(13, 25, 51), y = 1000,
         text = c("Pilot", "Early Adopters", "Late Adopters"))

figure_1_top <-
  y %>%
  filter(trained_in_month == month) %>%
  distinct(officers, period, cluster) %>%
  group_by(period) %>%
  summarize(officers = sum(officers)) %>%
  ggplot(aes(period, officers)) +
  geom_col(show.legend = FALSE, fill = flat[3]) + 
  geom_text(data = annotation_layer, inherit.aes = FALSE,
            aes(x, y, label = text),
            size = 3, color = "grey20") +
  scale_x_continuous(NULL,
                     breaks = NULL,
                     expand = expand_scale(add = 0.5),
                     limits = c(0, 63)) +
  scale_y_continuous("Officers",
                     expand = expand_scale(add = c(0, 0)),
                     breaks = seq(0, 800, by = 400),
                     oob = function(x, ...) x,
                     limits = c(0, 800)) +
  coord_cartesian(clip = "off") +
  theme(axis.line.y  = element_line(size = 0.2),
        axis.ticks.y = element_line(size = 0.2),
        plot.margin  = margin(t = 0.8, unit = "lines"))

# figure 1 right panel, clusters per group
figure_1_right <-
  y %>%
  distinct(cluster, trained_in_month) %>%
  arrange(trained_in_month) %>%
  mutate(group = rleid(trained_in_month)) %>%
  group_by(group) %>%
  summarize(n = n_distinct(cluster)) %>%
  ggplot(aes(rev(group), n)) +
  geom_col(fill = flat[3]) +
  coord_flip() +
  scale_x_continuous(NULL,
                     breaks = NULL,
                     expand = expand_scale(add = c(0, 5.7))) +
  scale_y_continuous("Clusters",
                     expand = expand_scale(add = c(0, 0)),
                     breaks = seq(0, 20, by = 5),
                     limits = c(0, 20)) +
  theme(axis.line.x  = element_line(size = 0.2),
        axis.ticks.x = element_line(size = 0.2),
        plot.margin  = margin(t = 0.8, unit = "lines"))

# figure 1 bottom panel, staggered adoption of training
design_matrix <-
  y %>%
  arrange(trained_in_month) %>%
  transmute(month, period, cluster, trained,
            group = rleid(trained_in_month)) %>%
  distinct(group, period, trained) %>%
  group_by(group) %>%
  mutate(
    condition = case_when(all(trained == 0) ~ "Always Control",
                          trained == 0      ~ "Pre-Training",
                          trained == 1      ~ "Post-Training")
  ) %>%
  ungroup() %>%
  mutate(
    condition = fct_relevel(factor(condition), "Pre-Training", "Post-Training")
  )

highlight_training <-
  tibble(
    xmin = c(13.5, # pilot
             seq(17.5, 33.5, by = 1),  # phase-one
             seq(40.5, 41.5, by = 1),  # phase-two [1:2]
             seq(43.5, 45.5, by = 1),  # phase-two [3:5]
             seq(47.5, 49.5, by = 1),  # phase-two [6:8]
             seq(51.5, 57.5, by = 1),  # phase-two [9:15]
             seq(59.5, 62.5, by = 1)), # phase-two [16:19]
    ymin = c(43.5, # pilot
             rev(seq(26.5, 42.5, by = 1)),  # phase-one
             rev(seq(24.5, 25.5, by = 1)),  # phase-two [1:2]
             rev(seq(21.5, 23.5, by = 1)),  # phase-two [3:5]
             rev(seq(18.5, 20.5, by = 1)),  # phase-two [6:8]
             rev(seq(11.5, 17.5, by = 1)),  # phase-two [9:15]
             rev(seq(7.5,  10.5, by = 1))), # phase-two [16:19]
  ) %>%
  mutate(xmax = xmin + 1,
         ymax = ymin + 1)

highlight_phase <-
  tibble(xmin = c(13.5, 17.5, 40.5),
         xmax = c(14.5, 34.5, 63.5),
         ymin = 0.5,
         ymax = 44.5)

figure_1_bottom <-
  design_matrix %>%
  mutate(stroke = ifelse(condition == "Pre-Training", 0.4, 0.7)) %>%
  ggplot(aes(period, rev(group), color = condition, fill = condition,
             shape = condition, stroke = stroke)) +
  geom_rect(data = highlight_training, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = "transparent", fill = "orange", alpha = 0.2) +
  geom_rect(data = highlight_phase, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = "grey60", fill = "transparent",
            linetype = "solid", size = 0.3) +
  geom_point(size = 0.8) +
  scale_color_manual(NULL,
                     values = c("grey70", flat[2], "grey70")) + # P,T,C
  scale_fill_manual(NULL,
                    values = c("white", "#4D79A7", "grey70")) + # P,T,C
  scale_shape_manual(NULL,
                     values = c(1, 4, 21)) +
  scale_x_continuous("Month",
                     expand = expand_scale(add = 0.5),
                     breaks = seq(12, 60, by = 12)) +
  scale_y_continuous("Cluster Group",
                     expand = expand_scale(add = c(0, 0.5)),
                     breaks = seq(5, 40, by = 5),
                     labels = rev(seq(5, 40, by = 5))) +
  guides(shape = guide_legend(override.aes = list(size = 1.9))) +
  theme(legend.position = c(0.145, -0.066),
        legend.direction = "horizontal",
        legend.text = element_text(margin = margin(l = -1, unit = "lines")),
        plot.margin = margin(t = 0, unit = "lines"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

# compose figure and write plot
figure_1 <- 
  wrap_plots(figure_1_top, figure_1_bottom, ncol = 1, heights = c(0.1, 0.9))
figure_1 <- 
  wrap_plots(figure_1, figure_1_right, ncol = 2, widths = c(0.9, 0.1))

ggsave(figure_1, filename = here("products/figures/figure_1.pdf"),
       width = 8, height = 5, device = cairo_pdf)
