##########################################################################
# File: 6_adoption.R
# Description: Plot Figure S1, Figure S2, Figure S3, Figure S4
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
source(here("src/aux/units.R"))
load(here("products/rdata/4_placebo.RData"))

# tidy assignment data by week and with CPD units
assignment <-
  training %>%
  remove_new_officers() %>%
  left_join(
    here("data/officers/officers.csv") %>%
      read_csv() %>%
      transmute(uid = UID,
                unit = str_pad(as.character(current_unit), 3, pad = "0")),
    by = "uid"
  ) %>%
  left_join(units, by = "unit") %>%
  transmute(uid, assigned, assigned_week = floor_date(assigned, "week"),
            unit, type, description)

# staggered adoption by unit-week
sad <-
  expand_grid(week = seq(min(assignment$assigned_week),
                         max(assignment$assigned_week),
                         by = "1 week"),
              unit = unique(assignment$unit)) %>%
  filter(week <= discontinuity) %>%
  left_join(assignment, by = "unit") %>%
  group_by(week, unit, type, description) %>%
  summarize(officers   = n_distinct(uid),
            trained    = n_distinct(uid[assigned_week <= week]),
            proportion = trained / officers) %>%
  group_by(unit, type, description) %>%
  mutate(n = trained - lag(trained),
         n = ifelse(n == 0, NA, n)) %>%
  ungroup() %>%
  arrange(parse_number(description)) %>%
  mutate(f_description = factor(description, levels = unique(description)))

# create sad figures
figure_s1 <- 
  wrap_plots(
    sad_figure(filter(sad, type == "district")),
    sad_figure(filter(sad, type == "district"),
               n = TRUE, option = "E"),
    ncol = 1
  )

figure_s2 <- 
  wrap_plots(
    sad_figure(filter(sad, officers >= 50, type == "special")),
    sad_figure(filter(sad, officers >= 50, type == "special"),
               n = TRUE, option = "E"),
    ncol = 1
  )

# cluster size figure
figure_s3 <-
  filter(y, month == trained_in_month) %>%
  ggplot(aes(officers)) +
  geom_histogram(binwidth = 1, color = "white") + 
  scale_x_continuous("Officers", expand = expand_scale(add = c(1, 1))) +
  scale_y_continuous("Count of Clusters", expand = expand_scale(add = c(0, 1))) +
  theme(panel.grid      = element_blank(),
        axis.line       = element_line(size = 0.3),
        axis.text       = element_text(color = "black"),
        axis.ticks      = element_line(size = 0.3), 
        strip.text      = element_text(size = 9),
        legend.margin   = margin(t = -1, unit = "lines"),
        legend.position = "none")

# plot mean outcome in 12 months prior to assignment
figure_s4 <- 
  bind_rows(
    assignment %>%
      filter(assigned <= discontinuity) %>%
      accumulation(period = 12) %>%
      calculate_pre_mean(complaints %>% rename(eid = cr_id)) %>%
      transmute(assigned, mean_t, mean_c, outcome = "Complaints") %>%
      pivot_longer(cols = c(mean_t, mean_c),
                   names_to = "group", values_to = "mean"),
    assignment %>%
      filter(assigned <= discontinuity) %>%
      accumulation(period = 12) %>%
      calculate_pre_mean(force %>% rename(eid = trr_id)) %>%
      transmute(assigned, mean_t, mean_c, outcome = "Force") %>%
      pivot_longer(cols = c(mean_t, mean_c),
                   names_to = "group", values_to = "mean")
  ) %>%
  arrange(assigned) %>%
  mutate(cluster = rleid(assigned)) %>%
  ggplot(aes(rev(assigned), mean, fill = group, shape = group)) +
  geom_point(size = 1.6, color = "grey10", alpha = 0.6) +
  scale_fill_manual(NULL, values = c(flat[1], flat[6]),
                     labels = c("Officers not yet assigned",
                                "Officers in assigned cluster")) +
  scale_shape_manual(NULL, values = c(21, 24),
                     labels = c("Officers not yet assigned",
                                "Officers in assigned cluster")) +
  scale_y_continuous("Mean Count in Previous Year",
                     breaks = pretty_breaks()) +
  scale_x_date("Date Assigned to Training",
               labels = function(x) format(rev(x), "%Y")) +
  coord_flip() +
  facet_wrap(~ outcome, ncol = 2, scales = "free_x") +
  theme(axis.line.x     = element_line(size = 0.3),
        axis.text       = element_text(color = "black"),
        axis.ticks.x    = element_line(size = 0.3),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey90"),
        panel.spacing.x = unit(4, "lines"),
        legend.position = "top",
        legend.margin   = margin(b = 0, unit = "lines"))

# write figures
ggsave(figure_s1, filename = here("products/figures/figure_s1.pdf"),
       width = 10, height = 9.2, device = cairo_pdf)
ggsave(figure_s2, filename = here("products/figures/figure_s2.pdf"),
       width = 10, height = 6.2, device = cairo_pdf)
ggsave(figure_s3, filename = here("products/figures/figure_s3.pdf"),
       width = 8, height = 4, device = cairo_pdf)
ggsave(figure_s4, filename = here("products/figures/figure_s4.pdf"),
       width = 8, height = 5.2, device = cairo_pdf)

# reported statistics
assignment %>%
  summarize(n_district = sum(type == "district", na.rm = TRUE),
            percentage_district = sum(type == "district", na.rm = TRUE) / n())


