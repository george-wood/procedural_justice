##########################################################################
# File: figure_3.R
# Description: Produce Figure 3
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/4_placebo.RData"))

# functions
rescale_att <-
  function(data, x, n_officers = 100) {
    ((x * data$clusters) / data$officers) * n_officers
  }

tidy_placebo <-
  function(fit, outcome = "complaints") {
    
  g <- 
    plot(fit) %>%
    ggplot_build()
  
  g_att <-
    g %>%
    pluck(1) %>%
    pluck(3) %>%
    filter(between(x, -36, 24)) %>%
    mutate(
      outcome = !!outcome,
      period  = min(fit$placebo.period),
      type    = ifelse(between(x,
                               fit$placebo.period[1],
                               fit$placebo.period[2]),
                       "placebo", "not")
    ) %>%
    left_join(n, by = c("x" = "end")) %>%
    mutate(y = rescale_att(data = ., y))
  
  g_placebo <- 
    g %>%
    pluck(1) %>%
    pluck(6) %>%
    filter(between(x, -36, 24)) %>%
    mutate(
      outcome = !!outcome,
      period  = min(fit$placebo.period)
    ) %>%
    left_join(n, by = c("x" = "end")) %>%
    mutate(y = rescale_att(data = ., y))
  
  g_att_ci <-
    g %>%
    pluck(1) %>%
    pluck(7) %>%
    filter(between(x, -36, 24)) %>%
    mutate(
      outcome = !!outcome,
      period  = min(fit$placebo.period)
    ) %>%
    left_join(n, by = c("x" = "end")) %>%
    mutate(ymin = rescale_att(data = ., ymin),
           ymax = rescale_att(data = ., ymax))
  
  g_placebo_ci <-
    g %>%
    pluck(1) %>%
    pluck(8) %>%
    filter(between(x, -36, 24)) %>%
    mutate(
      outcome = !!outcome,
      period  = min(fit$placebo.period)
    ) %>%
    left_join(n, by = c("x" = "end")) %>%
    mutate(ymin = rescale_att(data = ., ymin),
           ymax = rescale_att(data = ., ymax))
  
  g_att$y[is.na(g_att$y)] <- g_placebo$y[is.na(g_att$y)]
  return(list(g_att, g_att_ci, g_placebo_ci))
}

generate_op <-
  function(x) {
    x %>%
      mutate(
        op = fct_relevel(factor(paste0(str_to_title(outcome),
                                       " [", period, ", 0]")),
                         "Complaints [-3, 0]",
                         "Sustained [-3, 0]",
                         "Force [-3, 0]",
                         "Complaints [-5, 0]",
                         "Sustained [-5, 0]")
      )
  }

# officers per period
n <-
  mutate(y, end = interval(trained_in_month, month) %/% months(1)) %>%
  group_by(end) %>%
  summarize(officers = sum(officers),
            clusters = n_distinct(cluster))

# figure 4 data
figure_3_data <-
  models %>%
  transmute(outcome, placebo3, placebo5) %>%
  pivot_longer(cols = c(placebo3, placebo5),
               names_to = "period", values_to = "placebo") %>%
  mutate(period = 0 - parse_number(period),
         data = pmap(list(placebo, outcome, period),
                     function(a, b, c)
                       tidy_placebo(a, b)),
         placebo_att  = map(data, pluck, 1),
         placebo_ci   = map(data, pluck, 2),
         highlight_ci = map(data, pluck, 3))

figure_3_text <-
  figure_3_data %>%
  transmute(outcome, period,
            p = map_dbl(placebo, ~ .$est.placebo[1, "p.value"])) %>%
  generate_op()

figure_3 <-
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, alpha = 0.5) +
  geom_ribbon(
    data = generate_op(bind_rows(figure_3_data$placebo_ci)),
    aes(x = x, ymin = ymin, ymax = ymax),
    alpha = 0.2
  ) +
  geom_ribbon(
    data = generate_op(bind_rows(figure_3_data$highlight_ci)),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = flat[2], alpha = 0.2
  ) +
  geom_line(
    data = generate_op(bind_rows(figure_3_data$placebo_att)),
    aes(x, y), size = 0.4, color = flat[3]
  ) +
  geom_point(
    data = generate_op(bind_rows(figure_3_data$placebo_att)) %>%
      group_by(op) %>%
      mutate(fill_y = (y - mean(y)) / sd(y)),
    aes(x, y, fill = fill_y), shape = 21, stroke = 0.4
  ) +
  geom_text(
    data = figure_3_text,
    aes(x = -Inf, -Inf, hjust = -0.25, vjust = -1,
        label = sprintf(round(p, 3), fmt = "P = %#.3f")),
    size = 3
  ) +
  scale_x_continuous("Month Relative to Training",
                     breaks = seq(-36, 24, by = 12)) +
  scale_y_continuous("ATT on Frequency per 100 Officers",
                     limits = function(x) c(-max(abs(x)), max(abs(x))),
                     expand = expand_scale(mult = 0.05),
                     breaks = pretty_breaks()) +
  scale_fill_continuous_diverging(palette = "Purple-Brown",
                                  rev = FALSE, mid = 0.63) +
  facet_wrap(~ op, strip.position = "top", scales = "free_y") +
  theme_minimal(base_size = 9) +
  theme(panel.grid      = element_blank(),
        axis.line       = element_line(size = 0.3),
        axis.text       = element_text(color = "black"),
        axis.ticks      = element_line(size = 0.3), 
        strip.text      = element_text(size = 9),
        legend.margin   = margin(t = -1, unit = "lines"),
        legend.position = "none")

# write
figure_3

ggsave(figure_3, filename = here("products/figures/figure_3.pdf"),
       width = 10, height = 5, device = cairo_pdf)
