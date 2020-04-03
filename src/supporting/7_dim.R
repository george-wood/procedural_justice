##########################################################################
# File: 7_dim.R
# Description: Plot Figure S5
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/4_placebo.RData"))

# simple difference in means
dim <-
  y %>%
  group_by(trained = ifelse(month > trained_in_month, "post", "pre"),
           cluster) %>%
  summarize(months     = n_distinct(month),
            officers   = mean(officers),
            complaints = sum(complaints),
            sustained  = sum(sustained),
            force      = sum(force)) %>%
  group_by(trained) %>%
  transmute(cluster,
            Complaints = complaints / months / officers,
            Sustained  = sustained  / months / officers,
            Force      = force      / months / officers) %>%
  pivot_longer(cols = c(Complaints, Sustained, Force)) %>%
  pivot_wider(names_from = c(trained), values_from = value) %>%
  clean_names() %>%
  mutate(
    adoption = case_when(cluster %in% early_adopters ~ "Early",
                         cluster %in% late_adopters ~ "Late",
                         TRUE ~ "Pilot"),
    name = fct_relevel(name, "Complaints", "Sustained")
  ) %>%
  filter(adoption != "Pilot")

# mean outcomes per cluster-month before and after training
group_by(dim, name) %>% summarize_at(vars(pre, post), mean)

# Figure S5 omits 11 points to improve readability
limits <-
  dim %>%
  group_by(name) %>%
  filter(pre <= (mean(pre) * 3) & post <= (mean(pre) * 3)) %>%
  summarize(post  = max(c(post, pre), na.rm = TRUE),
            pre = max(c(post, pre), na.rm = TRUE)) %>%
  mutate(adoption = NA)

figure_s5 <-
  dim %>%
  group_by(name) %>%
  filter(pre <= (mean(pre) * 3) & post <= (mean(pre) * 3)) %>%
  ggplot(aes(pre, post, color = adoption)) +
  geom_abline(intercept = 0, slope = 1, color = "grey10", linetype = "dashed") +
  geom_point(alpha = 0.4) +
  geom_blank(data = limits) +
  scale_x_continuous("Pre-Training Mean per Officer per Month",
                     breaks = pretty_breaks()) +
  scale_y_continuous("Post-Training Mean per Officer per Month",
                     breaks = pretty_breaks()) +
  scale_color_manual("Cluster Adoption",
                     values = c(flat[6], flat[1], flat[4])) +
  facet_wrap(~ name, scales = "free") +
  theme(axis.line        = element_line(size = 0.3),
        axis.text        = element_text(color = "black"),
        axis.ticks       = element_line(size = 0.3), 
        strip.text       = element_text(size = 9),
        legend.direction = "horizontal",
        legend.position  = "top")

# write
figure_s5

ggsave(figure_s5, filename = here("products/figures/figure_s5.pdf"),
       width = 10, height = 4, device = cairo_pdf)

