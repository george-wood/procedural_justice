# correct century for dates
correct_century <-
  function(x, year = 2020) {
    m <- year(x) %% 100
    year(x) <- ifelse(m > year %% 100, 1900 + m, 2000 + m)
    x
  }

# clean suffixes
remove_suffix <- 
  function(x) {
    suffix <- c("SR", "JR", "II", "III", "IV")
    split  <- stri_split(x, regex = "\\s|\\.|\\,|\\(|\\)")
    x <-
      sapply(split,
             function(z)
               paste(z[!(z %in% suffix)], collapse = " "),
             USE.NAMES = FALSE)
    x <- trimws(x)
    return(x)
  }

# custom group count
add_grouped_count <-
  function(x, group_by, count_on = vars(uid)) {
    x %>%
      group_by(!!!group_by) %>%
      mutate(n = n_distinct(!!!count_on)) %>%
      ungroup() 
  }

# filter new officers
remove_new_officers <-
  function(x, end_date) {
    x %>%
      # exclude officers appointed to the CPD after 2011-01-09
      filter(min(assigned) %m-% months(12) >= appointed)
  }

# pull officers by adoption period
pull_adopters <-
  function(x, start, end) {
    x %>%
      filter(between(trained_in_month, start, end)) %>%
      distinct(cluster) %>%
      pull(cluster)
  }

# generate time-series cross-sectional data for evaluation
generate_tscs <-
  function(training, complaints = NULL, sustained = NULL, force = NULL,
           end_date = ymd("2016-03-31"), cluster_by = "day") {
    
    # set constants
    start <- floor_date(min(training$assigned) %m-% months(12), "month")
    end   <- ceiling_date(max(training$assigned), "month")
    
    # remove invalid officers
    D <-
      training %>%
      remove_new_officers() %>%
      arrange(assigned) %>%
      mutate(resigned = floor_date(resigned, "month"))
    
    # select clustering
    if (cluster_by == "day") {
      D <- mutate(D, cluster = rleid(assigned))
    } else {
      D <- mutate(D, cluster = rleid(floor_date(assigned, "month")))
    }
    
    # create T*N tibble
    Y <-
      tibble(month = seq(start, end, by = "1 month")) %>%
      mutate(period = 1:n()) %>%
      expand_grid(., cluster = unique(D$cluster)) %>%
      left_join(D, by = "cluster") %>%
      group_by(month, period, cluster) %>%
      summarize(
        trained_in_month = floor_date(min(assigned), "month"),
        officers = n_distinct(uid[is.na(resigned) | resigned > month])
      ) %>%
      ungroup() %>%
      mutate(trained = ifelse(month > trained_in_month, 1, 0))
    
    # populate T*N tibble with outcomes
    
    if (!is.null(complaints)) {
      Y <-
        left_join(
          Y,
          complaints %>%
            filter(uid %in% D$uid) %>%
            mutate(month = floor_date(date, "month")) %>%
            left_join(D, by = "uid") %>%
            group_by(month, cluster) %>%
            summarize(complaints = n_distinct(cr_id, na.rm = TRUE)),
          by = c("month", "cluster")
        ) %>%
        replace_na(list(complaints = 0))
    }
    
    if (!is.null(sustained)) {
      Y <-
        left_join(
          Y,
          complaints %>%
            filter(uid %in% D$uid,
                   (!is.na(settlement) | sustained == 1)) %>%
            mutate(month = floor_date(date, "month")) %>%
            left_join(D, by = "uid") %>%
            group_by(month, cluster) %>%
            summarize(sustained = n_distinct(cr_id, na.rm = TRUE)),
          by = c("month", "cluster")
        ) %>%
        replace_na(list(sustained = 0))
    }
    
    if (!is.null(force)) {
      Y <-
        left_join(
          Y,
          force %>%
            filter(uid %in% D$uid) %>%
            mutate(month = floor_date(date, "month")) %>%
            left_join(D, by = "uid") %>%
            group_by(month, cluster) %>%
            summarize(force = n_distinct(trr_id, na.rm = TRUE)),
          by = c("month", "cluster")
        ) %>%
        replace_na(list(force = 0))
    }
    
    # return TSCS tibble
    return(Y)
  }

# interactive-fixed effect (IFE) or matrix completion (MC) estimator
estimate_model <-
  function(data = y, formula = complaints ~ trained,
           method = "ife", r = c(0, 5), nboots = 1000, seed = 29590, ...) {
    
    data <-
      if (as_string(lhs(formula)) == "sustained") {
        filter(data, coverage == "sfc")
      } else {
        data
      }
    
    fect(formula  = formula,
         data     = data,
         method   = method,
         se       = TRUE,
         force    = "two-way", # fixed effects demeaning
         cl       = "unit", 
         wald     = FALSE,
         CV       = TRUE,
         r        = r,
         parallel = TRUE,
         nboots   = nboots,
         permute  = TRUE, # permutation test
         index    = c("cluster", "period"),
         seed     = seed)
  }

# round up to nearest x
round_up <-
  function(x, unit) {
    ceiling(x / unit)
  }

# placebo test
placebo_test <- function(data = y, formula, period = c(-3, 0), method = "ife",
                         tuning, nboots = 1000, seed = 29590) {
  
  data <-
    if (as_string(lhs(formula)) == "sustained") {
      filter(data, coverage == "sfc")
    } else {
      data
    }
  
  if (method == "ife") {
    fect(formula        = formula,
         data           = data,
         method         = method,
         se             = TRUE, force = "two-way", cl = "unit", 
         wald           = FALSE, CV = FALSE,
         r              = tuning,
         lambda         = NULL,
         parallel       = TRUE, nboots = nboots, placeboTest = TRUE,
         placebo.period = period, permute = TRUE,
         index          = c("cluster", "period"),
         seed           = seed)
  } else {
    fect(formula        = formula,
         data           = data,
         method         = method,
         se             = TRUE, force = "two-way", cl = "unit", 
         wald           = FALSE, CV = FALSE,
         r              = 0,
         lambda         = tuning,
         parallel       = TRUE, nboots = nboots, placeboTest = TRUE,
         placebo.period = period, permute = TRUE,
         index          = c("cluster", "period"),
         seed           = seed)
  }
  
}

# tidy data for ATT figures
tidy_att <-
  function(cf, fit, outcome) {
    
    alpha <- 0.05
    
    att <-
      cf %>%
      ungroup() %>%
      transmute(x, name, value) %>%
      pivot_wider(names_from = name, values_from = value) %>%
      mutate(att = Observed - `Counterfactual Estimate`)

    officer_period_count <-
      y %>%
      mutate(x = interval(trained_in_month, month) %/% months(1)) %>%
      group_by(x) %>%
      summarize(officers = sum(officers),
                clusters = n_distinct(cluster))
    
    ci <-
      fit$att.boot %>%
      as_tibble() %>%
      gather() %>%
      mutate(iter = parse_number(key)) %>%
      group_by(iter) %>%
      mutate(x = fit$time) %>%
      left_join(officer_period_count, by = "x") %>%
      mutate(value = (value * clusters) / officers * 100) %>%
      group_by(x) %>%
      summarize(lower = quantile(value, alpha / 2, na.rm = TRUE),
                upper = quantile(value, 1 - (alpha / 2), na.rm = TRUE)) 
    
    left_join(att, ci, by = "x") %>%
      mutate(outcome = str_to_title(!!outcome)) %>%
      filter(between(x, -36, 24)) %>%
      group_by(outcome) %>%
      mutate(fill_att = (att - mean(att)) / sd(att)) %>%
      ungroup()
  
  }

# tidy data for counterfactual and observed figures
tidy_counterfactual <-
  function(x, outcome, n_officers = 100) {

    training_period <-
      tibble(training_period = apply(x$D.dat, 2,
                                     function(x) min(which(x == 1)))) %>%
      mutate(cluster = 1:n())
    
    observed <-
      x$Y.dat %>%
      as_tibble() %>%
      gather(key = cluster,
             value = observed) %>%
      mutate(cluster = parse_number(cluster)) %>%
      left_join(training_period, by = "cluster") %>%
      group_by(cluster) %>%
      mutate(period = 1:n() + 1,
             x = period - training_period)
    
    counterfactual <-
      x$Y.ct %>%
      as_tibble() %>%
      gather(key = cluster,
             value = counterfactual) %>%
      mutate(cluster = parse_number(cluster)) %>%
      left_join(training_period, by = "cluster") %>%
      group_by(cluster) %>%
      mutate(period = 1:n() + 1,
             x = period - training_period)
    
    res <-
      left_join(observed, counterfactual,
                by = c("cluster", "training_period", "period", "x")) %>%
      filter(between(x, -48, 36)) %>%
      group_by(x, cluster) %>%
      summarize(observed = mean(observed),
                counterfactual = mean(counterfactual)) %>%
      pivot_longer(cols = c(observed, counterfactual)) %>%
      mutate(name = ifelse(grepl("obs", name),
                           "Observed", "Counterfactual Estimate"))
    
    officer_period_count <-
      y %>%
      mutate(x = interval(trained_in_month, month) %/% months(1)) %>%
      group_by(x, cluster) %>%
      summarize(`Observed` = sum(officers)) %>%
      group_by(cluster) %>%
      mutate(`Counterfactual Estimate` = max(`Observed`)) %>%
      pivot_longer(cols = c(`Observed`, `Counterfactual Estimate`),
                   values_to = "officers")
    
    left_join(res, officer_period_count,
              by = c("x", "cluster", "name")) %>%
      filter(between(x, -36, 24)) %>%
      group_by(x, name) %>%
      summarize(value    = sum(value),
                officers = sum(officers)) %>%
      mutate(value = value / officers * 100,
             outcome = str_to_title(!!outcome))
     
  }

# cumulative att
source(here("src/aux/cumulative.R"))
environment(att.cumu) <- asNamespace("fect")

cumulative_att <-
  function(x, period = c(0, 24), units = n) {
    x %>%
      att.cumu(period = period, alpha = 0.05, type = "on") %>%
      as_tibble() %>%
      clean_names() %>%
      filter(!is.na(start)) %>%
      left_join(units, by = "end") %>%
      mutate(catt     = rescale_att(data = ., catt),
             ci_lower = rescale_att(data = ., ci_lower),
             ci_upper = rescale_att(data = ., ci_upper),
             s_e      = rescale_att(data = ., s_e))
  }

# rescale att
rescale_att <-
  function(data, x, n_officers = 100) {
    ((x * data$clusters) / data$officers) * n_officers
  }

# compute accumulation of trained officers
accumulation <- 
  function(x, period = 12) {
    x %>%
      transmute(uid, assigned, pre = assigned %m-% months(period)) %>%
      arrange(assigned) %>%
      group_by(assigned, pre) %>%
      nest(trained = c(uid)) %>%
      ungroup() %>%
      mutate(trained = map(trained, as_tibble),
             cumulative_trained = accumulate(trained, bind_rows),
             control = map(cumulative_trained,
                           ~ assignment %>%
                             transmute(uid) %>%
                             filter(!uid %in% .x)))
  }

# calculate mean outcomes at time of assignment
calculate_pre_mean <- 
  function(x, outcome) {
    x %>%
      mutate(
        count  = pmap(list(pre, assigned),
                      ~ filter(outcome, date >= .x, date < .y)),
        y_t    = pmap_dbl(list(count, trained),
                          ~ n_distinct(.x$eid[.x$uid %in% .y$uid], na.rm = TRUE)),
        y_c    = pmap_dbl(list(count, control),
                          ~ n_distinct(.x$eid[.x$uid %in% .y$uid], na.rm = TRUE)),
        mean_t = y_t / map_dbl(trained, nrow),
        mean_c = y_c / map_dbl(control, nrow)
      )
  }

# visualize staggered adoption
sad_figure <- function(x, n = FALSE, option = "A") {
  
  x <-
    x %>%
    mutate_at(vars(f_description), fct_drop)
  
  labels <-
    x %>%
    distinct(f_description, type, officers) %>%
    mutate(
      officers = ifelse(
        levels(f_description) ==
          levels(f_description)[length(levels(f_description))],
        paste("Officers =", officers), officers
      )
    )
  
  if (n == TRUE) {
    p <-
      ggplot(x, aes(week, f_description,
                    fill = n, color = n)) +
      theme(legend.position = "bottom")
  } else {
    p <-
      ggplot(x, aes(week, f_description,
                    fill = proportion, color = proportion)) +
      theme(legend.position = "top")
  }
  
  p <-
    p +
    geom_tile(size = 0.5) +
    scale_y_discrete(NULL, drop = TRUE) +
    coord_cartesian(clip = "off") +
    scale_x_date(NULL, labels = NULL) +
    scale_fill_viridis_c(NULL, na.value = "transparent",
                         option = option, direction = -1) +
    scale_color_viridis_c(NULL, na.value = "transparent",
                          option = option, direction = -1) +
    theme(axis.text.y       = element_text(hjust = 0),
          legend.key.width  = unit(4, "lines"),
          legend.key.height = unit(0.8, "lines"),
          legend.title      = element_text(vjust = 0.85, size = 9),
          panel.grid        = element_blank(),
          plot.margin       = margin(r = 0, b = 1, unit = "lines"),
          panel.background  = element_rect(fill = "transparent", color = NA))
  
  if (n == TRUE) {
    p + 
      guides(color = guide_colorbar(title = "Officers Trained"),
             fill  = guide_colorbar(title = "Officers Trained")) +
      scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y")
  } else {
    p +
      guides(color = guide_colorbar(title = "Proportion Trained"),
             fill  = guide_colorbar(title = "Proportion Trained")) +
      geom_label(
        data = labels, inherit.aes = FALSE,
        aes(y = f_description, x = max(sad$week), label = officers),
        nudge_x = -3, size = 3, hjust = 1, vjust = 0.5,
        fill = "transparent", color = "white", 
        label.padding = unit(0, "lines"), label.size = unit(0, "lines")
      )
  }
}

# colors
flat <-
  c("#9b59b6", "#3498db", "#95a5a6", "#e74c3c", "#34495e", "#2ecc71")

# plot theme
theme_set(
  theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank())
)


