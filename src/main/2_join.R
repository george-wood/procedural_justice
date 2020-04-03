##########################################################################
# File: 2_join.R
# Description: Join training data to outcome data (complaints,
#              use of force) using first name, last name, and middle
#              initial.
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/1_import.RData"))

# match training and officer data
training_match <-
  training %>%
  left_join(officers, by = c("last_name", "first_name")) %>%
  filter(!is.na(appointed)) %>%
  filter((assigned < resigned | is.na(resigned)) & assigned > appointed)

# extract unique name matches
resolved_via_name <-
  training_match %>%
  add_count(last_name, first_name) %>%
  filter(n == 1)

# identify duplicates
duplicated_after_name <-
  training_match %>%
  anti_join(resolved_via_name,
            by = c("uid", "last_name", "first_name"))

# use positive matches on middle names to adjudicate
resolved_via_positive_mi <-
  duplicated_after_name %>%
  filter(mi.x == mi.y) %>%
  add_count(uid) %>%
  filter(n == 1)

# identify remaining duplicates
duplicated_after_positive_mi <-
  duplicated_after_name %>%
  anti_join(resolved_via_positive_mi,
            by = c("uid", "last_name", "first_name"))

# use negative matches on middle names to adjudicate
resolved_via_negative_mi <-
  duplicated_after_positive_mi %>%
  filter(!is.na(mi.x) & !is.na(mi.y) & mi.x != mi.y) %>%
  add_count(uid) %>%
  filter(n == 1)

# identify remaining duplicates
duplicated_after_negative_mi <-
  duplicated_after_positive_mi %>%
  anti_join(resolved_via_negative_mi,
            by = c("uid", "last_name", "first_name"))

# use NA middle name matches to adjudicate
resolved_via_na_mi <-
  duplicated_after_negative_mi %>%
  filter(is.na(mi.x) & is.na(mi.y)) %>%
  add_count(uid) %>%
  filter(n == 1)

# remaining duplicates
duplicated_after_na_mi <-
  duplicated_after_negative_mi %>%
  anti_join(resolved_via_na_mi,
            by = c("uid", "last_name", "first_name"))

# resolve via exclusion
resolved_via_exclusion <-
  duplicated_after_na_mi %>%
  group_by(last_name, first_name) %>%
  mutate(n = n_distinct(scrambled_id),
         m = n_distinct(uid)) %>%
  filter(n == 1, m == 1)

# finish matching
complete_match <-
  bind_rows(resolved_via_name) %>%
  bind_rows(resolved_via_positive_mi) %>%
  bind_rows(resolved_via_negative_mi) %>%
  bind_rows(resolved_via_na_mi) %>%
  bind_rows(resolved_via_exclusion)

# loss due to matching
n_training <- length(unique(training$scrambled_id))
n_complete <- nrow(complete_match)
n_no_match <- nrow(training %>%
                     left_join(officers,
                               by = c("last_name", "first_name")) %>%
                     filter(is.na(appointed)))
n_dropped  <- n_training - n_complete
p_dropped  <- round(n_dropped / n_training * 100, 2)
p_no_match <- round(n_no_match / n_training * 100, 2)
p_exact    <- round((n_dropped - n_no_match) / n_training * 100, 2)

# summary
glue(
"The raw training data contains {n_training} officers.
{n_complete} could be matched to exactly one unique identifier
in the roster data.

Percentage dropped: {p_dropped}%

Reason for dropping {n_dropped} officers:
* {n_no_match} ({p_no_match}%)
did not have a name match in the roster data;
* {n_dropped - n_no_match} ({p_exact}%)
could not be matched to exactly officer in the roster data."
)

# save
training <-
  complete_match %>%
  transmute(assigned, uid, appointed, resigned, birth_year)

rm(list = setdiff(ls(), c("officers", "training", "complaints", "force",
                          "discontinuity", "settlement_terminus")))
save.image(here("products/rdata/2_join.RData"))


