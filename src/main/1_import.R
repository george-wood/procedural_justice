##########################################################################
# File: 1_import.R
# Description: Import officer, complaints, settlements, use of force,
#              and training data.
#              Truncate complaints and use of force data due to
#              discontinuity in data at 2016-03.
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))

# read and tidy officer data
officers <-
  here("data/officers/officers.csv") %>%
  read_csv() %>%
  transmute(
    star = current_star, uid = UID, last_name = remove_suffix(last_name),
    first_name, mi = middle_initial, appointed = appointed_date,
    resigned = resignation_date, birth_year
  ) %>%
  filter(!is.na(appointed)) %>%
  group_by(uid, last_name, first_name, birth_year, appointed, resigned) %>%
  slice(1) %>%
  ungroup()

# read complaints data
accused <-
  bind_rows(
    here("data/complaints/accused_2000_2016.csv") %>%
      read_csv() %>%
      transmute(uid = UID, cr_id, finding = final_finding),
    here("data/complaints/accused_2000_2018.csv") %>%
      read_csv() %>%
      transmute(uid = UID, cr_id, finding = final_finding)
  ) %>%
  group_by(uid, cr_id) %>%
  mutate(finding = ifelse(any(finding %in% c("SUSTAINED", "SU")),
                          "sustained", "other_outcome")) %>%
  distinct() %>%
  ungroup()

complaints <-
  bind_rows(
    here("data/complaints/complaints_2000_2016.csv") %>%
      read_csv() %>%
      transmute(cr_id, date = incident_date),
    here("data/complaints/complaints_2000_2018.csv") %>%
      read_csv() %>%
      transmute(cr_id, date = incident_start_date)
  ) %>%
  distinct()

# read settlements data
settlements <-
  here("data/settlements/settlements_1952_2016.csv") %>%
  read_csv() %>%
  transmute(uid = UID, date = incident_date, complaint,
            settlement = parse_number(settlement))

# join complaints and settlement data 
complaints <-
  accused %>%
  left_join(complaints, by = "cr_id") %>%
  left_join(settlements, by = c("uid", "date")) %>%
  mutate(sustained = ifelse(finding == "sustained" | !is.na(settlement), 1, 0))

# read use of force data
type <-
  here("data/force/trr_actions_responses_2004_2016.csv") %>%
  read_csv() %>%
  filter(person == "Member Action") %>%
  group_by(trr_id) %>%
  filter(action_sub_category == max(action_sub_category)) %>%
  ungroup() %>%
  mutate(a = str_to_lower(member_action)) %>%
  transmute(
    trr_id,
    category = case_when(
      a %in% c("member presence",
               "verbal commands") ~
        "force_mitigation",
      a %in% c("escort holds",
               "control instrument",
               "wristlock",
               "armbar",
               "pressure sensitive areas") ~
        "control_tactics",
      a %in% c("open hand strike",
               "take down/emergency handcuffing",
               "elbow strike",
               "kicks",
               "closed hand strike/punch",
               "knee strike") ~
        "action_without_weapons",
      a %in% c("firearm",
               "canine") |
        grepl("impact|taser|chemical", a) ~
        "action_with_weapons",
      TRUE ~ "other"
    )
  )

trr <-
  here("data/force/trr_main_2004_2016.csv") %>%
  read_csv() %>%
  transmute(trr_id, date = trr_date)

user <-
  here("data/force/trr_officers_2004_2016.csv") %>%
  read_csv() %>%
  transmute(uid = UID, trr_id)

# join use of force data
force <-
  user %>%
  left_join(trr,  by = "trr_id") %>%
  left_join(type, by = "trr_id") %>%
  distinct()

# read training data and filter to first training assignment for each officer
training <-
  here("data/training/training.csv") %>%
  read_csv() %>%
  group_by(scrambled_id, last_name, first_name) %>%
  filter(assigned == min(assigned)) %>%
  ungroup()

# discontinuity in complaints data at 2016-03
discontinuity <- ymd("2016-03-01")

complaints %>%
  filter(date >= "2011-01-01") %>%
  group_by(month = floor_date(date, "month")) %>%
  summarize(n = n_distinct(cr_id)) %>%
  ggplot(aes(month, n)) +
  geom_vline(xintercept = discontinuity, linetype = "dashed") +
  geom_point()

force %>%
  filter(date >= "2011-01-01") %>%
  group_by(month = floor_date(date, "month")) %>%
  summarize(n = n_distinct(trr_id)) %>%
  ggplot(aes(month, n)) +
  geom_vline(xintercept = discontinuity, linetype = "dashed") +
  geom_point()

# truncate complaints and use of force data at discontinuity
# note that 96% of officers have been trained by this time
# remaining 4% will be always-control
complaints <-
  filter(
    complaints,
    between(date,
            min(training$assigned) %>% floor_date("month") %m-% months(12),
            discontinuity)
  )

force <-
  filter(
    force,
    between(date,
            min(training$assigned) %>% floor_date("month") %m-% months(12),
            discontinuity)
  )

# settlement data ends at 2015-11
settlement_terminus <-
  max(complaints$date[!is.na(complaints$settlement)], na.rm = TRUE) %>%
  floor_date("month") %m-% days(1)

# remove unncessary objects and save:
rm(list = setdiff(ls(), c("officers", "training", "complaints", "force",
                          "discontinuity", "settlement_terminus")))
save.image(here("products/rdata/1_import.RData"))



