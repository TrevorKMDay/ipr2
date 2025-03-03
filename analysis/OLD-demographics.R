library(tidyverse)
library(here)

setwd("C:/Users/Trevor/Desktop/ipr/get_data_qualtrics/")

participants <- read_rds("participants.rds")

demo0 <- read_rds(here("data", "demo0.rds"))

demo <- demo0 %>%
  filter(
    studyID %in% participants
  ) %>%
  select(-(StartDate:UserLanguage), EndDate) %>%
  select_all(~str_replace_all(., "[# ]", "_"))

demo_baby <- demo %>%
  select(studyID, EndDate, p1_or_p2, screen_time:child_birth_length)

# Child sex ====

demo_baby %>%
  filter(
    p1_or_p2 == "Parent 1"
  ) %>%
  pull(child_sex) %>%
  table(useNA = "a")

child_term <- demo_baby %>%
  filter(
    p1_or_p2 == "Parent 1"
  ) %>%
  select(studyID, EndDate, starts_with("child_dob")) %>%
  mutate(
    across(everything(), as.character)
  ) %>%
  pivot_longer(-c(studyID, EndDate)) %>%
  mutate(
    name = name %>%
      str_replace("_1_", "_month_") %>%
      str_replace("_2_", "_day_") %>%
      str_replace("_3_", "_year_") %>%
      str_remove("child_") %>%
      if_else(str_detect(., "_2"), str_replace(., "dob", "due"), .) %>%
      str_remove("_[12]$")
  ) %>%
  separate_wider_delim(name, delim = "_", names = c("dates", "component")) %>%
  pivot_wider(names_from = component) %>%
  rowwise() %>%
  mutate(
    date = as_date(paste(c(year, month, day), collapse = "-"))
  ) %>%
  pivot_wider(id_cols = c(studyID, EndDate), names_from = dates, values_from = date) %>%
  mutate(
    age = as.numeric(interval(dob, EndDate) / years(1)),
    days_late = interval(due, dob) / days(1),
    category = case_when(
      days_late >   21 ~ NA_character_,
      days_late >=  14 ~ "postterm",
      days_late >=   7 ~ "lateterm",
      days_late >=  -7 ~ "term",
      days_late >= -21 ~ "earlyterm",
      days_late <  -21 ~ "preterm"
    )
  ) %>%
  select(-dob, -due)

ggplot(child_term, aes(days_late, fill = category)) +
  geom_histogram(binwidth = 3) +
  labs(x = "Weeks early") +
  theme_bw()

table(child_term$category)

# Race/Ethnicity ====

demo_RE <- demo %>%
  filter(
    p1_or_p2 == "Parent 1"
  ) %>%
  select(studyID, starts_with("race")) %>%
  pivot_longer(-studyID) %>%
  mutate(
    name = str_replace(name, "race_ethnicity_1_", "hispanic_") %>%
      str_replace("race_ethnicity_2_", "race_") %>%
      str_replace("(race|hispanic)_1", "child") %>%
      str_replace("(race|hispanic)_2", "mom") %>%
      str_replace("(race|hispanic)_3", "dad")
  ) %>%
  separate_wider_delim(name, delim = "_", names = c("who", "X"),
                       too_many = "merge", too_few = "align_start") %>%
  mutate(
    X = case_match(
        X,
        "1" ~ "white",
        "2" ~ "black",
        "3" ~ "asian",
        "4" ~ "unk",
        "5" ~ "AIAN",
        "6" ~ "NHO",
        "7" ~ "PI",
        "8" ~ "other",
        NA ~ "hispanic"
      ),
    value = case_when(
      value == "Non-Hispanic" | is.na(value) ~ FALSE,
      value == "Hispanic" | !is.na(value) ~ TRUE
    )
  )

child_RE <- demo_RE %>%
  filter(
    who == "child"
  ) %>%
  mutate(
    value = if_else(value, X, NA)
  ) %>%
  pivot_wider(names_from = X) %>%
  rowwise() %>%
  mutate(

    unk = if_else(is.na(white) & is.na(black) &
                    is.na(asian) & is.na(unk) & is.na(AIAN) & is.na(NHO) &
                    is.na(PI) & is.na(other),
                  "unk", NA),

    across(everything(), ~replace_na(.x, "")),
    hispanic = if_else(hispanic == "", "non-hispanic", hispanic),

    race_ethnicity = paste(c(white, black, asian, unk, AIAN, NHO, PI, other),
                           collapse = " ") %>%
      trimws() %>%
      if_else(str_detect(., " "), "mixed", .)

  )

table(child_RE$hispanic, child_RE$race_ethnicity)

# Household ====

household <- demo %>%
  filter(
    p1_or_p2 == "Parent 1"
  ) %>%
  select(studyID, EndDate, household_income, starts_with("parents"),
         starts_with("siblings"))

household_income <- household%>%
  group_by(household_income) %>%
  summarize(
    n = n()
  ) %>%
  arrange(desc(household_income)) %>%
  mutate(
    sum = cumsum(n),
    pct = sum / 302,
    rev_pct = 1 - pct
  )

ggplot(household, aes(household_income)) +
  geom_histogram(stat = "count") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Count")

siblings <- household %>%
  select(-household_income) %>%
  mutate(
    across(everything(), as.character)
  ) %>%
  pivot_longer(-c(studyID, EndDate)) %>%
  separate_wider_delim(name, delim = "_", names = c(NA, "name", "sib_n")) %>%
  mutate(
    name = case_match(name, "1" ~ "sib_sex", "2" ~ "sib_rel",
                      "3" ~ "sib_at_home", "4" ~ "sib_dob_mo",
                      "5" ~ "sib_dob_y",  "6" ~ "sib_dob_d")
  ) %>%
  na.omit() %>%
  pivot_wider() %>%
  rowwise() %>%
  mutate(
    sib_dob_mo = case_match(
      sib_dob_mo,
      "Feburary" ~ "February",
      .default = sib_dob_mo
    ),
    sib_dob = paste0(c(sib_dob_y, sib_dob_mo, sib_dob_d), collapse = "-") %>%
      as_date(),
    sib_age = interval(sib_dob, EndDate) / years(1)
  )



birth_order <- child_term %>%
  select(studyID, age) %>%
  left_join(
    select(siblings, studyID, sib_n,starts_with("sib_dob"),  sib_age)
  ) %>%
  filter(
    !is.na(sib_n)
  ) %>%
  mutate(
    older = if_else(sib_age > age, "older",
                    if_else(sib_age == age, "twin",
                            "younger"))
  ) %>%
  group_by(studyID) %>%
  nest() %>%
  mutate(
    n_siblings = map_int(data, ~sum(!is.na(.x$sib_n))),
    n_older = map_int(data, ~sum(.x$older == "older", na.rm = TRUE)),
    n_younger = map_int(data, ~sum(.x$older == "older", na.rm = TRUE)),
    n_twin = map_int(data, ~sum(.x$older == "twin", na.rm = TRUE)),
    n_missing = map_int(data, ~sum(is.na(.x$older)))
  )

birth_order_missing <- birth_order %>%
  filter(
    n_siblings > 0,
    n_missing > 0,
    n_older == 0
  ) %>%
  unnest(data)

missing_info <- siblings %>%
  filter(
    is.na(sib_dob)
  )

clipr::write_clip(missing_info)
