library(tidyverse)

participants <- read_rds("participants.rds")

demo0 <- read_rds(here("data", "demo0.rds"))

demo <- demo0 %>%
  filter(
    studyID %in% participants
  ) %>%
  select(-(StartDate:UserLanguage)) %>%
  select_all(~str_replace_all(., "[# ]", "_"))

demo_baby <- demo %>%
  select(studyID, p1_or_p2, screen_time:child_birth_length)

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
  pivot_wider(names_from = X)

ethnicity <- child_RE %>%
  select(-studyID, -who) %>%
  colSums()

