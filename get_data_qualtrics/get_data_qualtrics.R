library(tidyverse)
library(here)

# Set up Qualtrics access ====
source(here("get_data_qualtrics", "00-qualtrics_setup.R"))

# Get data ====

dir.create(here("data"))

## CDI ====

cdi <- fetch_survey(id$cdi)
saveRDS(cdi, here("data", "cdi0.rds"))

cdi_finished <- cdi %>%
  labelled::remove_labels() %>%
  select(studyID, p1_or_p2, Finished, Progress) %>%
  filter(
    !is.na(studyID),
    !str_detect(studyID, "BAD|A[12]|T2|VERIFY|^9999"),
  ) %>%
  mutate(
    p1_or_p2 = str_remove(p1_or_p2, " ")
  ) %>%
  pivot_wider(id_cols = studyID, names_from = p1_or_p2, values_from = Progress,
              values_fill = 0) %>%
  mutate(
    inst = "CDI",
    both = as.character(Parent1 == 100 & Parent2 == 100),
    across(starts_with("Parent"), as.character)
  )

## vrRSB ====

vrrsb <- fetch_survey(id$vrRSB)
saveRDS(cdi, here("data", "vrrsb0.rds"))

vrrsb_finished <- vrrsb %>%
  labelled::remove_labels() %>%
  select(studyID, p1_or_p2, Finished, Progress, Q7_7) %>%
  filter(
    !is.na(studyID),
    !str_detect(studyID, "BAD|A[12]|T2|VERIFY|^9999"),
    studyID != "9999"
  ) %>%
  mutate(
    p1_or_p2 = str_remove(p1_or_p2, " "),
    Finished = if_else(Q7_7 == "Sometimes True", as.character(Finished), "Fail")
  ) %>%
  pivot_wider(id_cols = studyID, names_from = p1_or_p2, values_from = Progress,
              values_fill = 0) %>%
  mutate(
    inst = "vrRSB",
    both = as.character(Parent1 == 100 & Parent2 == 100),
    across(starts_with("Parent"), as.character)
  )

## BAPQ ====

bapq <- fetch_survey(id$bapq)
saveRDS(cdi, here("data", "bapq0.rds"))

bapq_finished <- bapq %>%
  labelled::remove_labels() %>%
  select(studyID, p1_or_p2, Finished, Progress) %>%
  filter(
    !is.na(studyID),
    !str_detect(studyID, "BAD|A[12]|T2|VERIFY|^9999"),
    studyID != "9999"
  ) %>%
  mutate(
    p1_or_p2 = str_remove(p1_or_p2, " "),
  ) %>%
  pivot_wider(id_cols = studyID, names_from = p1_or_p2, values_from = Progress,
              values_fill = 0) %>%
  mutate(
    inst = "BAPQ",
    both = as.character(Parent1 == 100 & Parent2 == 100),
    across(starts_with("Parent"), as.character)
  )

## Demographics ====

demo_p1 <- fetch_survey(id$demo_p1)
demo_p2 <- fetch_survey(id$demo_p2)

demo_p1_2 <- demo_p1 %>%
  mutate(
    p1_or_p2 = if_else(str_detect(study_id, "^1"), "Parent 1", p1_or_p2)
  ) %>%
  rename(
    studyID = study_id
  )

demo_p2_2 <- demo_p2 %>%
  mutate(
    studyID = as.character(study_id),
    p1_or_p2 = "Parent 2"
  )

demo_all <- bind_rows(demo_p1_2, demo_p2_2)
saveRDS(demo_all, here("data", "demo0.rds"))

demo2_finished <- demo_all %>%
  labelled::remove_labels() %>%
  select(studyID, p1_or_p2, Finished, Progress) %>%
  arrange(studyID, p1_or_p2) %>%
  filter(
    !is.na(studyID),
    !str_detect(studyID, "BAD|A[12]|T2|VERIFY|^9999"),
  ) %>%
  mutate(
    p1_or_p2 = str_remove(p1_or_p2, " "),
  ) %>%
  pivot_wider(id_cols = studyID, names_from = p1_or_p2,
              values_from = Progress, values_fill = 0) %>%
  mutate(
    inst = "demo",
    both = as.character(Parent1 & Parent2),
    across(starts_with("Parent"), as.character)
  )

## Combined ====

instruments <- bind_rows(cdi_finished, bapq_finished, vrrsb_finished,
                         demo2_finished)

inst_parents <- instruments %>%
  pivot_longer(starts_with("Parent")) %>%
  pivot_wider(id_cols = studyID, names_from = c(inst, name), values_fill = "0") %>%
  mutate(
    across(everything(), ~replace_na(.x, "FALSE")),
  ) %>%
  filter(
    vrRSB_Parent1 != "Fail",
    vrRSB_Parent2 != "Fail"
  )

inst_completed8 <- inst_parents %>%
  pivot_longer(-studyID) %>%
  group_by(studyID) %>%
  nest() %>%
  mutate(
    n_passed = map_int(data, ~sum(.x$value == "100"))
  )

table(inst_completed8$n_passed)

one_missing <- inst_completed8$studyID[inst_completed8$n_passed %in% c(6, 7)]

one_missing_inst <- inst_parents %>%
  filter(
    studyID %in% one_missing
  )

complete_participants <- inst_completed8$studyID[inst_completed8$n_passed == 8]
write_rds(complete_participants, "participants.rds")

# Five missing BAPQ, 4 partial
# Two missing vrRSB (P2), 1 partial
# One both parents missing demographics

