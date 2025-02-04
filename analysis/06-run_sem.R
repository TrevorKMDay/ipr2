library(tidyverse)
library(lavaan)

# Using lavaan 0.6-19

# load data

inclusions <- read_rds(here("analysis", "vrrsb_wide_included.rds"))$id

# Demographics ====

demo_parents <- read_rds(here("analysis", "demo_parents.rds")) %>%
  mutate(
    # Mean replace 3 missing values
    ed_y = replace_na(ed_y, round(mean(ed_y, na.rm = TRUE)))
  )

demo_parents2 <- demo_parents %>%
  select(id, new_parent, gender2, ed_y) %>%
  pivot_wider(
    names_from = new_parent,
    values_from = c(gender2, ed_y)
  )

demo_parents_new <- demo_parents %>%
  select(id, p1p2, new_parent)

demo_child <- read_rds("analysis/demo_child.rds") %>%
  filter(
    id %in% inclusions
  ) %>%
  rename(
    c_sex = sex,
    c_birth_order = child_birth_order
  ) %>%
  ungroup()

# This re-assigns the p1/p2 numbers

## CDI ====

cdi <- read_rds("analysis/cdi_wide_included.rds") %>%
  select(id, age, starts_with("words_produced_number")) %>%
  select_all(~str_replace(., "words_produced_number", "cdi_total")) %>%
  filter(
    id %in% inclusions
  ) %>%
  pivot_longer(
    starts_with("cdi_total"),
    values_to = "cdi_total"
  ) %>%
  mutate(
    p1p2 = str_extract(name, "iprParent[12]")
  ) %>%
  left_join(demo_parents_new, join_by(id, p1p2)) %>%
  select(id, age, new_parent, cdi_total) %>%
  pivot_wider(
    names_from = new_parent,
    values_from = cdi_total,
    names_prefix = "cdi_total_"
  )

## BAPQ ====

bapq <- read_rds("analysis/bapq_wide_all.rds") %>%
  select(id, starts_with("total")) %>%
  rowwise() %>%
  mutate(
    p_bapq_totalBE_p1 = mean(c(total_self_p1, total_partner_p2), na.rm = TRUE),
    p_bapq_totalBE_p2 = mean(c(total_self_p2, total_partner_p1), na.rm = TRUE)
  ) %>%
  ungroup()

all_data <- demo_child %>%
  left_join(demo_parents2, join_by(id)) %>%
  left_join(
    select(bapq, id, contains("totalBE"))
  ) %>%
  left_join(cdi, join_by(id)) %>%
  ungroup()

# Run the SEM

# TO DO: Rename parent vars to have p_ prefix

model <-
  '
    cdi_total_p1 ~~ cdi_total_p2

    cdi_total_p1 ~ age + c_sex + c_birth_order +
      gender2_p1 + ed_y_p1 + p_bapq_totalBE_p1

    cdi_total_p2 ~ age + c_sex + c_birth_order +
      gender2_p2 + ed_y_p2 + p_bapq_totalBE_p2

    # Covariances between parents
    ed_y_p1 ~~ ed_y_p2
    p_bapq_totalBE_p1 ~~ p_bapq_totalBE_p2
    gender2_p1 ~~ gender2_p2

    # Let educ/asd covary, est r=.17
    ed_y_p1 ~ p_bapq_totalBE_p1
    ed_y_p2 ~ p_bapq_totalBE_p2

    # Let parent gender/asd covary, d=.37
    gender2_p1 ~ p_bapq_totalBE_p1
    gender2_p2 ~ p_bapq_totalBE_p2

  '

sem_results <- sem(model, data = all_data)

lavaanPlot(sem_results, coefs = TRUE, covs = TRUE)

