library(tidyverse)
library(lavaan)
library(lavaanPlot)

# Using lavaan 0.6-19

# load data

inclusions <- read_rds(here("analysis", "vrrsb_wide_included.rds"))$id

# Demographics ====

demo_parents <- read_rds(here("analysis", "demo_parents.rds"))

demo_parents_wide <- demo_parents %>%
  mutate(
    # Mean replace 3 missing values
    p_edy = replace_na(p_edy, round(mean(p_edy, na.rm = TRUE)))
  ) %>%
  na.omit() %>%
  rename(
    p_tswc = p_meboth_tswc
  ) %>%
  pivot_wider(
    id_cols = c(id),
    names_from = p_pcg,
    values_from = c(p_gender, p_edy, p_tswc)
  ) %>%
  mutate(
    p_man_pcg = p_gender_pcg == "man",
    p_man_scg = p_gender_scg == "man"
  ) %>%
  select(id, starts_with("p_man"), starts_with("p_ed"), starts_with("p_tswc"))

demo_parents_new <- demo_parents %>%
  select(id, p1p2, p_pcg)

demo_child <- read_rds("analysis/demo_child.rds") %>%
  filter(
    id %in% inclusions
  ) %>%
  mutate(
    c_first_born = child_birth_order == "first_born",
    c_male = child_sex == "Male"
  ) %>%
  ungroup() %>%
  select(id, c_first_born, c_male)

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
    p1p2 = str_extract(name, "iprParent[12]"),
    cdi_total = cdi_total
  ) %>%
  left_join(demo_parents_new, join_by(id, p1p2)) %>%
  select(id, age, p_pcg, cdi_total) %>%
  filter(
    !is.na(p_pcg)
  ) %>%
  pivot_wider(
    names_from = p_pcg,
    values_from = cdi_total,
    names_prefix = "cdi_total_"
  )

## vrRSB ====

# vrrsb <- read_rds("analysis/vrrsb_wide_included.rds") %>%
#   select(id, starts_with("VRS")) %>%
#   rename(
#     vrrsb_VRS_p1 = VRS_p1,
#     vrrsb_VRS_p2 = VRS_p2
#   ) %>%
#   filter(
#     id %in% inclusions
#   ) %>%
#   pivot_longer(
#     starts_with("vrrsb_VRS"),
#   ) %>%
#   mutate(
#     p1p2 = str_extract(name, "iprParent[12]")
#   ) %>%
#   left_join(demo_parents_new, join_by(id, p1p2)) %>%
#   select(id, new_parent, ) %>%
#   pivot_wider(
#     names_from = new_parent,
#     names_prefix = "vrrsb_VRS_"
#   )

## BAPQ ====

bapq <- read_rds("analysis/bapq_wide_all.rds") %>%
  select(id, starts_with("total")) %>%
  rowwise() %>%
  mutate(
    p_bapq_totalBE_p1 = mean(c(total_self_p1, total_partner_p2), na.rm = TRUE),
    p_bapq_totalBE_p2 = mean(c(total_self_p2, total_partner_p1), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(id, contains("totalBE")) %>%
  pivot_longer(-id) %>%
  mutate(
    p1p2 = if_else(str_detect(name, "p1"), "iprParent1", "iprParent2")
  ) %>%
  left_join(demo_parents_new, by = join_by(id, p1p2)) %>%
  na.omit() %>%
  pivot_wider(id_cols = id, names_from = p_pcg, values_from = value,
              names_prefix = "p_bapq_totalBE_")

all_data <- demo_child %>%
  distinct() %>%
  left_join(demo_parents_wide, join_by(id)) %>%
  left_join(
    select(bapq, id, contains("totalBE"))
  ) %>%
  left_join(cdi, join_by(id)) %>%
  ungroup()

# Run the SEM

# TO DO: Rename parent vars to have p_ prefix

model <-
  '
    cdi_total_pcg ~~ cdi_total_scg

    cdi_total_pcg ~ age + c_male + c_first_born +
      p_man_pcg + p_edy_pcg + p_bapq_totalBE_pcg + p_tswc_pcg

    cdi_total_scg ~ age + c_male + c_first_born +
      p_man_scg + p_edy_scg + p_bapq_totalBE_scg + p_tswc_scg

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_totalBE_pcg ~~ p_bapq_totalBE_scg
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~ p_bapq_totalBE_pcg
    p_edy_scg ~ p_bapq_totalBE_scg

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~ p_bapq_totalBE_pcg
    p_man_scg ~ p_bapq_totalBE_scg

  '

sem_results <- sem(model, data = all_data)

lavaanPlot(sem_results, coefs = TRUE, covs = TRUE)

# SEM 2 ====

all_data2 <- all_data %>%
  pivot_longer(-c(id, age, starts_with("c_"))) %>%
  mutate(
    pcg = str_extract(name, "[ps]cg$"),
    name = str_remove(name, "_[ps]cg$")
  ) %>%
  pivot_wider() %>%
  group_by(id) %>%
  mutate(
    cdi_total_scg = rev(cdi_total)
  ) %>%
  ungroup()

model2 <- '

  cdi_total ~~ cdi_total_scg

  cdi_total ~ age + c_male + c_first_born +
    p_man + p_edy + p_bapq_totalBE + p_tswc +
    c_male:p_man

  cdi_total_scg ~ age + c_male + c_first_born +
    p_man + p_edy + p_bapq_totalBE + p_tswc +
    c_male:p_man

  p_man ~~ p_edy
  p_man ~~ p_bapq_totalBE
  p_man ~~ p_tswc

  # Set some covariances to 0
  c_male ~~ 0*c_first_born
  c_male ~~ 0*age
  age ~~ 0*c_first_born

'

sem_results2 <- sem(model2, data = all_data2)
lavaanPlot(sem_results2, coefs = TRUE, covs = TRUE)

