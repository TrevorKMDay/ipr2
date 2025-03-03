library(tidyverse)
library(here)
library(lavaan)
library(lavaanPlot)

# Using lavaan 0.6-19

# load data

inclusions <- read_rds(here("analysis", "vrrsb_wide_included.rds"))$id

# Demographics ====

demo_parents <- read_rds(here("analysis", "demo_parents.rds"))

demo_parents[!complete.cases(demo_parents), ]

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

demo_child <- read_rds(here("analysis", "demo_child.rds")) %>%
  filter(
    id %in% inclusions
  ) %>%
  mutate(
    c_first_born = child_birth_order == "first_born",
    c_male = child_sex == "Male"
  ) %>%
  ungroup() %>%
  select(id, c_first_born, c_male)

demo_child[!complete.cases(demo_child), ]

# This re-assigns the p1/p2 numbers

## CDI ====

cdi <- read_rds(here("analysis", "cdi_wide_included.rds")) %>%
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
    # Fix me
    !is.na(p_pcg)
  ) %>%
  pivot_wider(
    names_from = p_pcg,
    values_from = cdi_total,
    names_prefix = "cdi_total_"
  )

## vrRSB ====

vrrsb_vrs <- read_rds(here("analysis", "vrrsb_wide_included.rds")) %>%
  select(id, starts_with("VRS")) %>%
  filter(
    id %in% inclusions
  ) %>%
  pivot_longer(-id) %>%
  mutate(
    p1p2 = str_extract(name, "iprParent[12]")
  ) %>%
  left_join(demo_parents_new, join_by(id, p1p2)) %>%
  filter(
    # Fix me
    !is.na(p_pcg)
  ) %>%
  pivot_wider(
    id_cols = id,
    names_from = p_pcg,
    names_prefix = "vrrsb_VRS_"
  )

## BAPQ ====

bapq <- read_rds(here("analysis", "bapq_wide_all.rds")) %>%
  select(id, starts_with("total_"), starts_with("pl_")) %>%
  rowwise() %>%
  mutate(

    # Calculate best estimates for total and pragmatic language
    #   mean or fall through

    p_bapq_totalBE_iprParent1 = mean(c(total_self_iprParent1,
                                       total_partner_iprParent2),
                                      na.rm = TRUE),
    p_bapq_totalBE_iprParent2 = mean(c(total_self_iprParent2,
                                       total_partner_iprParent1),
                                      na.rm = TRUE),

    p_bapq_plBE_iprParent1 = mean(c(pl_self_iprParent1,
                                       pl_partner_iprParent2),
                                     na.rm = TRUE),
    p_bapq_plBE_iprParent2 = mean(c(pl_self_iprParent2,
                                       pl_partner_iprParent1),
                                     na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(id, contains("BE")) %>%
  pivot_longer(-id) %>%
  mutate(
    p1p2 = str_remove(name, "p_bapq_.*BE_"),
    name = str_extract(name, "[^_]*BE")
  ) %>%
  left_join(demo_parents_new, by = join_by(id, p1p2)) %>%
  na.omit() %>%
  pivot_wider(id_cols = id, names_from = c(name, p_pcg), values_from = value,
              names_prefix = "p_bapq_")

all_data <- demo_child %>%
  distinct() %>%
  left_join(demo_parents_wide, join_by(id)) %>%
  left_join(
    select(bapq, id, contains("BE")),
    join_by(id)
  ) %>%
  left_join(cdi, join_by(id)) %>%
  left_join(vrrsb_vrs, join_by(id)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    vrrsb_vrs_mean = mean(c(vrrsb_VRS_pcg, vrrsb_VRS_scg))
  )

all_data[!complete.cases(all_data), ]
