library(tidyverse)
library(here)
library(lavaan)
library(lavaanPlot)

# Using lavaan 0.6-19

# load data

inclusions <- read_rds(here("analysis", "vrrsb_wide_included.rds"))$id

# Demographics ====

demo_parents <- read_rds(here("analysis", "demo_parents.rds"))

cgss <- read_rds(here("analysis", "cgss.rds"))$summary %>%
  pivot_longer(-id, names_to = c("p1p2", "scale"), names_sep = "_") %>%
  pivot_wider(names_from = scale, names_prefix = "p_cgss_")

demo_parents_wide <- demo_parents %>%
  left_join(cgss, by = join_by(id, p1p2)) %>%
  mutate(
    # Mean replace 3 missing values
    p_edy = replace_na(p_edy, round(mean(p_edy, na.rm = TRUE)))
  ) %>%
  rename(
    p_tswc = p_meboth_tswc
  ) %>%
  pivot_wider(
    id_cols = c(id),
    names_from = p_pcg,
    values_from = c(p_gender, p_edy, p_tswc, starts_with("p_cgss_"))
  ) %>%
  mutate(
    p_man_pcg = p_gender_pcg == "man",
    p_man_scg = p_gender_scg == "man"
  ) %>%
  select(id, starts_with("p_man_"), starts_with("p_edy_"),
         starts_with("p_tswc"), starts_with("p_cgss_")) %>%
  select_all(~str_replace(., "cgss_", "cgss"))

# More missing cols from cgss
demo_parents_wide[!complete.cases(demo_parents_wide), ]

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

# This re-assigns the p1/p2 numbers

## CDI ====

cdi <- read_rds(here("analysis", "cdi_wide_included.rds")) %>%
  select_all(~str_replace(., "words_produced_number", "cdi_total")) %>%
  select(id, age, starts_with("cdi_total"))

## vrRSB ====

vrrsb <- read_rds(here("analysis", "vrrsb_wide_included.rds")) %>%
  select(id, age, outlier, starts_with("total"), starts_with("nwords")) %>%
  rename(
    vrrsb_outlier = outlier,
    vrrsb_total_pcg = total_pcg,
    vrrsb_total_scg = total_scg
  )

## BAPQ ====

bapq <- read_rds(here("analysis", "bapq_wide_all.rds")) %>%
  pivot_longer(-id) %>%
  separate_wider_delim(name, delim = "_",
                       names = c("score", "reporter", "p1p2")) %>%
  left_join(demo_parents_new, by = join_by(id, p1p2)) %>%
  filter(
    id %in% inclusions
  )

bapq2 <- bapq %>%
  select(-p1p2) %>%
  pivot_wider(names_from = c(score, p_pcg, reporter), values_from = value) %>%
  rowwise() %>%
  mutate(

    p_bapq_total_pcg_be = mean(c(total_pcg_self, total_scg_partner), na.rm = TRUE),
    p_bapq_aloof_pcg_be = mean(c(aloof_pcg_self, aloof_scg_partner), na.rm = TRUE),
    p_bapq_rigid_pcg_be = mean(c(rigid_pcg_self, rigid_scg_partner), na.rm = TRUE),
    p_bapq_pl_pcg_be = mean(c(pl_pcg_self, pl_scg_partner), na.rm = TRUE),

    p_bapq_total_scg_be = mean(c(total_scg_self, total_pcg_partner), na.rm = TRUE),
    p_bapq_aloof_scg_be = mean(c(aloof_scg_self, aloof_pcg_partner), na.rm = TRUE),
    p_bapq_rigid_scg_be = mean(c(rigid_scg_self, rigid_pcg_partner), na.rm = TRUE),
    p_bapq_pl_scg_be = mean(c(pl_scg_self, pl_pcg_partner), na.rm = TRUE),

  ) %>%
  ungroup()

bapq_be <- bapq2 %>%
  select(id, ends_with("be"))

bapq_be[!complete.cases(bapq_be), ]

all_data <- demo_child %>%
  distinct() %>%
  left_join(demo_parents_wide, join_by(id)) %>%
  left_join(bapq_be, join_by(id)) %>%
  left_join(cdi, join_by(id)) %>%
  left_join(
    select(vrrsb, -age),
    join_by(id)
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    vrrsb_mean = mean(c(vrrsb_total_pcg, vrrsb_total_scg))
  ) %>%
  ungroup()

# Just missing first-born status
all_data[!complete.cases(all_data), ]

write_rds(all_data, "analysis/sem_data.rds")

cor(all_data$nwords_pcg, all_data$cdi_total_pcg)
cor(all_data$nwords_scg, all_data$cdi_total_scg)

nwords <- all_data %>%
  select(id, age, starts_with("nwords"), starts_with("cdi_total")) %>%
  pivot_longer(-c(id, age)) %>%
  mutate(
    pcg = str_extract(name, "[ps]cg"),
    name = str_remove(name, "_[ps]cg"),
    value2 = replace(value, value > 680, 680)
  ) %>%
  pivot_wider(values_from = c(value, value2)) %>%
  mutate(
    diff1 = value_nwords - value_cdi_total,
    diff2 = value2_nwords - value2_cdi_total
  )

ggplot(nwords, aes(x = value_nwords, y = value_cdi_total)) +
  geom_point(aes(fill = age), color = "white", shape = 21, size = 2) +
  geom_smooth(aes(color = pcg), se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_fill_viridis(limits = c(23, 27), oob = scales::squish) +
  coord_cartesian(ylim = c(0, 680)) +
  theme_bw()

ggplot(nwords, aes(x = value2_nwords, y = value2_cdi_total)) +
  geom_point(aes(fill = age), color = "white", shape = 21, size = 2) +
  geom_smooth(aes(color = pcg), se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_fill_viridis(limits = c(23, 27), oob = scales::squish) +
  coord_cartesian(ylim = c(0, 680)) +
  theme_bw()

ggplot(nwords, aes(diff1)) +
  geom_histogram()

ggplot(nwords, aes(diff2)) +
  geom_histogram()

cor(nwords$value2_cdi_total, nwords$value2_nwords)

# Differences

all_diffs <- all_data %>%
  select(id, age, starts_with("c"), contains("tswc"), starts_with("cdi_total"),
         starts_with("vrrsb_total_")) %>%
  mutate(
    tswc_diff = p_tswc_pcg - p_tswc_scg ,
    cdi_diff = cdi_total_pcg - cdi_total_scg,
    vrrsb_diff = vrrsb_total_pcg - vrrsb_total_scg
  )

all_diffs_long <- all_diffs %>%
  select(id, age,ends_with("diff")) %>%
  pivot_longer(c(cdi_diff, vrrsb_diff))

ggplot(all_diffs_long, aes(x = tswc_diff, y = abs(value))) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  facet_wrap(vars(name), scales = "free") +
  theme_bw()

lm(cdi_diff ~ tswc_diff + age + c_male + c_first_born, data = all_diffs) %>%
  summary()

lm(vrrsb_diff ~ tswc_diff + age + c_male, data = all_diffs) %>%
  summary()

lm(abs(cdi_diff) ~ abs(tswc_diff) + age + c_male + c_first_born, data = all_diffs) %>%
  summary()

lm(abs(vrrsb_diff) ~ abs(tswc_diff) + age + c_male, data = all_diffs) %>%
  summary()

ggplot(all_diffs_long, aes(x = age, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(name), scales = "free") +
  theme_bw()
