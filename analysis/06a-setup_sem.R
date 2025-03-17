library(tidyverse)
library(here)
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

# Run the SEM

# TO DO: Rename parent vars to have p_ prefix

vrs_model1 <-
  '
    vrrsb_VRS_pcg ~~ vrrsb_VRS_scg

    vrrsb_VRS_pcg ~ age + c_male + c_first_born +
      p_man_pcg + p_edy_pcg + p_bapq_totalBE_pcg + p_tswc_pcg

    vrrsb_VRS_scg ~ age + c_male + c_first_born +
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

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

  '

vrs_results1 <- sem(vrs_model1, data = all_data)
lavaanPlot(vrs_results1, coefs = TRUE, covs = TRUE, sig = .05)

lavaanPlot(vrs_results1, coefs = TRUE,sig = .05)

parameterEstimates(vrs_results1)

cdi_model1 <-
  '
    cdi_total_pcg ~~ cdi_total_scg

    cdi_total_pcg ~ age + vrrsb_VRS_pcg + c_male + c_first_born +
      p_man_pcg + p_edy_pcg + p_bapq_totalBE_pcg + p_tswc_pcg

    cdi_total_scg ~ age + vrrsb_VRS_scg + c_male + c_first_born +
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

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

  '

cdi_results1 <- sem(cdi_model1, data = all_data)
lavaanPlot(cdi_results1, coefs = TRUE, covs = TRUE, sig = .05)

parameterEstimates(cdi_results1)

# SEM 2 ====

all_data2 <- all_data %>%
  pivot_longer(-c(id, age, starts_with("c_"), vrrsb_vrs_mean)) %>%
  mutate(
    pcg = str_extract(name, "[ps]cg$"),
    name = str_remove(name, "_[ps]cg$")
  ) %>%
  pivot_wider() %>%
  group_by(id) %>%
  mutate(
    cdi_total_scg = rev(cdi_total),
    vrrsb_VRS_scg = rev(vrrsb_VRS)
  ) %>%
  ungroup()

ggplot(all_data2, aes(x = age, y = cdi_total, color = c_male)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(limits = c(NA, 29)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  facet_wrap(vars(pcg)) +
  theme_bw() +
  labs(x = "Age (mo)", y = "CDI total", color = "Child sex")

ggplot(all_data2, aes(x = age, y = vrrsb_VRS, color = c_male)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(limits = c(NA, 29)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  facet_wrap(vars(pcg)) +
  theme_bw() +
  labs(x = "Age (mo)", y = "vrRSB video-ref. score", color = "Child sex")

vrs_model2 <- '

  vrrsb_VRS ~~ vrrsb_VRS_scg

  vrrsb_VRS ~ age + c_male + c_first_born +
    p_man + p_edy + p_bapq_totalBE + p_tswc +
    c_male:p_man

  vrrsb_VRS_scg ~ age + c_male + c_first_born +
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

vrs_model2 <- sem(vrs_model2, data = all_data2)
lavaanPlot(vrs_model2, coefs = TRUE, covs = TRUE, sig = .05, digits = 1)
vrs_model2_pe <- parameterEstimates(vrs_model2)

lmerTest::lmer(vrrsb_VRS ~ age*pcg + c_first_born*p_tswc + c_male*p_man +
                 p_tswc + p_edy*p_tswc + p_bapq_totalBE*p_tswc + (1|id),
               data = all_data2) %>%
  summary()

lmerTest::lmer(cdi_total ~ vrrsb_vrs_mean + age + c_first_born*p_tswc +
                 c_male*p_man +
                 p_tswc + p_edy*p_tswc + p_bapq_totalBE*p_tswc +  p_bapq_plBE +
                 (1|id),
               data = all_data2) %>%
  summary()

ggplot(all_data2, aes(x = p_tswc, y = pcg, fill = pcg)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(alpha = 0.5, width = 0, height = 0.1) +
  theme_bw()

ggplot(all_data2, aes(x = p_bapq_plBE, y = cdi_total,
                      color = p_bapq_plBE > 2.75)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(all_data2, aes(x = p_bapq_plBE, y = cdi_total,
                      color = p_bapq_plBE > 2.75)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# model2 <- '
#
#   cdi_total ~~ cdi_total_scg
#
#   cdi_total ~ age + c_male + c_first_born +
#     p_man + p_edy + p_bapq_totalBE + p_tswc +
#     c_male:p_man
#
#   cdi_total_scg ~ age + c_male + c_first_born +
#     p_man + p_edy + p_bapq_totalBE + p_tswc +
#     c_male:p_man
#
#   p_man ~~ p_edy
#   p_man ~~ p_bapq_totalBE
#   p_man ~~ p_tswc
#
#   # Set some covariances to 0
#   c_male ~~ 0*c_first_born
#   c_male ~~ 0*age
#   age ~~ 0*c_first_born
#
# '
#
# sem_results2 <- sem(model2, data = all_data2)
# lavaanPlot(sem_results2, coefs = TRUE, covs = TRUE, sig = .05, digits = 1)
#
# parameterEstimates(sem_results2)

model3 <- '

  level: 1
  vrrsb_VRS ~ p_man + p_edy + p_bapq_totalBE + p_tswc


  level: 2
  vrrsb_VRS ~ age + c_male + c_first_born

  # Set some covariances to 0
  c_male ~~ 0*c_first_born
  c_male ~~ 0*age
  age ~~ 0*c_first_born

'

all_data3 <- all_data2 %>%
  group_by(id) %>%
  mutate(
    id2 = cur_group_id(),
    pcg = if_else(pcg == "pcg", 1, 0)
  )

cdi_model3 <- sem(model = model3, data = all_data3, cluster = "id2")

lavaanPlot(cdi_model3, coef = TRUE) #, sig = .05)
pe <- parameterEstimates(cdi_model3)

pe %>%
  filter(
    pvalue < .05
  )

modindices(cdi_model3, sort. = TRUE)
