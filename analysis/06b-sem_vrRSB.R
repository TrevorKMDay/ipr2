library(tidyverse)
library(lavaan)
library(lavaanPlot)
library(semTools)
library(here)

all_data <- read_rds(here("analysis", "sem_data.rds"))

# Initial model ====

vrrsbt_modelspec1 <- '

    vrrsb_total_pcg ~~ vrrsb_total_scg

    vrrsb_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_total_pcg_be + p4a*p_tswc_pcg

    vrrsb_total_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_total_scg_be + p4b*p_tswc_scg

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_total_pcg_be ~~ p_bapq_total_scg_be
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~~ p_bapq_total_pcg_be
    p_edy_scg ~~ p_bapq_total_scg_be

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~~ p_bapq_total_pcg_be
    p_man_scg ~~ p_bapq_total_scg_be

    # Test if there is a difference between paths
    # https://www.regorz-statistik.de/blog/lavaan_path_comparison.html

    c1 := c1a - c1b
    c2 := c2a - c2b
    c3 := c3a - c3b

    p1 := p1a - p1b
    p2 := p2a - p2b
    p3 := p3a - p3b
    p4 := p4a - p4b

  '

vrrsbt_model1 <- sem(vrrsbt_modelspec1, data = all_data, missing = "ML")
lavaanPlot(vrrsbt_model1, coefs = TRUE, covs = TRUE, sig = .05)

summary(vrrsbt_model1)

modificationIndices(vrrsbt_model1, sort. = TRUE) %>%
  mutate(
    mi = round(mi, 1)
  ) %>%
  filter(
    # Only matched paths
    (str_detect(lhs, "pcg") & str_detect(rhs, "pcg")) |
      (str_detect(lhs, "scg") & str_detect(rhs, "scg")),
    # mi > 10vrrsbt_model3
  )

# Model 2 ====

vrrsbt_modelspec2 <- '

    vrrsb_total_pcg ~~ vrrsb_total_scg

    vrrsb_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_total_pcg_be +
      p4a*p_tswc_pcg

    vrrsb_total_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_total_scg_be +
      p4b*p_tswc_scg

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_total_pcg_be ~~ p_bapq_total_scg_be
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    # NEW PATHS IN MODEL 2
    p_tswc_pcg ~ p_man_pcg
    p_tswc_scg ~ p_man_scg
    p_edy_pcg ~ p_man_pcg
    p_edy_scg ~ p_man_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~~ p_bapq_total_pcg_be
    p_edy_scg ~~ p_bapq_total_scg_be

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~~ p_bapq_total_pcg_be
    p_man_scg ~~ p_bapq_total_scg_be

    # Test if there is a difference between paths

    c1 := c1a - c1b
    c2 := c2a - c2b
    c3 := c3a - c3b

    p1 := p1a - p1b
    p2 := p2a - p2b
    p3 := p3a - p3b
    p4 := p4a - p4b

  '

vrrsbt_model2 <- sem(vrrsbt_modelspec2, data = all_data, missing = "ML")
lavaanPlot(vrrsbt_model2, coefs = TRUE, covs = TRUE, sig = .05)
vrs_model_comparison <- compareFit(vrrsbt_model1, vrrsbt_model2)

modificationIndices(vrrsbt_model2, sort. = TRUE) %>%
  mutate(
    # mi = round(mi, 1)
  )  %>%
  filter(
    # Only matched paths
    (str_detect(lhs, "pcg") & str_detect(rhs, "pcg")) |
      (str_detect(lhs, "scg") & str_detect(rhs, "scg")),
    # mi > 1
  )

vrrsbt_modelspec3 <- '

    vrrsb_total_pcg ~~ vrrsb_total_scg

    vrrsb_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_total_pcg_be + p4a*p_tswc_pcg

    vrrsb_total_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_total_scg_be + p4b*p_tswc_scg

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_total_pcg_be ~~ p_bapq_total_scg_be
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    p_tswc_pcg ~ p_man_pcg + p_edy_pcg
    p_tswc_scg ~ p_man_scg + p_edy_scg
    p_edy_pcg ~ p_man_pcg + p_bapq_total_pcg_be
    p_edy_scg ~ p_man_scg + p_bapq_total_scg_be

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~~ p_bapq_total_pcg_be
    p_edy_scg ~~ p_bapq_total_scg_be

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~~ p_bapq_total_pcg_be
    p_man_scg ~~ p_bapq_total_scg_be

    # Test if there is a difference between paths
    # https://www.regorz-statistik.de/blog/lavaan_path_comparison.html

    c1 := c1a - c1b
    c2 := c2a - c2b
    c3 := c3a - c3b

    p1 := p1a - p1b
    p2 := p2a - p2b
    p3 := p3a - p3b
    p4 := p4a - p4b

  '

vrrsbt_model3 <- sem(vrrsbt_modelspec3, data = all_data, missing = "ML")
lavaanPlot(vrrsbt_model3, coefs = TRUE, covs = TRUE, sig = .05)
vrs_model_comparison <- compareFit(vrrsbt_model1, vrrsbt_model2, vrrsbt_model3)

modificationIndices(vrrsbt_model3, sort. = TRUE)%>%
  mutate(
    mi = round(mi, 2)
  )  %>%
  filter(
    # Only matched paths
    (str_detect(lhs, "pcg") & str_detect(rhs, "pcg")) |
      (str_detect(lhs, "scg") & str_detect(rhs, "scg")),
    # mi > 1
  )

pe <- parameterestimates(vrrsbt_model3) %>%
  filter(
    str_detect(lhs, "vrrsb_total_"),
    !str_detect(rhs, "vrrsb_total"),
    op != "~1"
  ) %>%
  mutate(
    parent = case_when(
      str_detect(lhs, "_pcg$") ~ "pcg",
      str_detect(lhs, "_scg$") ~ "scg",
      TRUE ~ "child"
    ),
    rhs = str_remove(rhs, "_[ps]cg"),

    across(c(est, se, z, ci.lower, ci.upper), ~round(.x, 2)),
    pvalue = round(pvalue, 4)

  )

ggplot(pe, aes(x = rhs)) +
  geom_pointrange(aes(y = est, ymin = ci.lower, ymax = ci.upper,
                      fill = parent, linetype = parent, shape = parent),
                  size = 1,
                  position = position_dodge2(width = 0.2)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  coord_flip() +
  scale_x_discrete(
    labels = c("Child Age (mo)", "Child First Born", "Child Male",
               "Parent BAPQ", "Parent Educ. (y)", "Parent Man",
               "Parent Time w/ Child (%)")
  ) +
  scale_shape_manual(values = c(21, 23)) +
  theme_bw() +
  labs(y = "Estimate", x = "Predictor")

