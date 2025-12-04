library(tidyverse)
library(lavaan)
library(lavaanPlot)
library(semTools)
library(here)

show_params_std <- function(lvn) {

  pe1 <- parameterEstimates(lvn) %>%
    select(lhs, op, rhs, label, est, pvalue)

  se1 <- standardizedSolution(lvn) %>%
    select(lhs, op, rhs, label, est.std, pvalue)

  result <- left_join(pe1, se1, join_by(lhs, op, rhs, label),
                      suffix = c("_raw", "_std"))

  return(result)

}

all_data <- read_rds(here("analysis", "sem_data.rds")) %>%
  mutate(
    # Create one column with the expectation for their child's sex
    p_cgsschild_pcg = if_else(c_male, p_cgssboys_pcg, p_cgssgirls_pcg),
    p_cgsschild_scg = if_else(c_male, p_cgssboys_scg, p_cgssgirls_scg)
  )

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
  ) %>%
  head(10)

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

show_params_std(vrrsbt_model3)

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

# Exploratory analyses =====

## Rater gender by child sex interaction

all_data2 <- all_data %>%
  mutate(
    pman_x_csex_pcg = case_when(
      p_man_pcg & c_male ~ "MM",
      p_man_pcg & !c_male ~ "MF",
      !p_man_pcg & c_male ~ "FM",
      !p_man_pcg & !c_male ~ "FF",
    ),
    pman_x_csex_scg = case_when(
      p_man_scg & c_male ~ "MM",
      p_man_scg & !c_male ~ "MF",
      !p_man_scg & c_male ~ "FM",
      !p_man_scg & !c_male ~ "FF",
    ),
  )

vrrsbt_modelspec4 <- '

    vrrsb_total_pcg ~~ vrrsb_total_scg

    vrrsb_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_total_pcg_be + p4a*p_tswc_pcg

    c_male ~ i1a*p_man_pcg
    c_male ~ i1b*p_man_scg

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

    c1 := c1a - c1b # age
    c2 := c2a - c2b # sex
    c3 := c3a - c3b # bo

    p1 := p1a - p1b # gender
    p2 := p2a - p2b # educ
    p3 := p3a - p3b # bapq
    p4 := p4a - p4b # tswc

    # parent sex by child gender interaction
    i1 := i1a - i1b

  '

vrrsbt_model4 <- sem(vrrsbt_modelspec4, data = all_data2, missing = "ML")
lavaanPlot(vrrsbt_model4, coefs = TRUE, covs = TRUE, sig = .05)

vrs_model_comparison <- compareFit(vrrsbt_model3, vrrsbt_model4, nested = TRUE)

modificationindices(vrrsbt_model4, sort. = TRUE) %>%
  head(10)

# This does not improve the model - all fit statistics are worse, and the MIs
#   for the new paths are really high

## CGSS ====

vrrsbt_modelspec5 <- '

    vrrsb_total_pcg ~~ vrrsb_total_scg

    # Use p6 as the prefix because p5 was used for the interaction term

    vrrsb_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_total_pcg_be + p4a*p_tswc_pcg +
      p6a*p_cgsschild_pcg

    vrrsb_total_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_total_scg_be + p4b*p_tswc_scg +
      p6b*p_cgsschild_pcg

    c_male ~ i2a*p_cgsschild_pcg
    c_male ~ i2b*p_cgsschild_scg

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

    c1 := c1a - c1b # age
    c2 := c2a - c2b # sex
    c3 := c3a - c3b # bo

    p1 := p1a - p1b # gender
    p2 := p2a - p2b # educ
    p3 := p3a - p3b # bapq
    p4 := p4a - p4b # tswc
    p6 := p6a - p6b # CGSS

    # Use i2 for CGSS term
    i2 := i2a - i2b

  '

vrrsbt_model5 <- sem(vrrsbt_modelspec5, data = all_data, missing = "ML")
lavaanPlot(vrrsbt_model5, coefs = TRUE, covs = TRUE, sig = .05)

vrs_model_comparison <- compareFit(vrrsbt_model3,vrrsbt_model5, nested = TRUE)

# Adding these terms improve A/BIC, but none of the other fit indices
