library(tidyverse)
library(lavaan)
library(semTools)
library(here)
library(lavaanPlot)

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
    across(starts_with("cdi_total"), ~ . / 100),
    # Create one column with the expectation for their child's sex
    p_cgsschild_pcg = if_else(c_male, p_cgssboys_pcg, p_cgssgirls_pcg),
    p_cgsschild_scg = if_else(c_male, p_cgssboys_scg, p_cgssgirls_scg)
  )

## Initial model ====

cdi_modelspec1 <- '

    cdi_total_pcg ~~ cdi_total_scg

    cdi_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
        c4a*vrrsb_mean +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_total_pcg_be + p4a*p_tswc_pcg

    cdi_total_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
        c4b*vrrsb_mean +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_total_scg_be + p4b*p_tswc_scg

    vrrsb_mean ~ age + c_male
    vrrsb_mean ~ age + c_male

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_total_pcg_be ~~ p_bapq_total_scg_be
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~ p_bapq_total_pcg_be
    p_edy_scg ~ p_bapq_total_scg_be

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~ p_bapq_total_pcg_be
    p_man_scg ~ p_bapq_total_scg_be

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    c1 := c1a - c1b
    c2 := c2a - c2b
    c3 := c3a - c3b
    c4 := c4a - c4b

    p1 := p1a - p1b
    p2 := p2a - p2b
    p3 := p3a - p3b
    p4 := p4a - p4b

  '

cdi_model1 <- sem(cdi_modelspec1, data = all_data, missing = "ML")
lavaanPlot(cdi_model1, coefs = TRUE, covs = TRUE, sig = .05)

summary(cdi_model1)

modificationindices(cdi_model1, sort. = TRUE) %>%
  mutate(
    mi = round(mi, 1)
  ) %>%
  filter(
    mi > 10,
    (str_detect(lhs, "pcg") & str_detect(rhs, "pcg")) |
      (str_detect(lhs, "scg") & str_detect(rhs, "scg")) |
      (str_detect(lhs, "vrrsb_mean") | str_detect(rhs, "vrrsb_mean"))
  )

# Second model ====

cdi_modelspec2 <- '

    cdi_total_pcg ~~ cdi_total_scg

    cdi_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
        c4a*vrrsb_mean +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_total_pcg_be + p4a*p_tswc_pcg

    cdi_total_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
        c4b*vrrsb_mean +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_total_scg_be + p4b*p_tswc_scg

    vrrsb_mean ~ age + c_male + p_bapq_total_pcg_be + p_bapq_total_scg_be
    vrrsb_mean ~ age + c_male + p_bapq_total_pcg_be + p_bapq_total_scg_be

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_total_pcg_be ~~ p_bapq_total_scg_be
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~ p_bapq_total_pcg_be
    p_edy_scg ~ p_bapq_total_scg_be

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~ p_bapq_total_pcg_be
    p_man_scg ~ p_bapq_total_scg_be

    p_tswc_pcg ~ p_man_pcg
    p_tswc_scg ~ p_man_scg
    p_edy_pcg ~ p_man_pcg
    p_edy_scg ~ p_man_scg

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    c1 := c1a - c1b
    c2 := c2a - c2b
    c3 := c3a - c3b
    c4 := c4a - c4b

    p1 := p1a - p1b
    p2 := p2a - p2b
    p3 := p3a - p3b
    p4 := p4a - p4b

  '

cdi_model2 <- sem(cdi_modelspec2, data = all_data, missing = "ML")
lavaanPlot(cdi_model2, coefs = TRUE, covs = TRUE, sig = .05)

cdi_model_comparison <- compareFit(cdi_model1, cdi_model2)

summary(cdi_model2)

modificationindices(cdi_model2, sort. = TRUE) %>%
  mutate(
    mi = round(mi, 1)
  ) %>%
  filter(
    (str_detect(lhs, "pcg") & str_detect(rhs, "pcg")) |
      (str_detect(lhs, "scg") & str_detect(rhs, "scg"))
  )

cdi_pe <- parameterestimates(cdi_model2) %>%
  filter(
    str_detect(lhs, "cdi"),
    !str_detect(rhs, "cdi"),
    op == "~"
  ) %>%
  mutate(
    parent = case_when(
      str_detect(lhs, "_pcg$") ~ "pcg",
      str_detect(lhs, "_scg$") ~ "scg",
      TRUE ~ "child"
    ),

    rhs = str_remove(rhs, "_[ps]cg"),

    across(c(est, se, ci.lower, ci.upper), ~ .x * 100),
    across(c(est, se, z, ci.lower, ci.upper), ~round(.x, 1)),
    pvalue = round(pvalue, 4)

  )

ggplot(cdi_pe, aes(x = rhs)) +
  geom_pointrange(aes(y = est, ymin = ci.lower, ymax = ci.upper,
                      fill = parent, linetype = parent, shape = parent),
                  size = 1,
                  position = position_dodge2(width = 0.2)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  coord_flip() +
  scale_x_discrete(
    labels = c("Child Age (mo)", "Child First Born", "Child Male",
               "Parent BAPQ", "Parent Educ. (y)", "Parent Man",
               "Parent Time w/ Child (%)", "Child VRS")
  ) +
  scale_shape_manual(values = c(21, 23)) +
  theme_bw() +
  labs(y = "Estimate", x = "Predictor")

show_params_std(cdi_model2)

# Exploratory analyses ====

cdi_modelspec3 <- '

    cdi_total_pcg ~~ cdi_total_scg

    cdi_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
        c4a*vrrsb_mean +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_total_pcg_be + p4a*p_tswc_pcg

    cdi_total_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
        c4b*vrrsb_mean +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_total_scg_be + p4b*p_tswc_scg

    c_male ~ i1a*p_man_pcg
    c_male ~ i1b*p_man_scg

    vrrsb_mean ~ age + c_male + p_bapq_total_pcg_be + p_bapq_total_scg_be
    vrrsb_mean ~ age + c_male + p_bapq_total_pcg_be + p_bapq_total_scg_be

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_total_pcg_be ~~ p_bapq_total_scg_be
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~ p_bapq_total_pcg_be
    p_edy_scg ~ p_bapq_total_scg_be

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~ p_bapq_total_pcg_be
    p_man_scg ~ p_bapq_total_scg_be

    p_tswc_pcg ~ p_man_pcg
    p_tswc_scg ~ p_man_scg
    p_edy_pcg ~ p_man_pcg
    p_edy_scg ~ p_man_scg

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    c1 := c1a - c1b
    c2 := c2a - c2b
    c3 := c3a - c3b
    c4 := c4a - c4b

    p1 := p1a - p1b
    p2 := p2a - p2b
    p3 := p3a - p3b
    p4 := p4a - p4b

    i1 := i1a - i1b

  '

cdi_model3 <- sem(cdi_modelspec3, data = all_data, missing = "ML")
lavaanPlot(cdi_model3, coefs = TRUE, covs = TRUE, sig = .05)

compareFit(cdi_model2, cdi_model3) %>%
  summary()

## CGSS ====

cdi_modelspec4 <- '

    cdi_total_pcg ~~ cdi_total_scg

    cdi_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
        c4a*vrrsb_mean +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_total_pcg_be + p4a*p_tswc_pcg +
      p6a*p_cgsschild_pcg

    cdi_total_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
        c4b*vrrsb_mean +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_total_scg_be + p4b*p_tswc_scg +
      p6a*p_cgsschild_scg

    c_male ~ i2a*p_cgsschild_pcg
    c_male ~ i2b*p_cgsschild_scg

    vrrsb_mean ~ age + c_male + p_bapq_total_pcg_be + p_bapq_total_scg_be
    vrrsb_mean ~ age + c_male + p_bapq_total_pcg_be + p_bapq_total_scg_be

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_total_pcg_be ~~ p_bapq_total_scg_be
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~ p_bapq_total_pcg_be
    p_edy_scg ~ p_bapq_total_scg_be

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~ p_bapq_total_pcg_be
    p_man_scg ~ p_bapq_total_scg_be

    p_tswc_pcg ~ p_man_pcg
    p_tswc_scg ~ p_man_scg
    p_edy_pcg ~ p_man_pcg
    p_edy_scg ~ p_man_scg

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    c1 := c1a - c1b
    c2 := c2a - c2b
    c3 := c3a - c3b
    c4 := c4a - c4b

    p1 := p1a - p1b
    p2 := p2a - p2b
    p3 := p3a - p3b
    p4 := p4a - p4b

    i2 := i2a - i2b

  '

cdi_model4 <- sem(cdi_modelspec4, data = all_data, missing = "ML")
lavaanPlot(cdi_model4, coefs = TRUE, covs = TRUE, sig = .05)

compareFit(cdi_model2, cdi_model4) %>%
  summary()

summary(cdi_model4)
