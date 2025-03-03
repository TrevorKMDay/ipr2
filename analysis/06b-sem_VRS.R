vrs_modelspec1 <- '

    vrrsb_VRS_pcg ~~ vrrsb_VRS_scg

    vrrsb_VRS_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_totalBE_pcg + p4a*p_tswc_pcg

    vrrsb_VRS_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_totalBE_scg + p4b*p_tswc_scg

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_totalBE_pcg ~~ p_bapq_totalBE_scg
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~~ p_bapq_totalBE_pcg
    p_edy_scg ~~ p_bapq_totalBE_scg

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~~ p_bapq_totalBE_pcg
    p_man_scg ~~ p_bapq_totalBE_scg

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

vrs_model1 <- sem(vrs_modelspec1, data = all_data)
lavaanPlot(vrs_model1, coefs = TRUE, covs = TRUE, sig = .05)

summary(regorz1)

modificationIndices(regorz1, sort. = TRUE)

vrs_modelspec2 <- '

    vrrsb_VRS_pcg ~~ vrrsb_VRS_scg

    vrrsb_VRS_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_totalBE_pcg + p4a*p_tswc_pcg

    vrrsb_VRS_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_totalBE_scg + p4b*p_tswc_scg

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_totalBE_pcg ~~ p_bapq_totalBE_scg
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    p_tswc_pcg ~ p_man_pcg
    p_tswc_scg ~ p_man_scg
    p_edy_pcg ~ p_man_pcg
    p_edy_scg ~ p_man_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~~ p_bapq_totalBE_pcg
    p_edy_scg ~~ p_bapq_totalBE_scg

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~~ p_bapq_totalBE_pcg
    p_man_scg ~~ p_bapq_totalBE_scg

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

vrs_model2 <- sem(vrs_modelspec2, data = all_data)
lavaanPlot(vrs_model2, coefs = TRUE, covs = TRUE, sig = .05)
vrs_model_comparison <- compareFit(vrs_model1, vrs_model2)

modificationIndices(vrs_model2, sort. = TRUE)

vrs_modelspec3 <- '

    vrrsb_VRS_pcg ~~ vrrsb_VRS_scg

    vrrsb_VRS_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_totalBE_pcg + p4a*p_tswc_pcg

    vrrsb_VRS_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_totalBE_scg + p4b*p_tswc_scg

    # Set some covariances to 0
    c_male ~~ 0*c_first_born
    c_male ~~ 0*age
    age ~~ 0*c_first_born

    # Covariances between parents
    p_edy_pcg ~~ p_edy_scg
    p_bapq_totalBE_pcg ~~ p_bapq_totalBE_scg
    p_man_pcg ~~ p_man_scg
    p_tswc_pcg ~~ p_tswc_scg

    p_tswc_pcg ~ p_man_pcg + p_edy_pcg
    p_tswc_scg ~ p_man_scg + p_edy_scg
    p_edy_pcg ~ p_man_pcg + p_bapq_totalBE_pcg
    p_edy_scg ~ p_man_scg + p_bapq_totalBE_scg

    # Let educ/asd covary, est r=.17
    p_edy_pcg ~~ p_bapq_totalBE_pcg
    p_edy_scg ~~ p_bapq_totalBE_scg

    # Let parent gender/asd covary, d=.37
    p_man_pcg ~~ p_bapq_totalBE_pcg
    p_man_scg ~~ p_bapq_totalBE_scg

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

vrs_model3 <- sem(vrs_modelspec3, data = all_data)
lavaanPlot(vrs_model3, coefs = TRUE, covs = TRUE, sig = .05)
vrs_model_comparison <- compareFit(vrs_model1, vrs_model2, vrs_model3)

modificationIndices(vrs_model3, sort. = TRUE)

pe <- parameterestimates(vrs_model3) %>%
  filter(
    str_detect(lhs, "vrrsb_VRS_"),
    !str_detect(rhs, "vrrsb_VRS")
  ) %>%
  mutate(
    parent = case_when(
      str_detect(lhs, "_pcg$") ~ "pcg",
      str_detect(lhs, "_scg$") ~ "scg",
      TRUE ~ "child"
    ),
    rhs = str_remove(rhs, "_[ps]cg$")
  )

ggplot(pe, aes(x = rhs)) +
  geom_pointrange(aes(y = est, ymin = ci.lower, ymax = ci.upper,
                      fill = parent, linetype = parent, shape = parent),
                  size = 1,
                  position = position_dodge2(width = 0.2)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  scale_x_discrete(
    labels = c("Child Age (mo)", "Child First Born", "Child Male",
               "Parent BAPQ", "Parent Educ. (y)", "Parent Man",
               "Parent Time w/ Child (%)")
  ) +
  scale_shape_manual(values = c(21, 23)) +
  coord_flip() +
  theme_bw() +
  labs(x = "Estimate", y = "Predictor")

## CDI ====

cdi_model_regorz <-
  '
    cdi_total_pcg ~~ cdi_total_scg

    cdi_total_pcg ~ c1a*age + c2a*c_male + c3a*c_first_born +
        c4a*vrrsb_VRS_pcg +
      p1a*p_man_pcg + p2a*p_edy_pcg + p3a*p_bapq_totalBE_pcg + p4a*p_tswc_pcg

    cdi_total_scg ~ c1b*age + c2b*c_male + c3b*c_first_born +
        c4b*vrrsb_VRS_pcg +
      p1b*p_man_scg + p2b*p_edy_scg + p3b*p_bapq_totalBE_scg + p4b*p_tswc_scg

    vrrsb_VRS_pcg ~ age + c_male
    vrrsb_VRS_scg ~ age + c_male

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

    c1 := c1a - c1b
    c2 := c2a - c2b
    c3 := c3a - c3b
    c4 := c4a - c4b

    p1 := p1a - p1b
    p2 := p2a - p2b
    p3 := p3a - p3b
    p4 := p4a - p4b

  '

all_data2 <- all_data %>%
  mutate(
    across(starts_with("cdi_total"), ~ . / 100)
  )

cdi_regorz1 <- sem(cdi_model_regorz, data = all_data2)
lavaanPlot(cdi_regorz1, coefs = TRUE, covs = TRUE, sig = .05)

summary(cdi_regorz1)

cdi_pe <- parameterestimates(cdi_regorz1) %>%
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
    rhs = str_remove(rhs, "_[ps]cg$"),

    est = est * 100,
    ci.lower = ci.lower * 100,
    ci.upper = ci.upper * 100
  )

ggplot(cdi_pe, aes(x = rhs)) +
  geom_pointrange(aes(y = est, ymin = ci.lower, ymax = ci.upper,
                      color = parent),
                  position = position_dodge2(width = 0.1)) +
  geom_hline(yintercept = 0, color = "red") +
  theme_bw() +
  labs(x = "Covariate", y = "Words")
