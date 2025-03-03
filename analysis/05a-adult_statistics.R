adult_demo <- read_rds("demo_parents.rds")

bapq <- read_rds(here("analysis", "bapq_wide_all.rds")) %>%
  select(id, starts_with("total")) %>%
  rowwise() %>%
  mutate(

    # Flag rows where one value is missing
    bapq_missing_iprParent1 =
      is.na(total_self_iprParent1) | is.na(total_partner_iprParent2),
    bapq_missing_iprParent2 =
      is.na(total_self_iprParent2) | is.na(total_partner_iprParent1),

    # Calculate best estimates
    bapq_totalbest_iprParent1 =
      mean(c(total_self_iprParent1, total_partner_iprParent2), na.rm = TRUE),
    bapq_totalbest_iprParent2 =
      mean(c(total_self_iprParent2, total_partner_iprParent1), na.rm = TRUE)
  )

bapq2 <- select(bapq, id, contains("best")) %>%
  pivot_longer(-id, values_to = "bapq_totalbest", names_to = "p1p2") %>%
  mutate(
    p1p2 = str_remove(p1p2, "bapq_totalbest_")
  )

adults <- left_join(adult_demo, bapq2, by = join_by(id, p1p2)) %>%
  mutate(
    p_ed = replace_na(p_ed, "college"),
    p_edy = as.numeric(as.character(p_edy, 16))
  ) %>%
  fastDummies::dummy_cols(select_columns = c("p_gender", "p_ed"))

adults[!complete.cases(adults), ]

adults %>%
  mutate(
    high_bapq = bapq_totalbest >= 3.15
  ) %>%
  pivot_wider(values_from = high_bapq, names_from = p1p2, id_cols = id) %>%
  summarize(
    n = n(),
    .by = c(-id)
  )

bapq_model <- lm(bapq_totalbest ~ p_gender_man + p_gender_nonbinary +
                   p_gender_transwoman +
                   p_ed_some_high + p_ed_high + p_ed_some_college +
                   p_ed_some_grad + p_ed_grad,
                 data = adults)

summary(bapq_model)

adults_bapqmean <- adults %>%
  filter(
    p_gender %in% c("man", "woman")
  ) %>%
  summarize(
    n = n(),
    mean_bapq = mean(bapq_totalbest),
    sd_bapq = sd(bapq_totalbest),
    .by = c(p_ed, p_edy, p_gender)
  ) %>%
  mutate(
    se_bapq = sd_bapq / sqrt(n),
    p_edy = if_else(p_gender == "man", p_edy - 0.125, p_edy + 0.125)
  ) %>%
  filter(
    p_ed != "some_high"
  )

ggplot(adults, aes(x = p_edy, y = bapq_totalbest, color = p_gender)) +
  geom_jitter(width = 0.25, height = 0, alpha = 0.5) +
  geom_errorbar(data = adults_bapqmean,
                aes(x = p_edy, y = mean_bapq, ymin = mean_bapq - se_bapq,
                    ymax = mean_bapq + se_bapq),
                width = 0.1, color = "black") +
  geom_point(data = adults_bapqmean,
             aes(x = p_edy, y = mean_bapq, fill = p_gender),
             shape = 21, size = 2.5, color = "black") +
  scale_x_continuous(breaks = seq(10, 20, by = 2),
                     labels = c("Some HS", "HS", "Some College", "College",
                                "Some Grad", "Grad School")) +
  scale_y_continuous(limits = c(1, NA)) +
  theme_bw() +
  labs(x = "Parent Education", y = "BAPQ Best-Estimate")

