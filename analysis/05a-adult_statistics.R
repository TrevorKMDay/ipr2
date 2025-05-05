library(tidyverse)
library(here)

summarize <- dplyr::summarize

# Load data

inclusions <- read_rds(here("analysis", "vrrsb_wide_included.rds"))$id

# Demographics ====

demo_parents <- read_rds(here("analysis", "demo_parents.rds"))

# Reassign 'iprParent[12]' to [PS]CG
demo_parents_new <- demo_parents %>%
  select(id, p1p2, p_pcg)

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
  select(id, starts_with("p_man"), starts_with("p_ed"),
         starts_with("p_tswc"))

## Load BAPQ ====

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

adults <- left_join(demo_parents, bapq2, by = join_by(id, p1p2)) %>%
  mutate(
    p_ed = replace_na(p_ed, "college"),
    p_edy = as.numeric(as.character(p_edy, 16))
  ) %>%
  fastDummies::dummy_cols(select_columns = c("p_gender", "p_ed"))

adults[!complete.cases(adults), ]

table(adults$bapq_totalbest >= 3.15)

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

# CGSS ====

cgss_qs <- tribble(
    ~name, ~scale,
    "ballet", "girls",
    "room", "home",
    "laundry", "home",
    "garbage", "home",
    "football", "boys",
    "military_toys", "boys",
    "kitchen_toys", "girls",
    "gun_toys", "boys",
    "jewelry_toys", "girls",
    "dishes_toys", "girls",
    "tools_toys", "boys",
    "sweeping", "home",
    "grass", "home",
    "set_table", "home",
    "nurse_toys", "girls",
    "hopscotch", "girls",
    "gi_joes", "boys",
    "truck_toys", "boys",
    "barbie_dolls", "girls",
    "dishes", "home",
    "baby_dolls", "girls",
    "car_toys", "boys",
  ) %>%
  mutate(
    item = row_number()
  )

cgss <- read_delim(here("data", "IPR_demographics_extended-250204.tsv"),
                            delim = "\t", show_col_types = FALSE) %>%
  rename(
    id = PSCID,
    p1p2 = Visit_label,
  ) %>%
  filter(
    id %in% inclusions
  ) %>%
  select(id, p1p2, contains("CGSS")) %>%
  pivot_longer(starts_with("q"), names_to = "item") %>%
  mutate(

    item = str_remove_all(item, "[^0-9]") %>%
      as.numeric(),

    value_numeric = case_match(
      value,
      "very_negative" ~ -3,
      "somewhat_negative" ~ -2,
      "slightly_negative" ~ -1,
      "neutral" ~ 0,
      "slightly_positive" ~ 1,
      "somewhat_positive" ~ 2,
      "very_positive"~ 3,
      "not_answered" ~ NA,
    )

  ) %>%
  left_join(cgss_qs, by = join_by(item))

write_rds(cgss, "analysis/cgss.rds")

cgss_summary <- cgss %>%
  group_by(id, p1p2, scale) %>%
  summarize(
    n_nonNA = sum(!is.na(value_numeric)),
    score = sum(value_numeric, na.rm = TRUE)
  ) %>%
  mutate(
    score = if_else(n_nonNA < 4, NA, score)
  ) %>%
  pivot_wider(id_cols = id, names_from = c(p1p2, scale),
              values_from = score) %>%
  ungroup()

library(corrr)

cgss_cors_rnp <- cgss_summary %>%
  ungroup() %>%
  select(-id) %>%
  as.matrix() %>%
  Hmisc::rcorr()

cgss_cors <- cgss_cors_rnp$r %>%
  as_cordf() %>%
  shave() %>%
  stretch(na.rm = TRUE)

cgss_p <- cgss_cors_rnp$P %>%
  as_cordf() %>%
  shave() %>%
  stretch(na.rm = TRUE) %>%
  rename(
    p = r
  )

cgss_cors_final <- left_join(cgss_cors, cgss_p) %>%
  mutate(
    p_cor = p.adjust(p, method = "fdr"),
    stars = case_when(
      p_cor < .05  ~ "*",
      p_cor < .01  ~ "**",
      p_cor < .001 ~ "***",
      TRUE ~ ""
    ),
    label = paste0(round(r, 2), stars)
  )

lims <- unique(c(cgss_cors$x, cgss_cors$y))

cgss_cors_final %>%
  pivot_wider(id_cols = x, names_from = y, values_from = label) %>%
  clipr::write_clip()

ggplot(cgss_cors_final, aes(x = x, y = y, fill = r)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  scale_x_discrete(limits = lims) +
  scale_y_discrete(limits = lims) +
  scale_fill_gradient2() +
  theme_bw() +
  labs(x = NULL, y = NULL,
       caption = "* p < .05 after FDR correction")
