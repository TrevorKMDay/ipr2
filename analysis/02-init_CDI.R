library(tidyverse)
library(here)
library(viridis)
library(irr)

setwd("C:/Users/Trevor/Desktop/ipr")

inclusions <- read_rds("analysis/vrrsb_wide_included.rds")$id

# Get data ====

demo_parents <- read_rds(here("analysis", "demo_parents.rds")) %>%
  select(id, p1p2, p_pcg)

ws0 <- read_tsv("data/IPR_MCDI_WS-250127.tsv", show_col_types = FALSE,
                quote = "") %>%
  rename(
    id = PSCID,
    p1p2 = Visit_label,
    age = Candidate_Age,
  ) %>%
  filter(
    id %in% inclusions,
  ) %>%
  arrange(id, p1p2) %>%
  left_join(demo_parents)

ws <- ws0 %>%
  select(id, p_pcg, age, starts_with("words_produced"),
         starts_with("complexity"), ) %>%
  mutate(
    across(c(ends_with("number"), ends_with("percentile"), age),
           as.numeric)
  )

ws_IIBC <- ws0 %>%
  select(id, p_pcg, matches("II_[BC]")) %>%
  pivot_longer(-c(id, p_pcg)) %>%
  separate_wider_delim(name, "_", names = c(NA, "section", "item")) %>%
  mutate(
    section = case_match(section, "B" ~ "forms", "C" ~ "endings"),
    value = case_when(value == "1" ~ TRUE, value == "NULL" ~ FALSE)
  ) %>%
  group_by(id, p_pcg, section) %>%
  summarize(
    total = sum(value)
  ) %>%
  pivot_wider(names_from = p_pcg, values_from = total) %>%
  ungroup()

ws_IIBC_icc <- ws_IIBC %>%
  group_by(section) %>%
  nest() %>%
  mutate(

    r = map(data, ~cor.test(.x$pcg, .x$scg, use = "c")),
    r_val = map_dbl(r, ~.x$estimate),
    r_p = map_dbl(r, ~.x$p.value),

    icc = map(data, ~icc(select(.x, pcg, scg), type = "agreement")),
    icc_val = map_dbl(icc, ~.x$value),
    icc_p = map_dbl(icc, ~.x$p.value)

  )

ws_wide <- ws %>%
  pivot_wider(id_cols = id, names_from = p_pcg, values_from = c(-id, -p_pcg)) %>%
  filter(
    !is.na(words_produced_number_pcg),
    !is.na(words_produced_number_scg)
  ) %>%
  mutate(
    age = (age_pcg + age_scg) / 2
  )

ws2 <- ws_wide %>%
  select(-starts_with("age_")) %>%
  pivot_longer(-c(id, age)) %>%
  mutate(
    p1p2 = str_extract(name, "[ps]cg$"),
    name = str_remove(name, "_[ps]cg$")
  ) %>%
  pivot_wider(names_from = p1p2)

ggplot(ws2, aes(x = pcg, y = scg, color = age)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_viridis(limits = c(23, 27), oob = scales::squish) +
  facet_wrap(vars(name), scales = "free") +
  theme_bw()

ws3 <- ws2 %>%
  group_by(name) %>%
  nest() %>%
  mutate(

    r = map(data, ~cor.test(.x$pcg, .x$scg, use = "c")),
    r_val = map_dbl(r, ~.x$estimate),
    r_p = map_dbl(r, ~.x$p.value),

    icc = map(data, ~icc(select(.x, pcg, scg), type = "agreement")),
    icc_val = map_dbl(icc, ~.x$value),
    icc_p = map_dbl(icc, ~.x$p.value)
  )

write_rds(ws_wide, here("analysis", "cdi_wide_included.rds"))

# Agreement ====

ws_wide2 <- ws_wide %>%
  filter(
    id %in% inclusions
  ) %>%
  arrange(age) %>%
  mutate(
    words_diff = abs(words_produced_number_pcg - words_produced_number_scg),
  )

mean(ws_wide2$words_diff)
sd(ws_wide2$words_diff)

ggplot(ws_wide2, aes(words_diff)) +
  geom_density()

ws_wide_by_age <- ws_wide2 %>%
  mutate(
    age = floor(age)
  ) %>%
  group_by(age) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    mean_diff = map_dbl(data, ~mean(.x$words_diff)),
    sd_mean_diff = map_dbl(data, ~sd(.x$words_diff))
  ) %>%
  filter(
    # Small groups
    age > 21,
    age < 28,
  ) %>%
  mutate(
    se_mean_diff = sd_mean_diff / sqrt(n)
  )

ggplot(ws_wide_by_age, aes(x = age, y = mean_diff)) +
  geom_pointrange(aes(ymin = mean_diff - se_mean_diff,
                      ymax = mean_diff + se_mean_diff)) +
  geom_line() +
  theme_bw()
