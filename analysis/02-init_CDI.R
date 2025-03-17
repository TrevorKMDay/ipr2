library(tidyverse)
library(here)
library(viridis)
library(irr)

setwd("C:/Users/Trevor/Desktop/ipr")

exclusions <- read_rds("analysis/exclusions.rds")
inclusions <- read_rds("analysis/vrrsb_wide_included.rds")$id

# Get data ====

ws0 <- read_tsv("data/IPR_MCDI_WS-250127.tsv", show_col_types = FALSE,
                quote = "") %>%
  rename(
    id = PSCID,
    p1p2 = Visit_label,
    age = Candidate_Age,
  ) %>%
  filter(
    !(id %in% exclusions),
    id %in% inclusions,
    id != "NULL"
  ) %>%
  arrange(id, p1p2)

ws <- ws0 %>%
  select(id, p1p2, age, starts_with("words_produced"),
         starts_with("complexity"), ) %>%
  mutate(
    across(c(ends_with("number"), ends_with("percentile"), age),
           as.numeric)
  )

ws_IIBC <- ws0 %>%
  select(id, p1p2, matches("II_[BC]")) %>%
  pivot_longer(-c(id, p1p2)) %>%
  separate_wider_delim(name, "_", names = c(NA, "section", "item")) %>%
  mutate(
    section = case_match(section, "B" ~ "forms", "C" ~ "endings"),
    value = case_when(value == "1" ~ TRUE, value == "NULL" ~ FALSE)
  ) %>%
  group_by(id, p1p2, section) %>%
  summarize(
    total = sum(value)
  ) %>%
  pivot_wider(names_from = p1p2, values_from = total)

ws_IIBC %>%
  group_by(section) %>%
  nest() %>%
  mutate(
    r = map(data, ~cor.test(.x$iprParent1, .x$iprParent2, use = "c")),
    r_val = map_dbl(r, ~.x$estimate),
    r_p = map_dbl(r, ~.x$p.value),
    icc = map(data, ~icc(select(.x, iprParent1, iprParent2),
                         type = "agreement")),
    icc_val = map_dbl(icc, ~.x$value),
    icc_p = map_dbl(icc, ~.x$p.value)
  )

ws_wide <- ws %>%
  pivot_wider(id_cols = id, names_from = p1p2, values_from = c(-id, -p1p2)) %>%
  filter(
    !is.na(words_produced_number_iprParent1),
    !is.na(words_produced_number_iprParent2)
  ) %>%
  mutate(
    age = (age_iprParent1 + age_iprParent2) / 2
  )

ws2 <- ws_wide %>%
  select(-starts_with("age")) %>%
  pivot_longer(-c(id, age)) %>%
  mutate(
    p1p2 = str_extract(name, "iprParent[12]$"),
    name = str_remove(name, "_iprParent[12]$")
  ) %>%
  pivot_wider(names_from = p1p2)

ggplot(ws2, aes(x = iprParent1, y = iprParent2, color = age)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_viridis(limits = c(23, 27), oob = scales::squish) +
  facet_wrap(vars(name), scales = "free") +
  theme_bw()

ws3 <- ws2 %>%
  group_by(name) %>%
  nest() %>%
  mutate(
    r = map(data, ~cor.test(.x$iprParent1, .x$iprParent2, use = "c")),
    r_val = map_dbl(r, ~.x$estimate),
    r_p = map_dbl(r, ~.x$p.value),
    icc = map(data, ~icc(select(.x, iprParent1, iprParent2),
                         type = "agreement")),
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
    words_diff = abs(words_produced_number_iprParent1 - words_produced_number_iprParent2),
  )

mean(ws_wide2$words_diff)
sd(ws_wide2$words_diff)

ggplot(ws_wide2, aes(words_diff)) +
  geom_density()

ws_wide_by_age <- ws_wide2 %>%
  mutate(
    age = round(age)
  ) %>%
  group_by(age) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    mean_diff = map_dbl(data, ~mean(.x$words_diff)),
    sd_mean_diff = map_dbl(data, ~sd(.x$words_diff))
  ) %>%
  filter(
    age <= 28,
    id %in% inclusions
  ) %>%
  mutate(
    se_mean_diff = sd_mean_diff / sqrt(n)
  )

ggplot(ws_wide_by_age, aes(x = age, y = mean_diff)) +
  geom_pointrange(aes(ymin = mean_diff - se_mean_diff,
                      ymax = mean_diff + se_mean_diff)) +
  geom_line() +
  theme_bw()



