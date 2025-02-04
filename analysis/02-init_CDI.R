library(tidyverse)
library(here)
library(viridis)
library(irr)

setwd("C:/Users/Trevor/Desktop/ipr")

exclusions <- read_rds("analysis/exclusions.rds")

# Get data ====

ws0 <- read_tsv("data/IPR_MCDI_WS-250127.tsv", show_col_types = FALSE,
                quote = "")

ws <- ws0 %>%
  rename(
    id = PSCID,
    p1p2 = Visit_label
  ) %>%
  select(id, p1p2, Candidate_Age, starts_with("words_produced"),
         starts_with("complexity")) %>%
  mutate(
    across(c(ends_with("number"), ends_with("percentile"), Candidate_Age),
           as.numeric),
  ) %>%
  filter(
    !(id %in% exclusions),
    id != "NULL"
  ) %>%
  arrange(id, p1p2)

ws_wide <- ws %>%
  pivot_wider(id_cols = id, names_from = p1p2, values_from = c(-id, -p1p2)) %>%
  filter(
    !is.na(words_produced_number_iprParent1),
    !is.na(words_produced_number_iprParent2)
  ) %>%
  mutate(
    age = (Candidate_Age_iprParent1 + Candidate_Age_iprParent2) / 2
  )

ws2 <- ws_wide %>%
  select(-starts_with("Candidate_Age")) %>%
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
    r = map(data, ~cor.test(.x$p1, .x$p2, use = "c")),
    r_val = map_dbl(r, ~.x$estimate),
    r_p = map_dbl(r, ~.x$p.value),
    icc = map(data, ~icc(select(.x, p1, p2), type = "agreement")),
    icc_val = map_dbl(icc, ~.x$value),
    icc_p = map_dbl(icc, ~.x$p.value)
  )

write_rds(ws_wide, here("analysis", "cdi_wide_included.rds"))
