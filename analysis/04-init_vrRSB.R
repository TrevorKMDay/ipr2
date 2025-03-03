library(tidyverse)
library(here)
library(viridis)
library(irr)

exclusions <- read_rds(here("analysis", "exclusions.rds"))

# Get data ====

vrrsb0 <- read_tsv(here("data", "IPR_vrRSB-250127.tsv"), show_col_types = FALSE,
                   na = "NULL") %>%
  arrange(PSCID, Visit_label)

vrrsb <- vrrsb0 %>%
  rename(
    id = PSCID,
    age = Candidate_Age,
    p1p2 = Visit_label,
    VRS = video_reference_score,
    RSB = RSB_total_score,
    SC = social_communicative_items,
    RRB = restricted_repetitive_items
  ) %>%
  select(id, p1p2, age, VRS, RSB, SC, RRB) %>%
  filter(
    !(id %in% exclusions),
    id != "NULL"
  )

vrrsb2 <- vrrsb %>%
  pivot_wider(names_from = p1p2, values_from = c(-id, -p1p2)) %>%
  filter(
    !is.na(VRS_iprParent1),
    !is.na(VRS_iprParent2)
  ) %>%
  mutate(
    age = (age_iprParent1 + age_iprParent2) / 2
  )

vrrsb3 <- vrrsb2 %>%
  select(-matches("age_")) %>%
  pivot_longer(-c(id, age)) %>%
  separate_wider_delim(name, delim = "_", names = c("name", "p1p2")) %>%
  pivot_wider(names_from = p1p2, )

ggplot(vrrsb3, aes(x = iprParent1, y = iprParent2, color = age)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_viridis(limits = c(23, 27), oob = scales::squish) +
  facet_wrap(vars(name), scales = "free") +
  theme_bw()

vrrsb4 <- vrrsb3 %>%
  group_by(name) %>%
  nest() %>%
  mutate(

    r = map(data, ~cor.test(.x$iprParent1, .x$iprParent2, use = "c")),
    r_val = map_dbl(r, ~round(.x$estimate, 3)),
    r_p = map_dbl(r, ~.x$p.value),

    icc = map(data, ~icc(select(.x, iprParent1, iprParent2),
                         type = "agreement")),
    icc_val = map_dbl(icc, ~round(.x$value, 3)),
    icc_p = map_dbl(icc, ~.x$p.value)
  )

write_rds(vrrsb2, here("analysis", "vrrsb_wide_included.rds"))
