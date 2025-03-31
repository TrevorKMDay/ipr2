library(tidyverse)
library(here)
library(viridis)
library(irr)

exclusions <- read_rds(here("analysis", "exclusions.rds"))

# Get data ====

demo_parents <- read_rds(here("analysis", "demo_parents.rds")) %>%
  select(id, p1p2, p_pcg)

vrrsb0 <- read_tsv(here("data", "IPR_vrRSB-250127.tsv"),
                   show_col_types = FALSE, na = "NULL") %>%
  arrange(PSCID, Visit_label)

# Reorganize data ====

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
  select(id, p1p2, age, VRS, RSB, SC, RRB, q_50_num_words) %>%
  filter(
    id %in% demo_parents$id
  ) %>%
  rename(
    total = RSB,
    nwords = q_50_num_words
  ) %>%
  left_join(demo_parents, by = join_by(id, p1p2))

vrrsb2 <- vrrsb %>%
  select(-p1p2) %>%
  pivot_wider(names_from = p_pcg, values_from = c(-id, -p_pcg)) %>%
  mutate(
    age = (age_pcg + age_pcg) / 2,
    outlier = RRB_pcg > 20
  )

vrrsb3 <- vrrsb2 %>%
  select(-matches("age_")) %>%
  pivot_longer(-c(id, age, outlier)) %>%
  separate_wider_delim(name, delim = "_", names = c("name", "pcg")) %>%
  pivot_wider(names_from = pcg) %>%
  group_by(name) %>%
  mutate(
    across(c(pcg, scg), ~scale(.x)[,1], .names = "{.col}_Z")
  )

ggplot(vrrsb3, aes(x = pcg, y = scg, color = age)) +
  geom_jitter(alpha = 0.5, height = 0.2, width = 0.2) +
  geom_smooth(method = "lm") +
  geom_smooth(data = filter(vrrsb3, !outlier), method = "lm") +
  scale_color_viridis(limits = c(23, 27), oob = scales::squish) +
  facet_wrap(vars(name), scales = "free") +
  theme_bw()

vrrsb4 <- vrrsb3 %>%
  group_by(name) %>%
  nest() %>%
  mutate(

    r = map(data, ~cor.test(.x$pcg, .x$scg, use = "c")),
    r_val = map_dbl(r, ~round(.x$estimate, 2)),
    r_p = map_dbl(r, ~.x$p.value),

    icc = map(data, ~icc(select(.x, pcg, scg),
                         type = "agreement")),
    icc_val = map_dbl(icc, ~round(.x$value, 2)),
    icc_p = map_dbl(icc, ~.x$p.value)

  )

vrrsb4_noutlier <- vrrsb3 %>%
  filter(
    !outlier
  ) %>%
  group_by(name) %>%
  nest() %>%
  mutate(

    r = map(data, ~cor.test(.x$pcg, .x$scg, use = "c")),
    r_val = map_dbl(r, ~round(.x$estimate, 2)),
    r_p = map_dbl(r, ~.x$p.value),

    icc = map(data, ~icc(select(.x, pcg, scg),
                         type = "agreement")),
    icc_val = map_dbl(icc, ~round(.x$value, 2)),
    icc_p = map_dbl(icc, ~.x$p.value)

  )

vrrsb_summary <- left_join(vrrsb4, vrrsb4_noutlier, join_by(name),
                           suffix = c(".all", ".299")) %>%
  select(-starts_with("data"), -icc.all, -icc.299, -r.all, -r.299) %>%
  ungroup() %>%
  mutate(
    across(where(is.numeric), ~round(.x, 3))
  )

write_rds(vrrsb2, here("analysis", "vrrsb_wide_included.rds"))
