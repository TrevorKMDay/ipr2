
library(tidyverse)
library(viridis)
library(here)
library(vcd)

summarize <- dplyr::summarize

# Unigram frequency ====

freq <- read_csv(here("data/unigram_freq.csv"), show_col_types = FALSE) %>%
  mutate(
    freq = count / sum(count),
    freq_log10 = log10(freq)
  )

# Wordbank ====

wb <- read_rds(here("data/Wordbank-WS-191105.rds")) %>%
  filter(
    type == "word",
    age > 21,
    age < 29
  ) %>%
  group_by(category, definition) %>%
  summarize(
    n = n(),
    produces = sum(value == "produces", na.rm = TRUE)
  ) %>%
  mutate(
    prod_rate = produces / n,
    expected_agr = (prod_rate^2) + ((1 - prod_rate)^2),

    definition = str_to_lower(definition) %>%
      str_remove_all("[*]") ,

    definition2 = str_remove_all(definition, " ") %>%
      str_remove("'") %>%
      str_remove("[(].*[)]")%>%
      str_remove_all("/.*"),

  ) %>%
  left_join(select(freq, word, freq_log10), by = join_by(definition2 == word))

missing_freq <- wb %>%
  filter(
    is.na(freq_log10)
  )

categories_in_order <- levels(wb$category)
words_in_order <- wb$definition

# Read in my data ====

included <- read_rds(here("analysis", "sem_data.rds"))$id

cdi <- read_tsv(here("data/IPR_MCDI_WS-250127.tsv"), quote = "",
               show_col_types = FALSE) %>%
  filter(
    PSCID %in% included
  )

cdi_items <- cdi %>%
  select(PSCID, Visit_label, Candidate_Age, matches("I_A_[0-9]+_[0-9]+")) %>%
  pivot_longer(starts_with("I_A_"), values_to = "produces") %>%
  separate_wider_delim(name, delim = "_",
                       names = c(NA, NA, "category", "item")) %>%
  mutate(
    produces = produces == "says",
    syntax = if_else(as.numeric(category) >= 16, "func", "content"),
    category = categories_in_order[as.numeric(category)],
  ) %>%
  rename(
    id = PSCID,
    age = Candidate_Age
  ) %>%
  arrange(id, Visit_label) %>%
  group_by(id) %>%
  mutate(
    definition = rep(words_in_order, 2)
  )

# where did these guys go?
length(unique(cdi_items$id))

cdi_items_wide <- cdi_items %>%
  pivot_wider(id_cols = c(id, category, definition, syntax),
              names_from = Visit_label, values_from = c(age, produces)) %>%
  mutate(
    across(starts_with("age_"), as.numeric),
    same = produces_iprParent1 == produces_iprParent2
  )

cdi_parent_overlap <- cdi_items_wide %>%
  group_by(id) %>%
  nest() %>%
  mutate(
    age = map_dbl(data, ~mean(c(.x$age_iprParent1, .x$age_iprParent2))),
    p1 = map_int(data, ~sum(.x$produces_iprParent1)),
    p2 = map_int(data, ~sum(.x$produces_iprParent2)),
    n_both_endorsed = map_int(data, ~sum(.x$produces_iprParent1 & .x$same)),

    p1_pct = n_both_endorsed / p1,
    p2_pct = n_both_endorsed / p2,

    disagreement = abs(p1 - p2),

    p1_interval = cut(p1, breaks = seq(0, 680, by = 68),
                      labels = seq(68, 680, by = 68)),
    p2_interval = cut(p2, breaks = seq(0, 680, by = 68),
                      labels = seq(68, 680, by = 68))

  ) %>%
  arrange(p1, p2)

cdi_parent_overlap_bins <- cdi_parent_overlap %>%
  mutate(
    across(c(p1_interval, p2_interval), ~as.numeric(as.character(.x))),
    max_interval = max(c(p1_interval, p2_interval))
  ) %>%
  group_by(p1_interval, p2_interval, max_interval) %>%
  summarize(
    n = n(),
    mean_both_endorsed = round(mean(n_both_endorsed)),
  ) %>%
  mutate(
    mbe_as_pct = mean_both_endorsed / max_interval
  )

ggplot(cdi_parent_overlap_bins, aes(x = p1_interval, y = p2_interval,
                                    fill = mbe_as_pct)) +
  geom_tile(aes(alpha =  n)) +
  geom_text(aes(label = mean_both_endorsed), size = 3.5) +
  scale_x_continuous(breaks = seq(0, 680, by = 68) + 34, minor_breaks = NULL,
                     labels =  seq(0, 680, by = 68)) +
  scale_y_continuous(breaks = seq(0, 680, by = 68) + 34, minor_breaks = NULL,
                     labels =  seq(0, 680, by = 68)) +
  scale_fill_gradient2(limits = c(0, 1), midpoint = 0.5) +
  scale_alpha_continuous(range = c(1/3, 1)) +
  theme_bw() +
  coord_equal() +
  labs(x = "Parent 1", y = "Parent 2", fill = "% overlapping\nwords")

# ggplot(cdi_parent_overlap, aes(x = p1_pct, y = p2_pct, color = age)) +
#   geom_hex(bins = 20) +
#   scale_x_continuous(limits = c(0, 1)) +
#   scale_y_continuous(limits = c(0, 1)) +
#   viridis::scale_color_viridis() +
#   coord_equal() +
#   theme_bw()
#
# ggplot(cdi_parent_overlap, aes(x = n_both_endorsed, y = disagreement)) +
#   geom_point()

cdi_items_nested <- cdi_items_wide %>%
  group_by(syntax, category, definition) %>%
  nest() %>%
  mutate(
    agr_rate = map_dbl(data, ~sum(.x$same, na.rm = TRUE)) / map_int(data, nrow),
    definition2 = str_remove_all(definition, " ") %>%
      str_remove("'") %>%
      str_remove("[(].*[)]")%>%
      str_remove_all("/.*"),
  ) %>%
  left_join(
    select(wb, category, definition2, prod_rate, expected_agr, freq_log10),
  )

ggplot(cdi_items_nested, aes(x = agr_rate)) +
  geom_histogram(binwidth = 0.025) +
  theme_bw()

cdi_items_by_category <- cdi_items_nested %>%
  group_by(category, syntax) %>%
  summarize(
    n = n(),
    mean_agr_rate = mean(agr_rate),
    sd_agr_rate = sd(agr_rate)
  ) %>%
  mutate(
    se_agr_rate = sd_agr_rate / sqrt(n)
  )

ggplot(cdi_items_by_category, aes(x = category, y = mean_agr_rate)) +
  geom_pointrange(aes(ymin = mean_agr_rate - se_agr_rate,
                      ymax = mean_agr_rate + se_agr_rate,
                      color = syntax, size = n)) +
  scale_size(range = c(0.25, 1.25)) +
  scale_y_continuous(limits = c(0.7, 0.9)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lm(mean_agr_rate ~ n + syntax, data = cdi_items_by_category) %>%
  summary()

freq_model <- lm(agr_rate ~ freq_log10*syntax, data = cdi_items_nested)
summary(freq_model)

# range(cdi_items_nested$freq_log10[cdi_items_nested$syntax == "content"], na.rm = TRUE)
# [1] -7.331893 -2.575176
predict(freq_model,
        newdata = tibble(freq_log10 = c(-2, -3, -4, -5), syntax = "content"))

# range(cdi_items_nested$freq_log10[cdi_items_nested$syntax == "func"], na.rm = TRUE)
# [1] -5.452863 -1.405184
predict(freq_model,
        newdata = tibble(freq_log10 = c(-2, -3, -4, -5), syntax = "func"))

ggplot(cdi_items_nested, aes(x = freq_log10, y = agr_rate, color = syntax)) +
  geom_point() +
  scale_x_reverse() +
  geom_smooth(method = "lm") +
  theme_bw()

expected <- lm(agr_rate ~ expected_agr + freq_log10*syntax,
               data = na.omit(cdi_items_nested))

rzd <- broom::augment(expected, data = na.omit(cdi_items_nested)) %>%
  select(-data) %>%
  arrange(desc(.std.resid))

interesting_rzd <- rzd %>%
  filter(
    abs(.std.resid) > 3
  )


ggplot(cdi_items_nested, aes(x = expected_agr, y = agr_rate)) +
  geom_point(aes(color = freq_log10), alpha = 0.75) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_y_continuous(limits = c(0.5, 1)) +
  scale_color_viridis() +
  facet_wrap(vars(syntax)) +
  theme_bw()

cdi_categories <- cdi_items_nested %>%
  group_by(category, syntax) %>%
  summarize(
    n = n(),
    mean_agr_rate = mean(agr_rate),
    mean_eer = mean(expected_agr),
    mean_freq_log10 = mean(freq_log10, na.rm = TRUE)
  )

ggplot(cdi_categories, aes(x = mean_eer, y = mean_agr_rate)) +
  geom_point(aes(size = n, color = mean_freq_log10)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggrepel::geom_label_repel(aes(label = category)) +
  scale_color_viridis() +
  theme_bw() +
  labs(x = "Mean EER", y = "Mean Agreement rate",
       color = bquote(Mean~log[10](f)))

# vrrRSB ====

reverse_items <- c(14, 17, 27, 35, 37:39, 42, 44:46, 48)

vrrsb0 <- read_tsv("data/IPR_vrRSB-250127.tsv", show_col_types = FALSE) %>%
  filter(
    PSCID %in% included
  ) %>%
  select(Visit_label, PSCID, starts_with("q_")) %>%
  pivot_longer(starts_with("q_")) %>%
  separate_wider_delim(name, delim = "_", names = c(NA, "q_id", "q_desc"),
                       too_many = "merge") %>%
  mutate(
    q_id = as.numeric(q_id),

    value_numeric = case_match(
      value,
      # VRS label, all others
      c("not_at_all", "not_true")  ~ 3,
      c("somewhat", "sometimes")   ~ 2,
      c("about_the_same", "often") ~ 1,
      c("more", "almost_always")   ~ 0
    ),

    value_numeric = if_else(q_id %in% reverse_items,
                            (-1 * value_numeric) + 3,
                            value_numeric),

    value_factor = ordered(value_numeric)

  ) %>%
  filter(
    q_id < 50
  ) %>%
  rename(
    id = PSCID,
    p1p2 = Visit_label
  ) %>%
  arrange(id, p1p2, q_id)

vrrsb <- vrrsb0 %>%
  pivot_wider(id_cols = c(id, q_id, q_desc),
              names_from = p1p2, values_from = value_factor) %>%
  group_by(q_id, q_desc) %>%
  nest() %>%
  mutate(
    confusion = map(data, ~table(.x$iprParent1, .x$iprParent2)),
    kappa = map(confusion,
                ~DescTools::CohenKappa(.x, weights = "E", conf.level = .95))
  )

vrrsb_kappa <- vrrsb %>%
  select(-data, -confusion) %>%
  unnest(kappa) %>%
  mutate(
    value = c("k", "lwr.ci", "upr.ci"),
    label = str_replace_all(q_desc, "_", " "),
    across(where(is.numeric), ~round(.x, 3)),
    vr = q_id <= 13
  ) %>%
  pivot_wider(names_from = value, values_from = kappa)

# Are VR items different from non-VR items?
t.test(vrrsb_kappa$k[vrrsb_kappa$vr], vrrsb_kappa$k[!vrrsb_kappa$vr])

png("plots/vrrsb_kappa.png", width = 6.5, height = 6.5, units = "in", res = 300)

ggplot(vrrsb_kappa, aes(x = k, y = q_id)) +
  geom_pointrange(aes(xmin = lwr.ci, xmax = upr.ci)) +
  geom_vline(xintercept = 0, color = "red") +
  scale_x_continuous(limits = c(NA, 1)) +
  scale_y_continuous(labels = vrrsb_kappa$label,
                     breaks = 1:49, minor_breaks = NULL,
                     trans = "reverse") +
  theme_bw() +
  labs(x = "Weighted kappa [95% CI]", y = "Question")

dev.off()

vrrsb_kappa_ordered <- vrrsb_kappa %>%
  arrange(desc(k))\
