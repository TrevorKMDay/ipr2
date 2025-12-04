library(tidyverse)

qualtrics <- readxl::read_xlsx("analysis/vrrsb_doublecheck.xlsx") %>%
  select(studyID, p1_or_p2, matches("Q[47]"))

labels <- as_vector(qualtrics[1, -c(1, 2)]) %>%
  str_remove("In comparison to the child in the video, - ") %>%
  str_remove("For each question, please check the box that best describes your child's behavior over the last month. Note that the headings for the answers are different from Section I: - ")


reverse_items <- c(1:13, 14, 17, 27, 35, 37:39, 42, 44:46, 48)

qualtrics2 <- qualtrics %>%
  filter(
    studyID != "Study ID:"
  ) %>%
  select(-Q7_7) %>%
  pivot_longer(-c(studyID, p1_or_p2)) %>%
  mutate(

    label = rep(labels, length.out = nrow(.)),
    q_id = rep(1:49, length.out = nrow(.)),

    value = case_match(
      value,
      "About the same as the child in the video" ~ "about_the_same",
      "More than the child in the video" ~ "more",
      "Almost Always True" ~ "almost_always",
      "Not True" ~ "not_true",
      "Sometimes True" ~ "sometimes",
      "Often True" ~ "often"
    ),


    value_numeric = case_match(
      value,
      # VRS label, all others
      c("not_at_all", "not_true")  ~ 0,
      c("somewhat", "sometimes")   ~ 1,
      c("about_the_same", "often") ~ 2,
      c("more", "almost_always")   ~ 3
    ),

    value_numeric = if_else(q_id %in% reverse_items,
                            (-1 * value_numeric) + 3,
                            value_numeric),

    value_factor = ordered(value_numeric)

  )

# max=99, 33 items
sc_items <- c(1:13, 14:15, 17, 19, 21, 25:28, 30, 32:33, 35, 36:40, 42, 49)

# max=33, 11 items
rrb_items <- c(16, 18, 20, 22:24, 29, 31, 34, 41, 47)

# max=132
rsb_items <- c(1:42, 47, 49)

qualtrics3 <- qualtrics2 %>%
  group_by(p1_or_p2) %>%
  nest() %>%
  mutate(

    # 49 = ap1

    VRS = map_int(data, ~sum(.x$value_numeric[.x$q_id %in% 1:13])),
    SC = map_int(data, ~sum(.x$value_numeric[.x$q_id %in% sc_items])),
    RRB = map_int(data, ~sum(.x$value_numeric[.x$q_id %in% rrb_items])),
    RSB = map_int(data, ~sum(.x$value_numeric[.x$q_id %in% rsb_items])),

  )

loris <- read_rds("analysis/vrrsb_wide_included.rds")

# download from loris

vrrsb0 <- read_tsv("data/IPR_vrRSB-250127.tsv", show_col_types = FALSE) %>%
  filter(
    PSCID == "IPR100100"
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
      c("not_at_all", "not_true")  ~ 0,
      c("somewhat", "sometimes")   ~ 1,
      c("about_the_same", "often") ~ 2,
      c("more", "almost_always")   ~ 3
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

side_by_side <- vrrsb0 %>%
  select(p1p2, q_id, value, value_numeric) %>%
  mutate(
    p1p2 = str_replace(p1p2, "iprParent", "Parent ")
  ) %>%
  left_join(qualtrics2, by = join_by(p1p2 == p1_or_p2, q_id))
