library(tidyverse)
library(here)

select <- dplyr::select

inclusions <- read_rds(here("analysis", "vrrsb_wide_included.rds"))$id

not_all_na <- function(x) any(!is.na(x))

# Demographics ====

demo1 <- read_tsv(here("data", "IPR_demographics-250204.tsv"),
                  show_col_types = FALSE, na = "NULL") %>%
  rename(
    id = PSCID,
    p1p2 = Visit_label,
    age = Candidate_Age
  ) %>%
  filter(
    id %in% inclusions,
    !str_detect("Testing", CommentID)
  ) %>%
  arrange(id, p1p2) %>%
  select(where(not_all_na))

# Nobody reported 5+ siblings, so drop those

demo_child <- demo1 %>%
  select(-c(id, CommentID, UserID, Examiner, Data_entry_completion_status),
         -ends_with("status"), -starts_with("parent"), -Window_Difference,
         -matches("sibling[567]_"), -starts_with("childcare")) %>%
  filter(
    p1p2 == "iprParent1"
  ) %>%
  mutate(

    screen_time_hrs = case_match(screen_time_hrs,
                                 "0_1" ~ "<1",
                                 "1_2" ~ "1-2",
                                 "2-5" ~ "2-5",
                                 "5_or_more" ~ ">5"),

    sibling1_age_mo = round(interval(sibling1_DoB, Testdate) / months(1), 1),
    sibling2_age_mo = round(interval(sibling2_DoB, Testdate) / months(1), 1),
    sibling3_age_mo = round(interval(sibling3_DoB, Testdate) / months(1), 1),
    sibling4_age_mo = round(interval(sibling4_DoB, Testdate) / months(1), 1),

  ) %>%
  select(-matches("sibling[1-6]_age$"), -ends_with("DoB"), -Testdate,
         -Date_taken) %>%
  rename(
    child_age_mo = age,
    child_sex = sex
  )


demo_parents <- demo1 %>%
  select(p1p2, CandID, Testdate, starts_with("parent"),
         -ends_with("status")) %>%
  mutate(

    parent1_age_y = round(interval(parent1_DoB, Testdate) / years(1)),
    parent2_age_y = round(interval(parent2_DoB, Testdate) / years(1))

  ) %>%
  select(-Testdate, -ends_with("DoB")) %>%
  pivot_longer(-c(p1p2, CandID), values_transform = as.character,
               names_pattern = "([^_]*)_(.*)", names_to = c("parent", "name")) %>%
  filter(
    (p1p2 == "iprParent1" & parent == "parent1") |
      (p1p2 == "iprParent2" & parent == "parent2")
  ) %>%
  pivot_wider(names_prefix = "parent_") %>%
  mutate(
    parent_age_y = as.numeric(parent_age_y)
  ) %>%
  select(-parent)

demo_final <- left_join(demo_child, demo_parents)

write_csv(demo_final, "shared/demographics_shared.csv")

# CDI ====

model <- read_csv("analysis/wordbank_model.csv")

model1 <- model %>%
  filter(
    data_id == data_id[1]
  ) %>%
  select(item_kind, category, item_id, item_definition, value)

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
  arrange(id, p1p2)

ws_summary <- ws0 %>%
  select(p1p2, CandID, age, ends_with("number"), ends_with("percentile"),
         combining_score)  %>%
  mutate(
    across(c(age, ends_with("number"), ends_with("percentile")), as.numeric)
  )

categories <- unique(model1$category)

ws_items <- ws0 %>%
  select(p1p2, CandID, starts_with("I"), combining, -ends_with("status"),
         -ends_with("number"), -ends_with("score"), -ends_with("words"),
         -ends_with("morphemes"), -id, -II_D_alltext) %>%
  rename(
    # Trick this into being ordered correctly
    II_E_0 = combining
  ) %>%
  pivot_longer(-c(p1p2, CandID)) %>%
  mutate(

    item_kind = case_when(
      str_detect(name, "^I_A_") ~ "word",
      str_detect(name, "^I_B_") ~ "how_use_words",
      str_detect(name, "^II_A_") ~ "word_endings",
      str_detect(name, "^II_B_[1-5]") ~ "word_forms_nouns",
      str_detect(name, "^II_B_") ~ "word_forms_verbs",
      str_detect(name, "^II_C_[1-5]") ~ "word_endings_nouns",
      str_detect(name, "^II_C_([1-9]|1[0-4])") ~ "word_endings_verbs",
      name == "II_E_0" ~ "combine",
      str_detect(name, "^II_E_") ~ "complexity"
     ),

    value = case_when(
      value == "NULL" ~ NA,
      item_kind == "word" & value == "says" ~ "produces",
      str_detect(item_kind, "^word_(forms|endings)_") & value == "1" ~
        "produces",
      value == "less_complex" ~ "simple",
      value == "more_complex" ~ "complex",
      value == "not_yet" ~ "not yet",
      TRUE ~ value,
    ),

    category_index = if_else(str_detect(name, "^I_A_"),
                             as.numeric(str_remove_all(name, "^I_A_|_[0-9]*$")),
                             NA),

    category = categories[category_index]

  ) %>%
  filter(
    !str_detect(name, "^II_D")
  )

# TO DO: Link item IDs for definitions

ws_items %>%
  group_by(CandID, p1p2) %>%
  dplyr::summarize(
    n = n()
  )

ws_items2 <- ws_items %>%
  group_by(p1p2, CandID) %>%
  mutate(
    item_id = paste0("item_", row_number())
  ) %>%
  left_join(
    select(model1, item_id, item_definition)
  ) %>%
  select(p1p2, CandID, item_kind, category, item_id, item_definition, value) %>%
  arrange(CandID, p1p2)

write_csv(ws_items2, "shared/cdi_shared.csv")

# vrRSB ====

vrrsb <- read_tsv("data/IPR_vrRSB-250127.tsv", show_col_types = FALSE) %>%
  select(Visit_label, CandID, ends_with("score"), ends_with("items"),
         starts_with("q_"), -ends_with("status"), -q_51_sophisticated_sentence,
         -q_50_num_words_nda) %>%
  filter(
    CandID %in% demo_final$CandID
  ) %>%
  rename(
    p1p2 = Visit_label,
    VRS = video_reference_score,
    RSB = RSB_total_score,
    SCS = social_communicative_items,
    RRS = restricted_repetitive_items
  ) %>%
  mutate(
    across(c(VRS, RSB, SCS, RRS, q_50_num_words), as.numeric)
  ) %>%
  arrange(CandID, p1p2)

write_csv(vrrsb, "shared/vrrsb_shared.csv")

# BAPQ ====

bapq <- read_tsv("data/IPR_BAPQ-250128.tsv", show_col_types = FALSE) %>%
  select(Visit_label, CandID, ends_with("self"), ends_with("partner")) %>%
  filter(
    CandID %in% demo_final$CandID
  ) %>%
  mutate(
    across(-c(Visit_label, CandID), as.numeric)
  ) %>%
  pivot_longer(-c(Visit_label, CandID)) %>%
  mutate(
    target = str_extract(name, "(self|partner)$"),
    name = str_remove(name, "_(self|partner)$")
  ) %>%
  pivot_wider() %>%
  rename(
    plang_score = pragmatic_language_score
  ) %>%
  arrange(CandID, p1p2, target)
