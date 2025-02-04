library(tidyverse)
library(here)

exclusions <- read_rds(here("analysis", "exclusions.rds"))
inclusions <- read_rds(here("analysis", "vrrsb_wide_included.rds"))$id

# Read in data ====

demo1 <- read_tsv(here("data", "IPR_demographics-250204.tsv"),
                  show_col_types = FALSE, na = "NULL") %>%
  rename(
    id = PSCID,
    p1p2 = Visit_label,
    age = Candidate_Age
  ) %>%
  filter(
    !(id %in% exclusions),
    id %in% inclusions,
    !str_detect("Testing", CommentID)
  ) %>%
  select(CandID, id, p1p2, age, sex, Date_taken,
         matches("parent[12]_(gender|sex|relationship)"),
         ends_with("education"),
         matches("sibling")) %>%
  arrange(id, p1p2)

demoC <- read_tsv(here("data", "IPR_demographics_comments-250127.tsv"),
                   show_col_types = FALSE) %>%
  mutate(
    result = if_else(str_detect(Comment, "Father"), "male", "female")
  ) %>%
  select(CandID, result)

# Sex ====

clean_gender <- function(sex, gender) {

  gender1 <- str_to_lower(gender) %>%
    trimws() %>%
    str_remove_all("[ -]")

  gender2 <- case_match(

    gender1,

    # I'm just hardcoding everything people did instead of trying a messy
    #   regex

    c("cisfemale", "cisgenderfemale", "femalecisgendersheterosexual",
      "she/her", "woman", "female", "ciswoman", "f", "fema",
      "female(she/her/hers)", "female,she/her", "female/woman",
      "femaleafab") ~ "woman",

    c("cismale", "cisman", "he", "he/him", "he/his", "m", "make", "male",
      "man", "man/male/boy/dudeetc.") ~ "man",

    # Converting these to 'cis' to take gender identity from sex. Can only
    #   assume individuals listing sexualities identify as cis
    c("cis", "cisgender", "bisexual", "heterosexual", NA_character_) ~ "cis",

    c("nonbinary", "unsure") ~ "nonbinary",

    .default = gender1

  )

  if (gender2 == "cis") {
    gender3 <- case_match(
      sex,
      "female" ~ "woman",
      "male" ~ "man",
      "not_answered" ~ NA_character_
    )
  } else {
    gender3 <- gender2
  }

  return(gender3)

}

demo1_sex <- demo1 %>%
  select(CandID, id, p1p2, matches("(sex|gender)$")) %>%
  pivot_longer(-c(CandID, id, p1p2)) %>%
  na.omit() %>%
  separate_wider_delim(name, delim = "_", names = c(NA, "name")) %>%
  pivot_wider() %>%
  mutate(
    gender2 = map2_chr(sex, gender, clean_gender)
  ) %>%
  arrange(id, gender2, desc(p1p2)) %>%
  group_by(id) %>%
  mutate(
    new_parent = c("p2", "p1")
  )

table(demo1_sex$sex, demo1_sex$gender2, useNA = "a")

table(demo1_sex$p1p2, demo1_sex$gender2)

missing_sex <- demo1_sex %>%
  filter(
    is.na(gender2)
  )

demo1_sex %>%
  filter(
    sex == "male",
    gender2 == "woman"
  )

demo1_gender_wide <- demo1_sex %>%
  pivot_wider(id_cols = id, names_from = p1p2, values_from = gender2)

table(demo1_gender_wide[, c("iprParent1", "iprParent2")])

demo1_sex2 <- demo1_sex %>%
  select(-gender)

# Get education values ====

educ_to_y <- function(educ) {

  ed_values <- tribble(
    ~educ, ~years,
    "some_high",    10,
    "high",         12,
    "some_college", 14,
    "college",      16,
    "some_grad",    18,
    "grad",         20
  )

  if (!is.na(educ))
    y <- ed_values$years[which(ed_values$educ == educ)]
  else
    y <- NA

  return(y)

}



demo1_ed <- demo1 %>%
  arrange(id) %>%
  select(id, p1p2, ends_with("education")) %>%
  pivot_longer(-c(id, p1p2), values_to = "ed") %>%
  select(-name) %>%
  filter(
    !is.na(ed)
  ) %>%
  mutate(
    ed = replace(ed, ed == "not_answered", NA) %>%
      ordered(., levels = c("some_high", "high", "some_college",
                            "college", "some_grad", "grad")),
    ed_y = Vectorize(educ_to_y)(ed)
  )

table(is.na(demo1_ed$ed))

# This is all educations paired with their other
all_eds_paired <- demo1_ed %>%
  select(id, p1p2, ed) %>%
  left_join(
    mutate(., p1p2 = if_else(p1p2 == "iprParent1", "iprParent2", "iprParent1")),
    by = join_by(id, p1p2)
  ) %>%
  mutate(
    ed.x_y = Vectorize(educ_to_y)(ed.x),
    ed.y_y = Vectorize(educ_to_y)(ed.y)
  ) %>%
  na.omit()

all_eds_paired %>%
  group_by(ed.x) %>%
  nest() %>%
  mutate(
    median = map_dbl(data, ~median(.x$ed.y_y, na.rm = TRUE))
  )

demo1_sexed <- left_join(demo1_sex2, demo1_ed, by = join_by(id, p1p2)) %>%
  select(id, p1p2, new_parent, sex, gender2, ed, ed_y)

# Siblings ====

child_ages <- read_rds("analysis/cdi_wide_included.rds") %>%
  select(id, age) %>%
  rename(
    child_age_mo = age
  ) %>%
  mutate(
    child_age_y = child_age_mo / 12
  )

demo1_sibs <- demo1 %>%
  filter(
    p1p2 == "iprParent1"
  ) %>%
  select(-ends_with("age")) %>%
  mutate(
    across(ends_with("_DoB"), ~round(interval(.x, Date_taken) / years(1), 3))
  ) %>%
  select_all(~str_replace(., "DoB", "age")) %>%
  select(id, starts_with("sibling"), -contains("comments")) %>%
  pivot_longer(-id, names_pattern = "(sibling[1-7])_(.*)",
               names_to = c("sibling", ".value")) %>%
  filter(
    sex != "not_answered" & type != "not_answered" & home != "not_answered"
  )

demo1_sibs_older <- child_ages %>%
  filter(
    id %in% inclusions
  ) %>%
  left_join(demo1_sibs, join_by(id)) %>%
  mutate(
    sib_is_older = age > child_age_y
  ) %>%
  group_by(id) %>%
  nest() %>%
  mutate(
    number_of_sibs = map_int(data, ~sum(!is.na(.x$sibling))),
    any_older_sibs = if_else(number_of_sibs == 0,
                             FALSE,
                             map_lgl(data, ~any(.x$sib_is_older))),
    child_birth_order = if_else(any_older_sibs, "later_born", "first_born")
  )

missing_sibs <- demo1_sibs_older %>%
  filter(
    is.na(child_birth_order)
  ) %>%
  unnest(data)

demo1_sibs_final <- demo1_sibs_older %>%
  left_join(
    select(demo1, id, sex)
  ) %>%
  select(id, sex, child_birth_order)

write_rds(demo1_sexed, "analysis/demo_parents.rds")
write_rds(demo1_sibs_final, "analysis/demo_child.rds")

# Demo2 ====

demo2 <- read_delim("data/IPR_demographics_extended-250127.tsv", delim = "\t",
                    show_col_types = FALSE)
