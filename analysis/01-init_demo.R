library(tidyverse)
library(here)

select <- dplyr::select

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
         matches("sibling"),
         household_income, subject_race, subject_ethnicity) %>%
  arrange(id, p1p2)

demoC <- read_tsv(here("data", "IPR_demographics_comments-250127.tsv"),
                   show_col_types = FALSE) %>%
  mutate(
    result = if_else(str_detect(Comment, "Father"), "male", "female")
  ) %>%
  select(CandID, result)

## Quick stats =====

demo_hh_re <- demo1 %>%
  select(id, p1p2, sex, household_income, subject_race, subject_ethnicity) %>%
  filter(
    p1p2 == "iprParent1"
  )

table(demo_hh_re$sex, useNA = "a")
table(demo_hh_re$household_income, useNA = "a")

table(demo_hh_re$subject_race, demo_hh_re$subject_ethnicity, useNA = "a")

# Sex ====

clean_gender <- function(sex, gender) {

  # This function takes what people put in the free-entry field and recodes it

  gender1 <- str_to_lower(gender) %>%
    str_trim() %>%

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
  select(CandID, id, p1p2, matches("parent._(sex|gender)$")) %>%
  pivot_longer(-c(CandID, id, p1p2)) %>%
  na.omit() %>%
  separate_wider_delim(name, delim = "_", names = c(NA, "name")) %>%
  pivot_wider() %>%
  mutate(

    # Clean gender
    gender2 = map2_chr(sex, gender, clean_gender),

    # Reassign gender categories
    gender2 = if_else(sex == "male" & gender == "Female",
                      "transwoman", gender2,
                      missing = gender2),


  )

# Check for missing data
demo1_sex[!complete.cases(demo1_sex), ]
table(demo1_sex$sex, demo1_sex$gender2, useNA = "a")

demo1_gender_wide <- demo1_sex %>%
  pivot_wider(id_cols = id, names_from = p1p2, values_from = gender2)

table(demo1_gender_wide[, c("iprParent1", "iprParent2")], useNA = "a")

# Remove original gender field for clarity
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

# Check for median imputation based on partner education - but not that helpful
all_eds_paired %>%
  group_by(ed.x) %>%
  nest() %>%
  mutate(
    median = map_dbl(data, ~median(.x$ed.y_y, na.rm = TRUE))
  )

demo1_sexed <- left_join(demo1_sex2, demo1_ed, by = join_by(id, p1p2)) %>%
  select(id, p1p2, sex, gender2, ed, ed_y)

demo1_sexed[!complete.cases(demo1_sexed), ]

# Siblings ====

child_ages <- read_rds(here("analysis", "cdi_wide_included.rds")) %>%
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
  select(-matches("sibling._age")) %>%
  rename(
    child_age = age
  ) %>%
  mutate(
    across(ends_with("_DoB"), ~round(interval(.x, Date_taken) / years(1), 3))
  ) %>%
  select_all(~str_replace(., "DoB", "age")) %>%
  select(id, child_age, starts_with("sibling"), -contains("comments")) %>%
  pivot_longer(-c(id, child_age), names_pattern = "(sibling[1-7])_(.*)",
               names_to = c("sibling", ".value")) %>%
  filter(
    sex != "not_answered" & type != "not_answered" & home != "not_answered"
  )

# Group by child and compare to all their siblings
demo1_sibs_older <- demo1_sibs %>%
  filter(
    id %in% inclusions
  ) %>%
  mutate(
    child_age_y = round(child_age / 12, 3),
    sib_is_older = age > child_age_y
  ) %>%
  group_by(id) %>%
  nest() %>%
  mutate(
    number_of_sibs = map_int(data, ~sum(!is.na(.x$sibling))),
    any_older_sibs = if_else(number_of_sibs == 0,
                             FALSE,
                             map_lgl(data, ~any(.x$sib_is_older))),
    child_birth_order = if_else(any_older_sibs, "later_born", "first_born",
                                missing = "missing")
  )

# Check for missing data
missing_sibs <- demo1_sibs_older %>%
  filter(
    is.na(child_birth_order)
  ) %>%
  unnest(data)

# Connect birth order to child sex and write out
demo1_sibs_final <- select(demo1, id, sex) %>%
  left_join(
    demo1_sibs_older,
    join_by(id)
  ) %>%
  rename(
    child_sex = sex
  ) %>%
  select(id, child_sex, child_birth_order) %>%
  distinct() %>%
  mutate(
    # Anyone missing here has no siblings, and so is the first born
    child_birth_order = replace_na(child_birth_order, "first_born") %>%
      replace(., . == "missing", NA)
  )

write_rds(demo1_sibs_final, "demo_child.rds")

# Demo2 ====

demo2 <- read_delim(here("data", "IPR_demographics_extended-250204.tsv"),
                    delim = "\t", show_col_types = FALSE) %>%
  rename(
    id = PSCID,
    p1p2 = Visit_label,
    me_TSWC = q1_TSWC,
    partner_TSWC = q2_TSWC,
    neither_TSWC = q3_TSWC,
    both_TSWC = q4_TSWC
  ) %>%
  select(id, p1p2, ends_with("TSWC"), contains("CGSS")) %>%
  filter(
    id %in% inclusions
  ) %>%
  mutate(
    across(ends_with("TSWC"), as.numeric),
  )


assign_pcg <- function(both, me) {

  if (both[1] > both[2]) {
    result <- c("pcg", "scg")
  } else if (both[2] > both[1]) {
    result <- c("scg", "pcg")
  } else if (both[1] == both[2]) {

    if (me[1] > me[2]) {
      result <- c("pcg", "scg")
    } else if (me[2] > me[1]) {
      result <- c("scg", "pcg")
    } else if (me[1] == me[2]) {
      result <- c(NA, NA)
    }

  }

  return(result)

}

demo2_tswc <- demo2 %>%
  select(-ends_with("CGSS")) %>%
  mutate(
    meboth_TSWC = (me_TSWC + both_TSWC) / 2
  ) %>%
  group_by(id) %>%
  mutate(
    pcg = assign_pcg(meboth_TSWC, me_TSWC)
  ) %>%
  left_join(demo1_sex2, join_by(id, p1p2)) %>%
  mutate(
    pcg = case_when(
      # One of the lesbian couples rated each other identically, just
      #   assign a PCG here
      id == "IPR314271" & p1p2 == "iprParent1" ~ "pcg",
      is.na(pcg) & gender2 == "man" ~ "pcg",
      is.na(pcg) & gender2 == "woman" ~ "scg",
      .default = pcg
    )
  )

# demo2_tswc %>%
#   group_by(id, pcg) %>%
#   summarize(
#     n = n()
#   ) %>%
#   filter(
#     n > 1
#   )

# Check for missing data
demo2_tswc[!complete.cases(demo2_tswc), ] %>%
  arrange(id)

table(demo2_tswc$pcg, demo2_tswc$gender2, useNA = "a")

tswc2 <- demo2_tswc %>%
  ungroup() %>%
  select(id, pcg, ends_with("TSWC")) %>%
  select_all(~str_remove(., "_TSWC")) %>%
  mutate(
    partnerboth = partner + both
  ) %>%
  pivot_wider(id_cols = id, names_from = pcg,
              values_from = c(me, partner, neither, both, meboth, partnerboth))

library(Hmisc)
library(corrplot)

tswc_corrs <- tswc2 %>%
  select(-id) %>%
  as.matrix() %>%
  rcorr()

corrplot(tswc_corrs$r, method = "color",
         addCoef.col = "black", number.digits = 2,
         p.mat = tswc_corrs$P, insig = "blank")

demo2_pcg <- demo2_tswc %>%
  select(id, p1p2, pcg, meboth_TSWC)

# Save final results ====

parents_demo <- left_join(demo1_sexed, demo2_pcg, join_by(id, p1p2)) %>%
  rename(
    p_sex = sex,
    p_gender = gender2,
    p_ed = ed,
    p_edy = ed_y,
    p_pcg = pcg,
    p_meboth_tswc = meboth_TSWC
  )

parents_demo[!complete.cases(parents_demo), ]
write_rds(parents_demo, here("analysis", "demo_parents.rds"))

