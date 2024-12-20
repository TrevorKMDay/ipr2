library(tidyverse)
library(here)

# Set up Qualtrics access ====
source(here("get_data_qualtrics", "00-qualtrics_setup.R"))

# Invites ====

# Total invited parents = nrow(invited_parents)
invited_parents <- invite_sheets %>%
  select(Recipient, PARENT, CHILD) %>%
  distinct() %>%
  arrange(Recipient)

# Screeners ====

get_survey <- function(id) {

  survey <- fetch_survey(id) %>%
    labelled::remove_labels() %>%
    mutate(
      how_find = as.character(how_find)
    )

  return(survey)

}

screener_ids <- sapply(id[1:6], unname)

screeners0 <- tibble(screener = names(id)[1:6],
                     id = screener_ids) %>%
  mutate(
    data = map(id, get_survey)
  )

screeners <- screeners0 %>%
  unnest(data) %>%
  select(-id) %>%
  filter(
    # Testing (n=3)
    Status != "Survey Preview"
  )

ineligible_noscg <- sum(screeners$scg_yesno == "No", na.rm = TRUE)
ineligible_2langs <- sum(screeners$lg_other_EN == "Yes", na.rm = TRUE)
ineligible_devdisorder <- sum(screeners$dev_disorder == "Yes", na.rm = TRUE)

screener_eligible <- screeners %>%
  filter(
    scg_yesno == "Yes",
    lg_other_EN == "No",
    dev_disorder == "No"
  )

# Consent ====

consent <- fetch_survey(id$consent, add_var_labels = FALSE)

consent2 <- consent %>%
  select(NewID, p1p2) %>%
  na.omit() %>%
  distinct() %>%
  mutate(
    p1p2 = str_remove(p1p2, "p") %>%
      as.numeric(),
    consented = TRUE
  ) %>%
  pivot_wider(names_from = p1p2, values_from = consented,
              names_prefix = "p") %>%
  mutate(
    NewID = as.character(NewID),
    across(c(p1, p2), ~replace_na(.x, FALSE)),
    both = p1 & p2
  ) %>%
  arrange(NewID)

total_consent <- sum(consent2$both)
total_consent / (1056 - 99)
