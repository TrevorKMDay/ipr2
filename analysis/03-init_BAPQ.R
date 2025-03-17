library(tidyverse)
library(here)

# Load data ====

bapq0 <- read_tsv(here("data", "IPR_BAPQ-250128.tsv"), na = "NULL",
                  show_col_types = FALSE) %>%
  arrange(PSCID, Visit_label) %>%
  rename(
    id = PSCID,
    p1p2 = Visit_label
  ) %>%
  filter(
    # This should filter out missing IDs, if not there's an actual problem
    !str_detect(CommentID, "Testing")
  )

# Rreduce columns
bapq <- bapq0 %>%
  select(id, p1p2, contains("score")) %>%
  mutate(
    across(contains("score"), as.numeric),
    # p1p2 = if_else(p1p2 == "iprParent1", "p1", "p2")
  ) %>%
  select_all(~str_replace(., "pragmatic_language", "pl"))

# Reformat ====

bapq_wide <- bapq %>%
  pivot_longer(-c(id, p1p2)) %>%
  separate_wider_delim(name, delim = "_", names = c("score", NA, "target")) %>%
  pivot_wider(names_from = c(score, target, p1p2), values_from = "value")

write_rds(bapq_wide, "bapq_wide_all.rds")

bapq_included <- bapq_wide %>%
  filter(

  )

bapq_complete <- bapq_wide %>%
  filter(
    !is.na(total_self_p1),
    !is.na(total_partner_p1),
    !is.na(total_self_p2),
    !is.na(total_partner_p2)
  )

