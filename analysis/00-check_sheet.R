library(tidyverse)
library(here)
library(googlesheets4)

gs4_auth("day00096@umn.edu")
ss <- "1HDJsP-WV9ESajZNaixWyZ1gtvnyI2JmQUDgXWIueWI8"

exclusions <- read_sheet(ss, sheet = "Exclusions") %>%
  mutate(
    studyID = paste0("IPR", studyID)
  )

write_rds(exclusions$studyID, "exclusions.rds")

waves <- tibble(s = paste("Wave", 1:6)) %>%
  mutate(
    data = map(s, ~read_sheet(ss, sheet = .x, range = "A1:I",
                              col_types = "cllllllll"))
  )

all_waves <- waves %>%
  unnest(data) %>%
  mutate(
    ID = paste0("IPR", str_remove(ID, "P"))
  ) %>%
  filter(
    !(ID %in% exclusions$studyID)
  )

cdi_complete <- all_waves$ID[all_waves$CDI_P1 & all_waves$CDI_P2]

write_rds(cdi_complete, "cdi_complete_tracked.rds")
