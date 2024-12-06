library(tidyverse)

setwd("C:/Users/Trevor/Desktop/ipr/abstracts")

cols <- c("secc_name", "actual_name", "key",
          paste0("month", str_pad(c(1, 6, 15, 24, 37), 2, "left", "0")),
          "other", "month54",
          "K", paste0("grade", 1:8),
          "year15")

instruments <- readxl::read_xlsx("seccyd-measures.xlsx", col_names = cols,
                                 skip = 1)

stages <- colnames(select(instruments, starts_with("month"), "K",
                          starts_with("grade"), "year15"))

ages_lut <- tibble(
    stage = stages,
    age_y = c(c(1, 6, 15, 24, 37, 54) / 12, 5:13, 15)
  )

inst_long <- instruments %>%
  select(actual_name, starts_with("month"), "other", "K", starts_with("grade"),
         "year15") %>%
  pivot_longer(-actual_name, names_to = "stage", values_to = "reporter") %>%
  filter(
    !is.na(reporter)
  ) %>%
  mutate(
    reporters = str_remove_all(reporter, "[[:space:]]") %>%
                  str_remove_all("’") %>%
                  str_split("[,/]")
  ) %>%
  mutate(
    moms = sapply(reporters, function(x) "M" %in% x),
    dads = sapply(reporters, function(x) "F" %in% x),
    # cg_or_p = sapply(reporters, function(x) "C" %in% x | "P" %in% x)
  )

mom_dad_lut <- tibble(
  moms = c(TRUE, TRUE, FALSE),
  dads = c(TRUE, FALSE, TRUE),
  val  = c("Mom/Dad", "Mom only", "Dad only")
)

inst_summary <- inst_long %>%
  group_by(stage, moms, dads) %>%
  summarize(
    n = n()
  ) %>%
  left_join(ages_lut) %>%
  filter(
    moms|dads
  ) %>%
  left_join(mom_dad_lut) %>%
  arrange(age_y)

overall <- inst_summary %>%
  filter(age_y >= 54 / 12) %>%
  group_by(moms, dads) %>%
  summarise(sum = sum(n))

ggplot(inst_summary ,aes(x = age_y, y = n, fill = val)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = round(ages_lut$age_y, 1)) +
  theme_bw()

