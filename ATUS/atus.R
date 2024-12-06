setwd("~/Insync/day00096@umn.edu/Google Drive/Research/parent-reliability/code/ATUS/")

library(tidyverse)

act <- read_csv("atusact_2020.dat")
rost <- read_csv("atusrost_2020.dat")


resp <- read_csv("atusresp_2020.dat") %>%
  rename(
    total_time_w_child = TRTOHHCHILD
  )

sum <- read_csv("atussum-2020/atussum_2020.dat") %>%
  rename(
    respondent_sex = TESEX,
    respondent_student = TESCHENR,
    number_of_children = TRCHILDNUM,
    youngest_child_age = TRYHHCHILD,
    respondent_employ  = TRDPFTPT,
    partner_employ     = TESPEMPNOT,
  ) %>%
  filter(
    number_of_children > 0,
    youngest_child_age > 1,
  ) %>%
  mutate(
    respondent_sex = if_else(respondent_sex == 1, "M", "F"),
    respondent_student = case_when(
                            respondent_student == 1 ~ "full",
                            respondent_student == 2 ~ "part",
                            respondent_student == -1 ~ NA_character_
                          ),
    respondent_employ = case_when(
                            respondent_employ == 1 ~ "full",
                            respondent_employ == 2 ~ "part",
                            respondent_employ == -1 ~ NA_character_
                          ),
    partner_employ = case_when(
                            partner_employ == 1 ~ "full",
                            partner_employ == 2 ~ "part",
                            partner_employ == -1 ~ NA_character_
                          )
  ) %>%
  select(TUCASEID, starts_with("respondent"), partner_employ)

atus <- sum %>%
  select(TUCASEID, starts_with("respondent"), partner_employ) %>%
  left_join(
    select(resp, TUCASEID, total_time_w_child)
  )

atus_rzd <- umx::umx_residualize("total_time_w_child", 
                                 c("respondent_employ", "respondent_student",
                                   "partner_employ"),
                                 data = atus)

ggplot(atus_rzd, aes(total_time_w_child, fill = respondent_sex)) +
  geom_density(alpha = 0.5)

atus_psex <- atus_rzd %>%
  group_by(respondent_sex) %>%
  summarize(
    m = mean(total_time_w_child, na.rm = TRUE),
    sd = sd(total_time_w_child, na.rm = TRUE)
  )

psex_d <- diff(atus_psex$m) / sqrt(sum(atus_psex$sd^2))
