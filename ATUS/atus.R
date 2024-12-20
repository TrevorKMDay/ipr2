library(tidyverse)

setwd("C:/Users/Trevor/Desktop/ipr/ATUS")

act <- read_csv("atusact_2020.dat", show_col_types = FALSE)
rost <- read_csv("atusrost_2020.dat", show_col_types = FALSE)

resp <- read_csv("atusresp_2020.dat", show_col_types = FALSE) %>%
  rename(
    total_time_w_child = TRTOHHCHILD
  )

sum <- read_csv("atussum-2020/atussum_2020.dat", show_col_types = FALSE) %>%
  rename(
    labor_force = TELFS,
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
    # No None status
    respondent_employ = case_when(
                            respondent_employ == 1 ~ "full",
                            respondent_employ == 2 ~ "part",
                            respondent_employ == -1 ~ NA_character_
                          ),
    partner_employ = case_when(
                            partner_employ == 1 ~ "full",
                            partner_employ == 2 ~ "part",
                            partner_employ == -1 ~ NA_character_
                          ),
    labor_force = case_match(
      labor_force,
      1:2 ~ "employed",
      3:5 ~ "unemployed"
    )
  ) %>%
  select(TUCASEID, starts_with("respondent"), partner_employ, labor_force)

atus <- sum %>%
  select(TUCASEID, starts_with("respondent"), partner_employ, labor_force) %>%
  left_join(
    select(resp, TUCASEID, total_time_w_child)
  ) %>%
  mutate(
    TUCASEID = as.character(TUCASEID)
  )

atus2 <- atus %>%
  mutate(
    employment = case_when(
      !is.na(respondent_employ) ~ respondent_employ,
      !is.na(respondent_student) ~ paste0("student_", respondent_student),
      labor_force == "unemployed" ~ "unemployed"
    )
  ) %>%
  mutate(
    female = respondent_sex == "F"
  )

ggplot(atus2, aes(x = employment,
                  y = total_time_w_child, fill = respondent_sex)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c("full", "part"),
                                           c("F", "M"))) +
  theme_bw()

png("atus_summary.png", width = 6, height = 4, units = "in", res = 300)

ggplot(atus2, aes(x = respondent_sex, y = total_time_w_child)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c("F", "M"))) +
  scale_y_continuous(limits = c(0, 1200)) +
  facet_wrap(vars(employment)) +
  theme_bw() +
  labs(x = "Respondent sex", y = "Total time w/ child (minutes)")

dev.off()

datus_rzd <- umx::umx_residualize("total_time_w_child",
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

lm_sexonly <- lm(total_time_w_child ~ female, data = atus2)

lm_sex_employ <- lm(total_time_w_child ~ female + employment,
                    data = atus2)

lm_sex_employ_int <- lm(total_time_w_child ~ female*employment,
                        data = atus2)

anova(lm_sexonly, lm_sex_employ, lm_sex_employ_int)
