library(tidyverse)
library(ggVennDiagram)

setwd("C:/Users/Trevor/Desktop/ipr/abstracts")

files <- list.files("data/2024", pattern = "*.txt", full.names = TRUE)

abstracts0 <- tibble(f = files) %>%
  mutate(
    data = map(f, read_tsv, show_col_types = FALSE,
               col_types = cols(PY = col_number(),
                                .default = col_character()))
  )

abstracts <- abstracts0 %>%
  unnest(data) %>%
  select(AU, SO, PY, DI, AB) %>%
  rename(
    publication = SO,
    abstract = AB
  ) %>%
  filter(
    !is.na(abstract)
  ) %>%
  mutate(
    publication = str_to_title(publication)
  )

strings <- abstracts %>%
  mutate(
    abstract = tolower(abstract),
    moms     = str_detect(abstract, "mother|mom"),
    dads     = str_detect(abstract, "father|dad"),
    cg_parent = str_detect(abstract, "caregiver|parent"),
  )

strings %>%
  group_by(publication) %>%
  summarize(
    old = min(PY),
    new = max(PY)
  )

string_summary <- strings %>%
  select(-abstract) %>%
  pivot_longer(c(moms, dads, cg_parent)) %>%
  group_by(publication, name) %>%
  summarize(
    n = n(),
    value = sum(value)
  ) %>%
  mutate(
    p = value / n
  )

total_moms <- sum(strings$moms) / nrow(strings)
total_dads <- sum(strings$dads) / nrow(strings)
total_cgp <- sum(strings$cg_parent) / nrow(strings)



ggplot(string_summary, aes(x = name, y = p, fill = publication)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_discrete(labels = c("Caregiver/Parent", "Dad", "Mom")) +
  labs(x = "Caregiver", y = "Proportion", color = "Publication") +
  theme_bw()

range(abstracts$PY)

ggplot(abstracts, aes(PY, fill = publication)) +
  geom_histogram(binwidth = 1) +
  theme_bw()

# Venn diagram

venn_list = list(moms = strings$DI[strings$moms],
                 dads = strings$DI[strings$dads],
                 cgp = strings$DI[strings$cg_parent])

png("abstracts_upsetplot_2024.png", width = 6, height = 4, units = "in",
    res = 300)

ggVennDiagram(venn_list,
              category.names = c("Moms", "Dads", "CG/Parent"),
              force_upset = TRUE)

dev.off()
