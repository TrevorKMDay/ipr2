setwd("G:/My Drive/Research/parent-reliability/abstracts/")

library(tidyverse)
library(ggVennDiagram)

select <- dplyr::select

files <- list.files("data/", "*.tsv", full.names = TRUE)

load_data <- function(tsv) {

  columnnames <- c("info", "title", "authors", "author_info", "abstract", "CR",
                   "doi_pmid", "coi")

  data <- read.delim(tsv, header = FALSE)

  result <- data %>%
    setNames(columnnames[1:ncol(.)]) %>%
    separate(info, into = c("index", "info"), sep = ". ", extra = "merge",
             convert = TRUE) %>%
    mutate(
      year = str_extract(info, "^.*[.] [12][09][0-9][0-9]") %>%
              str_remove("^.*[.]") %>%
              as.numeric(),
      journal = str_extract(info, "^[^.]+") %>%
                  str_remove("[. ]")
    )

  return(result)

}

extract_text <- function(data) {

  data %>%
    select(journal, index, abstract) %>%
    mutate(
      abstract = tolower(abstract),
      moms     = str_detect(abstract, "mother"),
      dads     = str_detect(abstract, "father"),
      care     = str_detect(abstract, "caregiver"),
      parent   = str_detect(abstract, "parent")
    ) %>%
    return()

}

to_venn <- function(x, sub = 1:4) {

  x <- list(
          mom = x$index[x$moms],
          dad = x$index[x$dads],
          caregiver = x$index[x$care],
          parent = x$index[x$parent]
        )

  return(x[sub])

}

################################################################################
# Child Dev

data <- tibble(
    file = files
  ) %>%
  mutate(
    data = map(file, load_data),
    text = map(data, extract_text)
  )

text <- data %>%
  select(-data) %>%
  unnest(cols = c(text)) %>%
  select(-file) %>%
  mutate(
    index = as.numeric(index)
  ) %>%
  filter(!is.na(index))

summary <- text %>%
  filter(moms | dads) %>%
  group_by(journal, moms, dads) %>%
  dplyr::summarize(
    n = n()
  )

png("plots/venn1.png")

text %>%
  to_venn(1:4) %>%
  ggVennDiagram()

dev.off()

################################################################################

mom_dad_lut <- tibble(
  moms = c(TRUE, TRUE, FALSE),
  dads = c(TRUE, FALSE, TRUE),
  val  = c("Mom/Dad", "Mom only", "Dad only")
)

all2 <- left_join(text, mom_dad_lut)

journal_total <- all2 %>%
  group_by(journal) %>%
  summarize(total = n())

all2_total <- all2 %>%
  group_by(journal, val) %>%
  summarize(n = n()) %>%
  left_join(journal_total) %>%
  mutate(p = n / total) %>%
  filter(!is.na(val))

total <- all2_total %>%
  ungroup() %>%
  group_by(val) %>%
  summarise(
    n = sum(n),
    total = sum(total)
  ) %>%
  mutate(p = n / total)

png("plots/histo1.png", width = 800, 400)

ggplot(all2_total, aes(x = val, y = p * 100, fill = journal)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  labs(x = "Abstract content", y = "% papers mentioning mom|dad",
       fill = "Journal")

dev.off()
