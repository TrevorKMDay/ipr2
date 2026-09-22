library(tidyverse)
library(tidymodels)
library(here)

# Load data ===

all_data <- read_rds(here("analysis", "sem_data.rds"))

# Set up data frames ===

bld <- all_data %>%
  select(id, contains("bapq_total"), starts_with("vrrsb_total"),
         vrrsb_outlier, starts_with("cdi_total")) %>%
  mutate(
    vrrsb_p_minus_s = vrrsb_total_pcg - vrrsb_total_scg,
    cdi_p_minus_s = cdi_total_pcg - cdi_total_scg
  )

bld2 <- bld %>%
  mutate(
    vrrsb_mean = (vrrsb_total_pcg + vrrsb_total_scg) / 2,
    cdi_mean = (cdi_total_pcg + cdi_total_scg) / 2
  )

# Actual B-A plots ====

ggplot(bld2, aes(x = vrrsb_mean, y = vrrsb_p_minus_s)) +
  geom_point(aes(color = vrrsb_outlier), alpha = 0.5) +
  geom_smooth(aes(color = vrrsb_outlier), method = "lm") +
  geom_smooth(method = "lm", color = "red4") +
  scale_color_manual(values = c("black", "red")) +
  theme_bw() +
  labs(x = "Mean vrRSB score", y = "vrRSB PCG-SCG",
       color = "vrRSB outlier",
       caption = "Black regression excludes outlier, red includes it.",
       title = "vrRSB score")

ggsave(here("analysis", "ba_plots", "ba_vrrsb.png"), width = 6, height = 5)

lm1_vrrsb <- lm(vrrsb_p_minus_s ~ vrrsb_mean, data = bld2)

ggplot(bld2, aes(x = cdi_mean, y = cdi_p_minus_s)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Mean CDI score", y = "CDI PCG-SCG", title = "Total CDI words")

ggsave(here("analysis", "ba_plots", "ba_cdi.png"), width = 6, height = 5)

lm2_cdi <- lm(cdi_p_minus_s ~ cdi_mean, data = bld2)

models1 <- tribble(
    ~name, ~model,
    "vrrsb", lm1_vrrsb,
    "cdi", lm2_cdi
  ) %>%
  mutate(
    tidy = map(model, tidy)
  ) %>%
  select(-model) %>%
  unnest(tidy)

# The difference in vrRSB scores declines as the mean score increases;
#   there is no matching effect in the CDI data.

# Jed's request ====

bld3 <- bld2 %>%
  pivot_longer(starts_with("p_bapq_total"),
               names_sep = "_", names_to = c(NA, NA, NA, "cg", NA),
               values_to = "bapq_total_be")

ggplot(bld3, aes(x = bapq_total_be, y = vrrsb_p_minus_s)) +
  geom_point(aes(color = vrrsb_outlier), alpha = 0.5) +
  geom_smooth(method = "lm", fullrange = TRUE) +
  scale_x_continuous(limits = c(1, NA)) +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(vars(cg)) +
  theme_bw() +
  labs(x = "BAPQ Total (best-estimate)", y = "vrRSB PCG-SCG",
       color = "vrRSB outlier",
       title = "Difference in vrRSB against BAPQ total")

ggsave(here("analysis", "ba_plots", "ba_vrrsb_v_bapq.png"), width = 6,
       height = 5)

lm3_vrrsb2 <- lm(vrrsb_p_minus_s ~ bapq_total_be*cg, data = bld3)

ggplot(bld3, aes(x = bapq_total_be, y = cdi_p_minus_s)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", fullrange = TRUE) +
  scale_x_continuous(limits = c(1, NA)) +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(vars(cg)) +
  theme_bw() +
  labs(x = "BAPQ Total (best-estimate)", y = "CDI PCG-SCG",
       title = "Difference in total CDI words against BAPQ total")

ggsave(here("analysis", "ba_plots", "ba_cdi_v_bapq.png"), width = 6, height = 5)

lm4_cdi2 <- lm(cdi_p_minus_s ~ bapq_total_be*cg, data = bld3)

models2 <- tribble(
    ~name, ~model,
    "vrrsb", lm3_vrrsb2,
    "cdi", lm4_cdi2
  ) %>%
  mutate(
    tidy = map(model, tidy)
  ) %>%
  select(-model) %>%
  unnest(tidy)

# No effects of mean BAPQ on the difference; in the vrRSB or CDI, and for
#   either PCG or SCG.
