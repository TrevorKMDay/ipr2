library(tidyverse)

all_data <- read_rds(here("analysis", "sem_data.rds"))

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

ggsave("ba_plots/ba_vrrsb.png", width = 6, height = 5)

ggplot(bld2, aes(x = cdi_mean, y = cdi_p_minus_s)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Mean CDI score", y = "CDI PCG-SCG", title = "Total CDI words")

ggsave("ba_plots/ba_cdi.png", width = 6, height = 5)

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

ggsave("ba_plots/ba_vrrsb_v_bapq.png", width = 6, height = 5)

ggplot(bld3, aes(x = bapq_total_be, y = cdi_p_minus_s)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", fullrange = TRUE) +
  scale_x_continuous(limits = c(1, NA)) +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(vars(cg)) +
  theme_bw() +
  labs(x = "BAPQ Total (best-estimate)", y = "CDI PCG-SCG",
       title = "Difference in total CDI words against BAPQ total")

ggsave("ba_plots/ba_cdi_v_bapq.png", width = 6, height = 5)
