library(tidyverse)
library(here)
library(corrr)
library(irr)
library(viridis)

cdi <- read_rds(here("analysis", "cdi_wide_included.rds")) %>%
  select(id, age, starts_with("words_produced_number_")) %>%
  select_all(~str_replace(., "words_produced_number", "cdi_total"))

vrrsb <- read_rds(here("analysis", "vrrsb_wide_included.rds")) %>%
  select(id, age, everything(), -starts_with("age_p")) %>%
  rename_with(~paste0("vrrsb_", .x), -c(id, age))

cdi_ttest <- t.test(cdi$cdi_total_pcg, cdi$cdi_total_scg, paired = TRUE)
vrrsb_ttest <- t.test(vrrsb$vrrsb_total_pcg, vrrsb$vrrsb_total_scg,
                      paired = TRUE)

esc::esc_t(cdi_ttest$statistic, cdi_ttest$p.value, grp1n = 300, grp2n = 300)
esc::esc_t(vrrsb_ttest$statistic, vrrsb_ttest$p.value, grp1n = 300,
           grp2n = 300)

# Combine ====

child_rows <- full_join(cdi, vrrsb, by = join_by(id),
                        suffix = c("_cdi", "_vrrsb"))

## Correlations ====

correlations <- child_rows %>%
  select(age_cdi, starts_with("cdi"), starts_with("vrrsb_total")) %>%
  as.matrix() %>%
  Hmisc::rcorr()

correlations_r <- correlations$r %>%
  as_cordf() %>%
  shave() %>%
  stretch(na.rm = TRUE)

correlations_p <- correlations$P %>%
  as_cordf() %>%
  shave() %>%
  stretch(na.rm = TRUE) %>%
  rename(
    p = r
  )

correlations_table <- left_join(correlations_r, correlations_p,
                                by = join_by(x, y))%>%
  mutate(
    r = round(r, 3),
    p = round(p, 3)
  )

# Plots ====

child_long <- child_rows %>%
  select(-vrrsb_age_scg) %>%
  pivot_longer(-c(id, starts_with("age"), vrrsb_outlier)) %>%
  separate_wider_delim(name, delim = "_", names = c("inst", "score", "pcg")) %>%
  pivot_wider(names_from = pcg)

plot_cdi <- ggplot(filter(child_long, inst == "cdi"), aes(x = pcg, y = scg)) +
  geom_point(aes(color = age_cdi), alpha = 0.5) +
  geom_smooth(method = "lm", fullrange = TRUE) +
  scale_color_viridis(limits = c(23, 28), oob = scales::squish) +
  scale_x_continuous(limits = c(0, 680), breaks = seq(0, 680, by = 136)) +
  scale_y_continuous(limits = c(0, 680), breaks = seq(0, 680, by = 136)) +
  theme_bw() +
  labs(x = "PCG", y = "SCG", color = "Age (mo.)",
       title = "(a) CDI total score")

plot_vrrsb <-ggplot(filter(child_long, inst == "vrrsb", score == "total"),
                     aes(x = pcg, y = scg)) +
  geom_point(aes(color = age_cdi), alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_smooth(
    data = filter(child_long, inst == "vrrsb", score == "total",
                  !vrrsb_outlier),
    method = "lm", color = "darkgreen", fill = "lightgreen",
  ) +
  scale_color_viridis(limits = c(23, 28), oob = scales::squish) +
  theme_bw() +
  labs(x = "PCG", y = "SCG", color = "Age (mo.)",
       title = "(b) vrRSB total score")

library(patchwork)

plot_child <- plot_cdi + plot_vrrsb +
  plot_layout(guides = "collect", axes = "collect")

png(here("plots", "child_plot.png"), width = 6.5, height = 3.5, units = "in",
    res = 300)

print(plot_child)

dev.off()



cdi2 <- ggplot(filter(child_long, inst == "cdi"), aes(x = pcg, y = scg)) +
  geom_point(aes(color = age_cdi), alpha = 0.5, size = 7.5) +
  geom_smooth(method = "lm", fullrange = TRUE, linewidth = 3) +
  scale_color_viridis(limits = c(23, 28), oob = scales::squish) +
  scale_x_continuous(limits = c(0, 680), breaks = seq(0, 680, by = 136)) +
  scale_y_continuous(limits = c(0, 680), breaks = seq(0, 680, by = 136)) +
  theme_bw() +
  labs(x = "PCG", y = "SCG", color = "Age (mo.)",
       title = "(a) CDI total score") +
  theme(text = element_text(size = 24))


vrrsb2 <- ggplot(filter(child_long, inst == "vrrsb", score == "total"),
                 aes(x = pcg, y = scg)) +
  geom_point(aes(color = age_cdi), alpha = 0.5, size = 7.5) +
  geom_smooth(method = "lm", fullrange = TRUE, linewidth = 3) +
  geom_smooth(
    data = filter(child_long, inst == "vrrsb", score == "total",
                  !vrrsb_outlier),
    method = "lm", color = "darkgreen", fill = "lightgreen",
    fullrange = FALSE, linewidth = 2
  ) +
  scale_color_viridis(limits = c(23, 28), oob = scales::squish) +
  theme_bw() +
  labs(x = "PCG", y = "SCG", color = "Age (mo.)",
       title = "(b) vrRSB total score") +
  theme(text = element_text(size = 24))

plot2_child <- cdi2 + vrrsb2 +
  plot_layout(guides = "collect", axes = "collect")

png(here("plots", "child_plot_poster.png"), width = 20, height = 9,
    units = "in", res = 300)

print(plot2_child)

dev.off()

