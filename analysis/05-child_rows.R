library(tidyverse)
library(corrr)
library(irr)

cdi <- read_rds(here("analysis", "cdi_wide_included.rds")) %>%
  select(id, age, starts_with("words_produced_number_p")) %>%
  rename(
    cdi_total_p1 = words_produced_number_p1,
    cdi_total_p2 = words_produced_number_p2
  )

vrrsb <- read_rds(here("analysis", "vrrsb_wide_included.rds")) %>%
  select(id, age, everything(), -starts_with("age_p")) %>%
  rename_with(~paste0("vrrsb_", .x), -c(id, age))

bapq <- read_rds(here("analysis", "bapq_wide_all.rds")) %>%
  select(id, starts_with("total")) %>%
  rowwise() %>%
  mutate(
    bapq_missing_p1 = is.na(total_self_p1) | is.na(total_partner_p2),
    bapq_missing_p2 = is.na(total_self_p2) | is.na(total_partner_p1),
    bapq_totalbest_p1 = mean(c(total_self_p1, total_partner_p2), na.rm = TRUE),
    bapq_totalbest_p2 = mean(c(total_self_p2, total_partner_p1), na.rm = TRUE)
  )

bapq2 <- select(bapq, id, contains("best"))

# Combine ====

child_rows <- full_join(cdi, vrrsb, by = join_by(id),
                        suffix = c(".cdi", ".vrrsb"))

# ICCs ====

cdi_icc <- icc(select(child_rows_complete, starts_with("cdi")),
               type = "consistency")

vrs_icc <- icc(select(child_rows_complete, starts_with("vrrsb_VRS_")),
               type = "consistency")

## Correlations ====

child_rows_complete <- child_rows %>%
  na.omit() %>%
  left_join(bapq2)


Hmisc::rcorr(child_rows_complete$bapq_totalbest_p1,
             child_rows_complete$bapq_totalbest_p2)

correlations <- child_rows_complete %>%
  select(age.cdi, starts_with("cdi"), starts_with("vrrsb_VRS")) %>%
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

child_long <- child_rows_complete %>%
  pivot_longer(-c(id, starts_with("age"))) %>%
  separate_wider_delim(name, delim = "_", names = c("inst", "score", "p1p2")) %>%
  pivot_wider(names_from = p1p2)

child_long2 <- child_long %>%
  filter(
    inst %in% c("cdi", "bapq") | inst == "vrrsb" & score == "VRS"
  )

plot_bapq <- ggplot(filter(child_long2, inst == "bapq"), aes(x = p1, y = p2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 3.15, color = "red") +
  geom_hline(yintercept = 3.15, color = "red") +
  scale_x_continuous(limits = c(1, NA), breaks = 1:6) +
  scale_y_continuous(limits = c(1, NA), breaks = 1:6) +
  theme_bw() +
  labs(x = "Parent 1", y = "Parent 2", color = "Age (mo.)",
       title = "BAPQ BE Total Score")

plot_cdi <- ggplot(filter(child_long2, inst == "cdi"), aes(x = p1, y = p2)) +
  geom_point(aes(color = age.cdi), alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_viridis(limits = c(23, 28), oob = scales::squish) +
  scale_x_continuous(limits = c(0, 680), breaks = seq(0, 680, by = 136)) +
  scale_y_continuous(limits = c(0, 680), breaks = seq(0, 680, by = 136)) +
  theme_bw() +
  labs(x = "Parent 1", y = "Parent 2", color = "Age (mo.)",
       title = "CDI Total Score")

plot_vrrsb <- ggplot(filter(child_long2, inst == "vrrsb"), aes(x = p1, y = p2)) +
  geom_jitter(aes(color = age.cdi), alpha = 0.5, width = 0.2, height = 0.2) +
  geom_smooth(method = "lm") +
  scale_color_viridis(limits = c(23, 28), oob = scales::squish) +
  scale_x_continuous(limits = c(NA, NA), breaks = seq(0, 20, by = 5)) +
  scale_y_continuous(limits = c(NA, NA), breaks = seq(0, 20, by = 5)) +
  theme_bw() +
  labs(x = "Parent 1", y = "Parent 2", color = "Age (mo.)",
       title = "vrRSB VRS")

library(patchwork)

plot_child <- plot_cdi + plot_vrrsb +
  plot_layout(guides = "collect", axes = "collect")

png(here("plots", "child_plot.png"), width = 6.5, height = 3.5, units = "in",
    res = 300)

print(plot_child)

dev.off()

# BAPQ ====

bapq3 <- bapq %>%
  filter(
    id %in% child_rows_complete$id
  )

bapq3_nomissing <- bapq3 %>%
  filter(
    !bapq_missing_p1, !bapq_missing_p2
  )

bapq3_nomissing_corr <- bapq3_nomissing %>%
  select(starts_with("total")) %>%
  as.matrix() %>%
  Hmisc::rcorr()
