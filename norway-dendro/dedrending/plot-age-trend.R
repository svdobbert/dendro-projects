## plot growth by age

# by species
# boxplot
p1 <- ggplot(input_age) +
  # geom_hline(yintercept = 100, linetype = "dashed") +
  # geom_line(data = input_age_mean_species, aes(x = age, y = perc_tbrm_species), col = palette[1]) +
  geom_boxplot(aes(x = age, y = value, group = age), col = palette[3], alpha = .3) +
  theme_bw() +
  facet_wrap(~species, scale = "free_y", nrow = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = unit(c(0, 1, 0, 1), "cm")
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  labs(x = "Age", y = "Ring width [μm]")
p1

p2 <- ggplot(input_age) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_boxplot(aes(x = age, y = perc_tbrm_species, group = age), col = palette_light[3], alpha = .3) +
  geom_line(data = input_age_mean_species, aes(x = age, y = perc_tbrm_species), col = palette[1]) +
  geom_pointrange(data = input_age_mean_species, aes(x = age, y = perc_tbrm_species, ymin = perc_tbrm_species - perc_tbrm_se, ymax = perc_tbrm_species + perc_tbrm_se), col = palette[1], size = 0.05) +
  theme_bw() +
  facet_wrap(~species, scale = "free_y", nrow = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = unit(c(0, 1, 1, 1), "cm")
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 300)) +
  labs(x = "Biological age [years]", y = "Ring width compared to annual biweigth robust mean [%]")
p2

p3 <- ggplot(input_age_mean_species) +
  geom_area(aes(x = age, y = sample_depth), alpha = .5, fill = palette[2]) +
  theme_bw() +
  geom_text(data = labels, aes(x = Inf, y = Inf, hjust = 1.1, vjust = 1.2, label = label)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    plot.margin = unit(c(0, 1, -0.5, 1), "cm")
  ) +
  facet_wrap(~species, scale = "free_y", nrow = 1) +
  labs(x = "", y = "Sample depth")
p3

p4 <- ggplot(input_age_mean_species) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_line(aes(x = age, y = perc_tbrm_species), alpha = .5, col = palette[1]) +
  geom_pointrange(aes(x = age, y = perc_tbrm_species, ymin = perc_tbrm_species - perc_tbrm_se, ymax = perc_tbrm_species + perc_tbrm_se), alpha = .5, col = palette[1]) +
  theme_bw() +
  facet_wrap(~species) +
  theme(axis.text.x = element_blank()) +
  labs(x = "", y = "Ring width compared to annual biweigth robust mean [%]")
p4

cairo_pdf("Age_trend_by_species.pdf", width = 17, height = 10, pointsize = 10)
ggarrange(
  plotlist = list(p3, p1, p2), heights = c(0.4, 1, 1), widths = c(1, 1, 1), legend = "bottom",
  nrow = 3, common.legend = TRUE, align = "v", labels = c("a", "b", "c")
)
dev.off()


###
sample_depth <- p3
boxplot <- p1
comp <- p2


## by group (base, stem, root)

p1 <- ggplot(input_age) +
  # geom_hline(yintercept = 100, linetype = "dashed") +
  # geom_line(data = input_age_mean_species, aes(x = age, y = perc_tbrm_species), col = palette[1]) +
  geom_boxplot(aes(x = age, y = value, group = age), col = palette[3], alpha = .3) +
  theme_bw() +
  facet_wrap(~ species + group, scale = "free_y", nrow = 1) +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = unit(c(0, 1, 0, 1), "cm")
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  labs(x = "Age", y = "Ring width [μm]")
p1

p2 <- ggplot(input_age) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_boxplot(aes(x = age, y = perc_tbrm_group, group = age), col = palette_light[3], alpha = .3) +
  geom_line(data = input_age_mean_group, aes(x = age, y = perc_tbrm_group), col = palette[1]) +
  geom_pointrange(data = input_age_mean_group, aes(x = age, y = perc_tbrm_group, ymin = perc_tbrm_group - perc_tbrm_se, ymax = perc_tbrm_group + perc_tbrm_se), col = palette[1], size = 0.05) +
  theme_bw() +
  facet_wrap(~ species + group, nrow = 1) +
    theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = unit(c(0, 1, 1, 1), "cm")
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 300)) +
  labs(x = "", y = "Ring width compared to annual biweigth robust mean [%]")
p2

p3 <- ggplot(input_age_mean_group) +
  geom_area(aes(x = age, y = sample_depth), alpha = .5, fill = palette[2]) +
  theme_bw() +
  geom_text(data = labelsGroup, aes(x = Inf, y = Inf, hjust = 1.1, vjust = 1.2, label = label)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~ species + group, nrow = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    plot.margin = unit(c(0, 1, -0.5, 1), "cm")
  ) +
  labs(x = "", y = "Sample depth")
p3

p4 <- ggplot(input_age_mean_group) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_line(aes(x = age, y = perc_tbrm_group), alpha = .5, col = palette[1]) +
  geom_pointrange(aes(x = age, y = perc_tbrm_group, ymin = perc_tbrm_group - perc_tbrm_se, ymax = perc_tbrm_group + perc_tbrm_se), alpha = .5, col = palette[1]) +
  theme_bw() +
  facet_wrap(~ species + group, nrow = 1) +
  theme(axis.text.x = element_blank()) +
  labs(x = "", y = "Ring width compared to annual biweigth robust mean [%]")
p4

cairo_pdf("Age_trend_by_group.pdf", width = 30, height = 10, pointsize = 10)
ggarrange(
  plotlist = list(p3, p1, p2), heights = c(0.3, 1, 1), widths = c(1, 1, 1), legend = "bottom",
  nrow = 3, common.legend = TRUE, align = "v"
)
dev.off()
