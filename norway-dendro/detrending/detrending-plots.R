# plot individual curves as (annual) boxplots
inputIndividualCurves <- subset(inputProcess, inputProcess$var == "Individual curves")
# pinput1$value <- replace(pinput1$value, pinput1$value < 10, NA)

plotsBoxplot <- ggplot(inputIndividualCurves) +
  geom_boxplot(aes(x = as.numeric(year), y = value, group = year, col = var), alpha = .3) +
  geom_text(data = labels, aes(x = Inf, y = Inf, hjust = 1.1, vjust = 1.2, label = label)) +
  scale_alpha_manual(values = c(0.1)) +
  scale_color_manual(values = c(palette[2], palette[6])) +
  facet_wrap(~species, scale = "free_y", nrow = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Calendar year", y = "Ring width [μm]", col = "", alpha = "") +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank()
  )
plotsBoxplot 

# plot by biological age (lines and mean)
inputProcessSubset <- subset(inputProcess, is.na(inputProcess$value) == FALSE)
plotsAge <- ggplot(inputProcessSubset) +
  geom_line(aes(x = age, y = value, group = id, col = var, alpha = var)) +
  # geom_point(aes(x = age, y = value)) +
  scale_alpha_manual(values = c(1, 0.1)) +
  scale_color_manual(values = c(palette[3], palette[2], palette[6])) +
  geom_pointrange(data = subset(inputProcessSubset, inputProcessSubset$var == "Biweigth robust mean series"), aes(x = age, y = value, ymin = value - se, ymax = value + se), col = palette[3], size = 0.05) +
  facet_wrap(~species, scale = "free", nrow = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Biological age [year]", y = "Ring width [μm]", col = "", alpha = "") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.spacing.y = unit(0, "pt"),
    legend.justification = c(1.02, 1.08),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(0.35, "cm"),
    legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
    legend.position = c(1, 1)
  )
plotsAge

# Plot RCS curves with linear trend and spline
# create input dataframe from raw series (biweight robust mean), spline and linear trend
MeanSeries <- data.frame(
  age = input_age_mean_species$age,
  species = input_age_mean_species$species,
  tbrm = input_age_mean_species$value,
  var = rep("Biweigth robust mean series", nrow(input_age_mean_species)),
  value = input_age_mean_species$value,
  se = input_age_mean_species$se
)

RCSspline <- list()
RCSlinear <- list()
for (i in seq_along(inputSpeciesMean)) {
  RCSspline[[i]] <- data.frame(
    age = inputSpeciesMean[[i]]$age,
    species = inputSpeciesMean[[i]]$species,
    tbrm = inputSpeciesMean[[i]]$value,
    var = rep("RCS curve (spline)", nrow(inputSpeciesMean[[i]])),
    value = smooth[[i]],
    se = rep(NA, nrow(inputSpeciesMean[[i]]))
  )

  RCSlinear[[i]] <- data.frame(
    age = inputSpeciesMean[[i]]$age,
    species = inputSpeciesMean[[i]]$species,
    tbrm = inputSpeciesMean[[i]]$value,
    var = rep("RCS curve (linear trend)", nrow(inputSpeciesMean[[i]])),
    value = linear[[i]],
    se = rep(NA, nrow(inputSpeciesMean[[i]]))
  )
}

pinputTrend <- as.data.frame(rbind(
  MeanSeries,
  do.call(rbind, RCSspline),
  do.call(rbind, RCSlinear)
))

# trend <- subset(trend, trend$age <= 20)
# trend <- subset(trend, trend$species != "S. herbacea" | trend$age <= 10)

# plot
pinputTrendSubset <- subset(pinputTrend, is.na(pinputTrend$value) == FALSE)
plotsRCS <- ggplot(pinputTrendSubset) +
  geom_line(aes(x = age, y = value, group = var, col = var), size = 0.5) +
  geom_pointrange(aes(x = age, y = value, ymin = value - se, ymax = value + se, col = var), size = 0.05) +
  scale_color_manual(values = c(palette[3], palette[2], palette[1])) +
  scale_fill_manual(values = c(palette[3], palette[2], palette[1])) +
  facet_wrap(~species, scale = "free", nrow = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Biological age [years]", y = "Ring width [μm]", col = "", alpha = "") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.spacing.y = unit(0, "pt"),
    legend.justification = c(1.02, 1.08),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(0.35, "cm"),
    legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
    legend.position = c(1, 1)
  )
plotsRCS 

# Plot Ring width indices (RWI) from individual curves
pinputRWI <- data.frame(
  age = rep(input_age_mean_species$age, 4),
  species = rep(input_age_mean_species$species, 4),
  tbrm = rep(input_age_mean_species$value, 4),
  var = rep(c("Biweigth robust mean series", "Smoothing spline curve (mean)", "Negative exponential model curve (mean)", "Hugershoff model curve (mean)"), each = nrow(input_age_mean_species)),
  value = c(input_age_mean_species$value, spline, exp, hug),
  se = c(input_age_mean_species$se, spline_se, exp_se, hug_se)
)
pinputRWI$var <- factor(pinputRWI$var, levels = c("Biweigth robust mean series", "Smoothing spline curve (mean)", "Negative exponential model curve (mean)", "Hugershoff model curve (mean)"))

pinputRWISubset <- subset(pinputRWI, is.na(pinputRWI$value) == FALSE)
plotsRWI <- ggplot(pinputRWISubset) +
  geom_line(aes(x = age, y = value, group = var, col = var), size = 0.5) +
  geom_pointrange(aes(x = age, y = value, ymin = value - se, ymax = value + se, col = var), size = 0.05) +
  scale_color_manual(values = c(palette[3], palette[4], palette[5], palette[6], palette[7])) +
  facet_wrap(~species, scale = "free", nrow = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Biological age [years]", y = "Ring width [μm]", col = "", alpha = "") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.justification = c(1.02, 1.08),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.y = unit(0, "pt"),
    legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
    legend.position = c(1, 1)
  )
plotsRWI

# detrended curve (mean)
# input <- split(input_age, input_age$id)
# input <- subset(input_age, input_age$age <= 20)
# input$value <- replace(input$value, input$species == "S. herbacea" & input$age > 10, NA)
# add rcs curves
inputSpecies <- split(input_age, input_age$species)
for (i in seq_along(inputSpecies)) {
  inputSpecies[[i]]$curves_rcs_spline <- rep(smooth[[i]], length(unique(inputSpecies[[i]]$id)))
  inputSpecies[[i]]$curves_rcs_lin <- rep(linear[[i]], length(unique(inputSpecies[[i]]$id)))
  inputSpecies[[i]]$curves_rcs <- rep(inputSpeciesMean[[i]]$value, length(unique(inputSpecies[[i]]$id)))
}

inputDetrended <- as.data.frame(do.call(rbind, inputSpecies))
inputDetrended$series_rcs_spline <- inputDetrended$value / inputDetrended$curves_rcs_spline
inputDetrended$series_rcs_lin <- inputDetrended$value / inputDetrended$curves_rcs_lin
inputDetrended$series_rcs <- inputDetrended$value / inputDetrended$curves_rcs
inputDetrended$value2 <- inputDetrended$value / 100

# get averraged curves
pinput <- aggregate(inputDetrended, by = list(year = inputDetrended$year, species = inputDetrended$species), function(x) mean(x, na.rm = TRUE), drop = FALSE)
pinput_se <- aggregate(inputDetrended, by = list(year = inputDetrended$year, species = inputDetrended$species), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)), drop = FALSE)

# create input data for plot
pinput <- data.frame(
  detrended = c(pinput$value2, pinput$series_spline, pinput$series_negexp, pinput$series_hug, pinput$series_rcs_spline, pinput$series_rcs_lin, pinput$series_rcs),
  se = c(pinput_se$value2, pinput_se$series_spline, pinput_se$series_negexp, pinput_se$series_hug, pinput_se$series_rcs_spline, pinput_se$series_rcs_lin, pinput_se$series_rcs),
  var = rep(c("Mean series", "Ring width index (smoothing spline)", "Ring width index (negative exponential model)", "Ring width index (Hugershoff model)", "RCS chronology (smoothing spline)", "RCS chronology (linear trend)", "RCS chronology (raw)"), each = nrow(pinput)),
  year = rep(pinput$year, 7),
  species = rep(pinput$species, 7)
)
pinput$year <- as.numeric(pinput$year)
pinputSubset <- subset(pinput, is.na(pinput$detrended) == FALSE)
plotsDetrended <- ggplot(pinputSubset) +
  geom_line(aes(x = year, y = detrended, group = var, col = var), size = 0.5) +
  geom_line(data = subset(pinputSubset, pinputSubset$var == "Mean series"), aes(x = year, y = detrended, group = var), size = .8, col = palette[3]) +
  geom_pointrange(aes(x = year, y = detrended, ymin = detrended - se, ymax = detrended + se, col = var), size = 0.05, alpha = .5) +
  scale_color_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4])) +
  scale_fill_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4])) +
  facet_wrap(~species, scale = "free", nrow = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(sec.axis = dup_axis(trans = ~ . * 100, name = "")) +
  labs(x = "Calender year", y = "Ring width index/RCS chronology", col = "", alpha = "") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.justification = c(1.02, 1.08),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.y = unit(0, "pt"),
    legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
    legend.position = c(1, 1)
  )
plotsDetrended

# plot difference between mean series and detrended series
inputDifference <- split(pinput, pinput$var)
for (i in seq_along(inputDifference)) {
  inputDifference[[i]]$difference <- inputDifference[[i]]$detrended - inputDifference[[1]]$detrended
}
inputDifference <- as.data.frame(do.call(rbind, inputDifference))

# inputDifference$difference <- replace(inputDifference$difference, inputDifference$species == "B. nana" | inputDifference$difference < 4, NA)
pinput <- subset(inputDifference, inputDifference$var != "Mean series")
plotsDifference <- ggplot(pinput) +
  geom_hline(yintercept = 0) +
  # geom_area(aes(x = year, y = difference, group = var, fill = var),position = "jitter", alpha = 0.1) +
  geom_line(aes(x = year, y = difference, group = var, col = var), size = 0.5) +
  # geom_line(data = subset(pinput, pinput$var == "Mean series"), aes(x = year, y = detrended, group = var), size = .8, col = palette[3]) +
  # geom_pointrange(aes(x = year, y = difference, ymin = difference -se, ymax = difference + se, col = var), size = 0.05, alpha = .5) +
  scale_color_manual(values = c(palette[2], palette[7], palette[1], palette[6], palette[5], palette[4])) +
  scale_fill_manual(values = c(palette[2], palette[7], palette[1], palette[6], palette[5], palette[4])) +
  facet_wrap(~species, scale = "free_y", nrow = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Calender year", y = "Difference to mean series", col = "", alpha = "") +
  theme(legend.position = "bottom") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.justification = c(1.02, 1.08),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.y = unit(0, "pt"),
    legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
    legend.position = c(1, 1)
  )
plotsDifference

finalPlots <- ggarrange(
  plotlist = list(sample_depth, boxplot, comp, plotsAge, plotsRCS, plotsRWI, plotsDetrended),
  ncol = 1, common.legend = FALSE, align = "hv", labels = c("a", "b", "c", "d", "e", "f", "g")
)
finalPlots

cairo_pdf("detrending_all.pdf", width = 17, height = 28, pointsize = 10)
finalPlots
dev.off()

## individual curve example
# select individual
pinput <- split(inputDetrended, inputDetrended$id)
pinput <- pinput[["2435A2base C. galianoi"]]
first(pinput$id)

trend <- data.frame(
  age = rep(pinput$age, 3),
  var = rep(c("Individual curve", "RCS curve (spline)", "RCS curve (linear trend)"), each = nrow(pinput)),
  value = c(pinput$value, pinput$curves_rcs_spline, pinput$curves_rcs_lin)
)

trend <- subset(trend, is.na(trend$value) == FALSE)
PlotsRCSExample <- ggplot(trend) +
  geom_line(aes(x = age, y = value, group = var, col = var), size = 0.5) +
  scale_color_manual(values = c(palette[3], palette[2], palette[1])) +
  scale_fill_manual(values = c(palette[3], palette[2], palette[1])) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "", y = "", col = "", alpha = "", title = "Individual curve") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1),
    legend.justification = c(1.02, 1.08),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.y = unit(0, "pt"),
    legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
    legend.position = c(1, 1)
  )
PlotsRCSExample

# combine plots with example
PlotsRCSExampleArrange <- ggarrange(
  plotlist = list(NA, PlotsRCSExample), heights = c(1, 2),
  ncol = 1)
PlotsRCSExampleArrange 
PlotsRCSAll <- ggarrange(
  plotlist = list(plotsRCS, PlotsRCSExampleArrange), widths = c(1, 0.2),
  nrow = 1, common.legend = FALSE
)
PlotsRCSAll

trend2 <- data.frame(
  age = rep(pinput$age, 4),
  var = rep(c("Individual curve", "Smoothing spline curve", "Negative exponential model curve", "Hugershoff model curve"), each = nrow(pinput)),
  value = c(pinput$value, pinput$curves_spline, pinput$curves_negexp, pinput$curves_hug)
)

trend2$var <- factor(trend2$var, levels = c("Individual curve", "Smoothing spline curve", "Negative exponential model curve", "Hugershoff model curve"))

trend2 <- subset(trend2, is.na(trend2$value) == FALSE)
PlotsRWIExample <- ggplot(trend2) +
  geom_line(aes(x = age, y = value, group = var, col = var), size = 0.5) +
  scale_color_manual(values = c(palette[3], palette[4], palette[5], palette[6], palette[7])) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "", y = "", col = "", alpha = "", title = "Individual curve") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1),
    legend.justification = c(1.02, 1.08),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.y = unit(0, "pt"),
    legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
    legend.position = c(1, 1)
  )
PlotsRWIExample

# combine plots with example
PlotsRWIExampleArrange <- ggarrange(
  plotlist = list(NA, PlotsRWIExample), heights = c(1, 2),
  ncol = 1)
PlotsRWIExampleArrange 
PlotsRWIAll <- ggarrange(
  plotlist = list(plotsRWI, PlotsRWIExampleArrange), widths = c(1, 0.2),
  nrow = 1, common.legend = FALSE
)
PlotsRWIAll


pinput$value2 <- pinput$value / 100
pinput <- data.frame(
  detrended = c(pinput$value2, pinput$series_spline, pinput$series_negexp, pinput$series_hug, pinput$series_rcs_spline, pinput$series_rcs_lin, pinput$series_rcs),
  var = rep(c("Individual curve", "Ring width index (smoothing spline)", "Ring width index (negative exponential model)", "Ring width index (Hugershoff model)", "RCS chronology (smoothing spline)", "RCS chronology (linear trend)", "RCS chronology (raw)"), each = nrow(pinput)),
  year = as.numeric(rep(pinput$year, 7))
)

PlotsDetrendedxample <- ggplot(pinput) +
  geom_line(aes(x = year, y = detrended, group = var, col = var), size = 0.5) +
  geom_line(data = subset(pinput, pinput$var == "Mean series"), aes(x = year, y = detrended, group = var), size = .8, col = palette[3]) +
  scale_color_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4])) +
  scale_fill_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(sec.axis = dup_axis(trans = ~ . * 100, name = "")) +
  labs(x = "", y = "", col = "", alpha = "", title = "Individual curve") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1),
    legend.justification = c(1.02, 1.08),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.y = unit(0, "pt"),
    legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
    legend.position = c(1, 1)
  )
PlotsDetrendedxample

# combine plots with example
PlotsDetrendedExampleArrange <- ggarrange(
  plotlist = list(NA, PlotsDetrendedxample), heights = c(1, 2),
  ncol = 1)
PlotsDetrendedExampleArrange
PlotsDetrendedAll <- ggarrange(
  plotlist = list(plotsDetrended, PlotsDetrendedExampleArrange), widths = c(1, 0.2),
  nrow = 1, common.legend = FALSE
)
PlotsDetrendedAll

inputDifferenceExample <- split(pinput, pinput$var)
for (i in seq_along(inputDifferenceExample)) {
  inputDifferenceExample[[i]]$difference <- inputDifferenceExample[[i]]$detrended - inputDifferenceExample[[1]]$detrended
}
inputDifferenceExample <- as.data.frame(do.call(rbind, inputDifferenceExample))
p5 <- ggplot(inputDifferenceExample) +
  geom_line(aes(x = year, y = difference, group = var, col = var), size = 0.5) +
  # geom_line(data = subset(pinput, pinput$var == "Mean series"), aes(x = year, y = detrended, group = var), size = .8, col = palette[3]) +
  scale_color_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4])) +
  scale_fill_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4])) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Age", y = "Difference to inividual curve", col = "", alpha = "") +
  theme(legend.position = "bottom") +
  theme_bw()
p5

# final plots
PlotsBoxplots <- ggarrange(
  plotlist = list(plotsBoxplot , NA), widths = c(1, 0.2),
  nrow = 1, common.legend = FALSE
)

plotsAge <- ggarrange(
  plotlist = list(plotsAge, NA), widths = c(1, 0.2),
  nrow = 1, common.legend = FALSE
)

finalPlots <- ggarrange(
  plotlist = list(PlotsBoxplots, plotsAge, PlotsRCSAll, PlotsRWIAll, PlotsDetrendedAll),
  ncol = 1, common.legend = FALSE, align = "hv", labels = c("a", "b", "c", "d", "e", "f", "g")
)
finalPlots 

cairo_pdf("detrending_all.pdf", width = 27, height = 25, pointsize = 10)
finalPlots
dev.off()
