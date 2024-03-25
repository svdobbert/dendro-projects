#RCS prozess
inputProcess <- data.frame(value = c(input_age$value, input_age_mean_species$value),
                     age = c(input_age$age, input_age_mean_species$age),
                     year = c(input_age$year, input_age_mean_species$year),
                     species = c(input_age$species, input_age_mean_species$species),
                     id = c(input_age$id, rep("mean", nrow(input_age_mean_species))),
                     var = c(rep("Individual curves", nrow(input_age)), rep("Biweigth robust mean series", nrow(input_age_mean_species))),
                     se = c(rep(NA, nrow(input_age)), input_age_mean_species$se)
)
# calculate linear and smoothed RCS series
inputSpecies <- split(inputProcess , inputProcess$species)

series <- list()
smooth <- list()
linear <- list()
series.rwi <- list()
p_value <- list()
coefficient <- list()

overall_p <- function(model) {
  f <- summary(model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

inputSpeciesMean <- split(input_age_mean_species, input_age_mean_species$species)
for(i in seq_along(inputSpeciesMean)) {
  series[[i]] <- as.numeric(na.omit(inputSpeciesMean[[i]]$value))
  smooth[[i]] <- c(dplR::caps(series[[i]][1: (length(series[[i]])/2)]),
  dplR::caps(series[[i]][((length(series[[i]])/2)):length(series[[i]])]))
  linear[[i]] <- lm(inputSpeciesMean[[i]]$value ~ inputSpeciesMean[[i]]$age)
  p_value[[i]] <- overall_p(linear[[i]])
  coefficient[[i]] <- summary(linear[[i]])[["coefficients"]][[2]] 
  linear[[i]] <- predict(linear[[i]])

  smooth[[i]] <- c(smooth[[i]], rep(
  NA, 
  (nrow(inputSpeciesMean[[i]]) - length(smooth[[i]])))
)
linear[[i]] <- c(linear[[i]], rep(
  NA, 
  (nrow(inputSpeciesMean[[i]]) - length(linear[[i]])))
)
}

## Plot histogram
inputTrend <- split(input_age, input_age$id)
age <- lapply(inputTrend, function(x) data.frame(id = first(x$id),
                                            species = first(x$species),
                                            group = first(x$group),
                                            age = length(na.omit(x$value))))
age_structure <- as.data.frame(do.call(rbind, age))

p <- ggplot(age_structure) +
  geom_histogram(aes(x = age), fill = palette[4], alpha = 1, binwidth = 1) +
  facet_wrap(~ species, nrow = 1) +
  labs(x = "Biological age [years]") +
  scale_y_continuous(expand = c(0,1)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(strip.background = element_blank())
p
cairo_pdf("histrogram.pdf", width = 13, height = 3, pointsize = 10)  
p
dev.off()

# calculate RWI
list <- list()
for (i in seq_along(inputTrend)) {
list[[i]] <- list(Spline = rep(NA, nrow(inputTrend[[i]])),
             ModNegExp = rep(NA, nrow(inputTrend[[i]])),
             AgeDepSpline = rep(NA, nrow(inputTrend[[i]])),
             ModHugershoff = rep(NA, nrow(inputTrend[[i]])))
list[[i]] <- list(series = list[[i]],
             curves = list[[i]])


# detrend individual series
if (length(na.omit(inputTrend[[i]]$value)) > 2) {
series.rwi[[i]] <- detrend.series(y = inputTrend[[i]]$value, make.plot = F, verbose=TRUE, return.info = TRUE) 
} else {
series.rwi[[i]] <- list[[i]]
}

inputTrend[[i]]$curves_spline <- series.rwi[[i]][["curves"]][["Spline"]]
inputTrend[[i]]$series_spline <- series.rwi[[i]][["series"]][["Spline"]]
inputTrend[[i]]$curves_negexp <- series.rwi[[i]][["curves"]][["ModNegExp"]]
inputTrend[[i]]$series_negexp <- series.rwi[[i]][["series"]][["ModNegExp"]]
inputTrend[[i]]$curves_agespline <- series.rwi[[i]][["curves"]][["AgeDepSpline"]]
inputTrend[[i]]$series_agespline <- series.rwi[[i]][["series"]][["AgeDepSpline"]]
inputTrend[[i]]$curves_hug <- series.rwi[[i]][["curves"]][["ModHugershoff"]]
inputTrend[[i]]$series_hug <- series.rwi[[i]][["series"]][["ModHugershoff"]]
}

input_age <- as.data.frame(do.call(rbind, inputTrend))

# test <- input[[16]]

# trend <- data.frame(age = rep(input_test$age, 3),
#                       species = rep(input_test$species, 3),
#                       tbrm = rep(input_test$value, 3),
#                       var = rep(c("Biweigth robust mean series", "RCS curve (spline)", "RCS curve (linear trend)"), each = nrow(input_test)),
#                       value = c(input_test$value, smooth, linear),
#                     se = c(input_test$se, rep(NA, nrow(input_test)*2)))   

spline <- aggregate(input_age, by = list(age = input_age$age, species = input_age$species), function(x) mean(x, na.rm = TRUE))$curves_spline
exp <- aggregate(input_age, by = list(age = input_age$age, species = input_age$species), function(x) mean(x, na.rm = TRUE))$curves_negexp
agespline <- aggregate(input_age, by = list(age = input_age$age, species = input_age$species), function(x) mean(x, na.rm = TRUE))$curves_agespline
hug <- aggregate(input_age, by = list(age = input_age$age, species = input_age$species), function(x) mean(x, na.rm = TRUE))$curves_hug


spline_se <- aggregate(input_age, by = list(age = input_age$age, species = input_age$species), function(x) sd(x, na.rm = TRUE)/sqrt(length(x)))$curves_spline
exp_se <- aggregate(input_age, by = list(age = input_age$age, species = input_age$species), function(x) sd(x, na.rm = TRUE)/sqrt(length(x)))$curves_negexp
agespline_se <- aggregate(input_age, by = list(age = input_age$age, species = input_age$species), function(x) sd(x, na.rm = TRUE)/sqrt(length(x)))$curves_agespline
hug_se <- aggregate(input_age, by = list(age = input_age$age, species = input_age$species), function(x) sd(x, na.rm = TRUE)/sqrt(length(x)))$curves_hug





# linear trends of individual series
linear_individual <- list()
p_value_individual <- list()
coefficient_individual <- list()
species_individual <- list()
group_individual <- list()
id_individual <- list()
for(i in c(1:length(inputTrend))) {
  linear_individual[[i]] <- lm(inputTrend[[i]]$value ~ inputTrend[[i]]$age)
  p_value_individual[[i]] <- overall_p(linear_individual[[i]])
  coefficient_individual[[i]] <- summary(linear_individual[[i]])[["coefficients"]][[2]] 
  species_individual[[i]] <- first(inputTrend[[i]]$species)
  group_individual[[i]] <- first(inputTrend[[i]]$group)
  id_individual[[i]] <- first(inputTrend[[i]]$id)
}
trend <- data.frame(estimate = unlist(coefficient_individual),
                    p_value = unlist(p_value_individual),
                    species = unlist(species_individual),
                    group = unlist(group_individual),
                    id = unlist(id_individual))
trend$site <- substr(trend$id, 1, 5)
trend$individual <- substr(trend$id, 1, 6)
trend$trend <- rep("neutral", nrow(trend))
trend$trend <- replace(trend$trend, trend$estimate > 0 & trend$p_value <= 0.05, "positive")
trend$trend <- replace(trend$trend, trend$estimate < 0 & trend$p_value <= 0.05, "negative")
trend$trend_individual <- paste0(trend$trend, trend$individual)
trend$trend_section <- paste0(trend$trend, trend$group)
trend <- split(trend, trend$species)

# compute unique levels in data frame 
lvls <- list()
sum <- list()
n <- list()
perc <- list()
pinput <- list()
pinput_individual <- list()
pinput_section <- list()
palette_site <- list()
palette_section <- list()
for(i in seq_along(trend)) {
# overall trend
lvls[[i]] <- unique(unlist(trend[[i]]$trend)) 
sum[[i]] <- table(factor(trend[[i]]$trend, levels = lvls[[i]],  
       ordered = TRUE))
n[[i]] <- sum(sum[[i]])
perc[[i]] <- sapply(sum[[i]], function(x) x/n[[i]]*100)
pinput[[i]] <- data.frame(trend = lvls[[i]],
                     perc = perc[[i]])
pinput[[i]]$lvl <- rep("all", nrow(pinput[[i]]))

# trend by individual
lvls[[i]] <- unique(unlist(trend[[i]]$trend_individual)) 
sum[[i]] <- table(factor(trend[[i]]$trend_individual, levels = lvls[[i]],  
                    ordered = TRUE))
n[[i]] <- sum(sum[[i]])
perc[[i]] <- sapply(sum[[i]], function(x) x/n[[i]]*100)
pinput_individual[[i]] <- data.frame(trend = lvls[[i]],
                     perc = perc[[i]])
sum(pinput_individual[[i]]$perc)
pinput_individual[[i]]$lvl <- rep("individual", nrow(pinput_individual[[i]]))

# trend by section
lvls[[i]] <- unique(unlist(trend[[i]]$trend_section)) 
sum[[i]] <- table(factor(trend[[i]]$trend_section, levels = lvls[[i]],  
                    ordered = TRUE))
n[[i]] <- sum(sum[[i]])
perc[[i]] <- sapply(sum[[i]], function(x) x/n[[i]]*100)
pinput_section[[i]] <- data.frame(trend = lvls[[i]],
                      perc = perc[[i]])
sum(pinput_section[[i]]$perc)
pinput_section[[i]]$lvl <- rep("section", nrow(pinput_section[[i]]))

# combine results
pinput[[i]] <- as.data.frame(rbind(pinput[[i]], pinput_individual[[i]], pinput_section[[i]]))
pinput[[i]]$overall_trend <- replace(pinput[[i]]$trend, grep('neutral', pinput[[i]]$trend), "neutral")
pinput[[i]]$overall_trend <- replace(pinput[[i]]$overall_trend, grep('negative', pinput[[i]]$trend), "negative")
pinput[[i]]$overall_trend <- replace(pinput[[i]]$overall_trend, grep('positive', pinput[[i]]$trend), "positive")

# reclass trend to factor
lvls[[i]] <- unique(unlist(trend[[i]]$trend)) 
pinput[[i]]$overall_trend <- factor(pinput[[i]]$overall_trend, levels = lvls[[i]])

# get ids
pinput[[i]]$id <- sub("neutral|positive|negative", "", pinput[[i]]$trend)
pinput[[i]]$id <- ifelse(pinput[[i]]$id == "", 
pinput[[i]]$trend, pinput[[i]]$id
)

pinput_individual[[i]] <- subset(pinput[[i]], pinput[[i]]$lvl != "section")
pinput_section[[i]] <- subset(pinput[[i]], pinput[[i]]$lvl != "individual")

# calculate label positions
pinput_individual[[i]] <- pinput_individual[[i]] %>%
  arrange(lvl, rev(trend))
# Calculate y position, placing it in the middle
pinput_individual[[i]] <- pinput_individual[[i]] %>%
  group_by(lvl) %>%
  mutate(label_y = cumsum(perc) - 0.5 * perc)
pinput_individual[[i]]$label_y <- replace(pinput_individual[[i]]$label_y, pinput_individual[[i]]$lvl == "individual", NA)

# add species column
species <- names(inputSpecies)
pinput_individual[[i]]$species <- rep(species[i], nrow(pinput_individual[[i]]))

# get site from id
pinput_individual[[i]]$site <- gsub("[[:digit:]]\\b", "", pinput_individual[[i]]$id)

# generate individual color palettes
palette_site[[i]] <- c(colfunc(length(unique(pinput_individual[[i]]$site))-length(lvls[[i]])), paletteTrends)
palette_section[[i]] <- c(colfunc(length(unique(pinput_section[[i]]$id))-length(lvls[[i]])), paletteTrends)
}

## Create pie charts (by site)
p <- list()
for(i in seq_along(pinput_individual)) {
p[[i]] <- ggplot(pinput_individual[[i]], aes(x = lvl, y = perc, fill = site, group = trend)) +
  geom_col(col = "white") +
  geom_text(aes(y = label_y, label = paste(round(perc, 1), "%")), col = "white") +
  scale_fill_manual(values = palette_site[[i]]) +
  scale_x_discrete(limits = c("", unique(pinput_individual[[i]]$lvl))) +
  scale_color_manual(values =  paletteTrends) +
  theme(legend.position = "right") +
  guides(fill=guide_legend(ncol=2)) +
  coord_polar("y") +
  labs(fill = "site", title = species[i]) +
  theme_void()
}

cairo_pdf("pieChart.pdf", width = 30, height = 10, pointsize = 10)  
ggpubr::ggarrange(plotlist = p,
          nrow = 1, common.legend = FALSE, align = "v") 
dev.off()


## Create pie charts (by section)
p2 <- list()
for(i in seq_along(pinput_section)) {
pinput_section[[i]]$id <- factor(pinput_section[[i]]$id, levels = c("stem", "base", "root", "negative", "neutral", "positive"))
p2[[i]] <- ggplot(pinput_section[[i]], aes(x = lvl, y = perc, fill = id, group = trend)) +
    geom_col(col = "white") +
 #   geom_text(aes(y = label_y, label = paste(round(perc, 1), "%")), col = "white") +
    scale_fill_manual(values = palette_section[[i]]) +
    scale_x_discrete(limits = c("", unique(pinput_section[[i]]$lvl))) +
  #  scale_color_manual(values =  c( "#FDC99A","#DB0201", "#9EA06F")) +
    theme(legend.position = "right") +
    coord_polar("y") +
    labs(fill = "section", title = species[i]) +
    theme_void()
}

# remove unneeded
names(p2) <- species

# ToDo: Make dynamic - exclude species with only one section
p2[["B. nana"]] <- NULL
p2[["R. ferrugineum" ]] <- NULL
cairo_pdf("pieChartSection.pdf", width = 8, height = 7, pointsize = 10)  
ggarrange(plotlist = p2,
          nrow = 1, common.legend = FALSE, align = "v") 
dev.off()
