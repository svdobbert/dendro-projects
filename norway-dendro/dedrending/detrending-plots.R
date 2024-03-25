# plot individual curves as (annual) boxplots
inputIndividualCurves <- subset(inputProcess, inputProcess$var == "Individual curves")
#pinput1$value <- replace(pinput1$value, pinput1$value < 10, NA)

p <- ggplot(inputIndividualCurves) +
  geom_boxplot(aes(x = as.numeric(year), y = value, group = year, col = var), alpha = .3) +
  geom_text(data = labels, aes(x = Inf, y = Inf, hjust = 1.1, vjust = 1.2, label = label)) +
  scale_alpha_manual(values = c(0.1)) +
  scale_color_manual(values = c(palette[2], palette[6])) +
  facet_wrap(~ species, scale = "free_y") +
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Calendar year", y = "Ring width [μm]", col = "", alpha = "") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank()) 
p 

# plot by biological age (lines and mean)
p1 <- ggplot(inputProcess) +
  geom_line(aes(x = age, y = value, group = id, col = var, alpha = var)) +
  #geom_point(aes(x = age, y = value)) +
  scale_alpha_manual(values = c(1, 0.1)) +
  scale_color_manual(values = c(palette[3], palette[2], palette[6])) +
  geom_pointrange(data = subset(inputProcess, inputProcess$var == "Biweigth robust mean series"),aes(x = age, y = value, ymin = value - se, ymax = value + se), col = palette[3], size = 0.05) +
  facet_wrap(~ species, scale = "free_y") +
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Biological age [year]", y = "Ring width [μm]", col = "", alpha = "") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.spacing.y = unit(0, "pt"),
        legend.justification = c(1.02, 1.08), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, 'cm'),
        legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
        legend.position = c(1, 1)) 
p1  


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
for(i in seq_along(inputSpeciesMean)) {
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
p2 <- ggplot(pinputTrend) +
  geom_line(aes(x = age, y = value, group = var, col = var), size = 0.5) +
  geom_pointrange(aes(x = age, y = value, ymin = value -se, ymax = value + se, col = var), size = 0.05) +
  scale_color_manual(values = c(palette[3], palette[2], palette[1]))+
  scale_fill_manual(values = c(palette[3], palette[2], palette[1]))+
  facet_wrap(~ species, scale = "free_y") +
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Biological age [years]", y = "Ring width [μm]", col = "", alpha = "") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.spacing.y = unit(0, "pt"),
        legend.justification = c(1.02, 1.08), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, 'cm'),
        legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
        legend.position = c(1, 1)) 
p2


# Plot Ring width indices (RWI) from individual curves
pinputRWI <- data.frame(age = rep(input_age_mean_species$age, 4),
                    species = rep(input_age_mean_species$species, 4),
                    tbrm = rep(input_age_mean_species$value, 4),
                    var = rep(c("Biweigth robust mean series", "Smoothing spline curve (mean)", "Negative exponential model curve (mean)", "Hugershoff model curve (mean)"), each = nrow(input_age_mean_species)),
                    value = c(input_age_mean_species$value, spline, exp, hug),
                    se = c(input_age_mean_species$se, spline_se, exp_se, hug_se))                
pinputRWI $var <- factor(pinputRWI$var, levels = c("Biweigth robust mean series", "Smoothing spline curve (mean)", "Negative exponential model curve (mean)",  "Hugershoff model curve (mean)"))


p3 <- ggplot(pinputRWI ) +
  geom_line(aes(x = age, y = value, group = var, col = var), size = 0.5) +
  geom_pointrange(aes(x = age, y = value, ymin = value -se, ymax = value + se, col = var), size = 0.05) +
  scale_color_manual(values = c(palette[3], palette[4], palette[5], palette[6], palette[7]))+
  facet_wrap(~ species, scale = "free_y") +
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Biological age [years]", y = "Ring width [μm]", col = "", alpha = "") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.justification = c(1.02, 1.08), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, 'cm'),
        legend.spacing.y = unit(0, "pt"),
        legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
        legend.position = c(1, 1)) 
p3


####

# ToDo


# detrended curve (mean)
#input <- split(input_age, input_age$id)
input <- subset(input_age, input_age$age <= 20)
input$value <- replace(input$value, input$species == "S. herbacea" & input$age > 10, NA)
input <- split(input, input$species)
input[[1]]$curves_rcs_spline <- rep(smooth[1:(length(smooth)/3)], length(unique(input[[1]]$id)))
input[[2]]$curves_rcs_spline <- rep(smooth[((length(smooth)/3)+1):(((length(smooth)/3)*2))], length(unique(input[[2]]$id)))
input[[3]]$curves_rcs_spline <- rep(smooth[(((length(smooth)/3)*2)+1):length(smooth)], length(unique(input[[3]]$id)))

input[[1]]$curves_rcs_lin <- rep(linear[1:(length(smooth)/3)], length(unique(input[[1]]$id)))
input[[2]]$curves_rcs_lin <- rep(linear[((length(smooth)/3)+1):(((length(smooth)/3)*2))], length(unique(input[[2]]$id)))
input[[3]]$curves_rcs_lin <- rep(linear[(((length(smooth)/3)*2)+1):length(smooth)], length(unique(input[[3]]$id)))

input[[1]]$curves_rcs <- rep(input_test$value[1:(length(smooth)/3)], length(unique(input[[1]]$id)))
input[[2]]$curves_rcs <- rep(input_test$value[((length(smooth)/3)+1):(((length(smooth)/3)*2))], length(unique(input[[2]]$id)))
input[[3]]$curves_rcs <- rep(input_test$value[(((length(smooth)/3)*2)+1):length(smooth)], length(unique(input[[3]]$id)))

input_age <- as.data.frame(do.call(rbind, input))
input_age$series_rcs_spline <- input_age$value/input_age$curves_rcs_spline
input_age$series_rcs_lin <- input_age$value/input_age$curves_rcs_lin
input_age$series_rcs <- input_age$value/input_age$curves_rcs

input_age$value2 <- input_age$value/100
min(input_age$year)
pinput <- aggregate(input_age, by = list(year = input_age$year, species = input_age$species), function(x) mean(x, na.rm = TRUE), drop=FALSE)
pinput_se <- aggregate(input_age, by = list(year = input_age$year, species = input_age$species), function(x) sd(x, na.rm = TRUE)/sqrt(length(x)), drop=FALSE)

pinput <- data.frame(detrended = c(pinput$value2, pinput$series_spline, pinput$series_negexp, pinput$series_hug, pinput$series_rcs_spline, pinput$series_rcs_lin, pinput$series_rcs),
                     se = c(pinput_se$value2, pinput_se$series_spline, pinput_se$series_negexp, pinput_se$series_hug, pinput_se$series_rcs_spline, pinput_se$series_rcs_lin, pinput_se$series_rcs),
                     var = rep(c("Mean series", "Ring width index (smoothing spline)", "Ring width index (negative exponential model)", "Ring width index (Hugershoff model)", "RCS chronology (smoothing spline)", "RCS chronology (linear trend)", "RCS chronology (raw)"), each = nrow(input_age_mean_species)),
                     year = rep(pinput$year, 7),
                     species = rep(pinput$species, 7))

pinput2 <- subset(pinput, pinput$species != "B. nana" | pinput$detrended < 6)                     
p4 <- ggplot(pinput2) +
  geom_line(aes(x = year, y = detrended, group = var, col = var), size = 0.5) +
  geom_line(data = subset(pinput2, pinput2$var == "Mean series"), aes(x = year, y = detrended, group = var), size = .8, col = palette[3]) +
  geom_pointrange(aes(x = year, y = detrended, ymin = detrended -se, ymax = detrended + se, col = var), size = 0.05, alpha = .5) +
  scale_color_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4]))+
  scale_fill_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4]))+
  facet_wrap(~ species, scale = "free_y") +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(sec.axis = dup_axis(trans=~.*100, name = "Ring width (series) [μm]"))+
  labs(x = "Calender year", y = "Ring width index/RCS chronology", col = "", alpha = "") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.justification = c(1.02, 1.08), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, 'cm'),
        legend.spacing.y = unit(0, "pt"),
        legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
        legend.position = c(1, 1)) 
p4

input <- split(pinput, pinput$var)
for(i in c(1:length(input))) {
  input[[i]]$difference <- input[[i]]$detrended - input[[1]]$detrended
}
input <- as.data.frame(do.call(rbind, input))

input2 <- subset(input, input$species != "B. nana" | input$difference < 4) 
input2 <- subset(input2, input2$var != "Mean series")
p5 <- ggplot(input2) +
  geom_hline(yintercept = 0) +
 # geom_area(aes(x = year, y = difference, group = var, fill = var),position = "jitter", alpha = 0.1) +
  geom_line(aes(x = year, y = difference, group = var, col = var), size = 0.5) +
  #geom_line(data = subset(pinput, pinput$var == "Mean series"), aes(x = year, y = detrended, group = var), size = .8, col = palette[3]) +
  #geom_pointrange(aes(x = year, y = difference, ymin = difference -se, ymax = difference + se, col = var), size = 0.05, alpha = .5) +
  scale_color_manual(values = c(palette[2], palette[7], palette[1], palette[6], palette[5], palette[4]))+
  scale_fill_manual(values = c(palette[2], palette[7], palette[1], palette[6], palette[5], palette[4]))+
  facet_wrap(~ species, scale = "free_y") +
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Calender year", y = "Difference to mean series", col = "", alpha = "") +
  theme(legend.position = "bottom") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.justification = c(1.02, 1.08), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, 'cm'),
        legend.spacing.y = unit(0, "pt"),
        legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
        legend.position = c(1, 1)) 
p5


cairo_pdf("detrending3.pdf", width = 14, height = 12, pointsize = 10)  
ggarrange(plotlist = list(p2, p3, p4),
          ncol = 1, common.legend = FALSE, align = "v", labels = c("a", "b", "c")) 
dev.off()

cairo_pdf("detrending3.pdf", width = 12, height = 18/5, pointsize = 10)  
p5
dev.off()

cairo_pdf("detrending_all.pdf", width = 12, height = 22, pointsize = 10)  
ggarrange(plotlist = list(sample_depth, boxplot, comp, p1, p2, p3, p4),legend = "right",
          ncol = 1, common.legend = FALSE, align = "v", labels = c("a", "b", "c", "d", "e", "f", "g")) 
dev.off()


# individual curve example
pinput <- subset(input_age, input_age$species != "B. nana")
pinput <- split(pinput, pinput$id)
pinput <- pinput[[102]]

trend <- data.frame(age = rep(pinput$age, 3),
                    var = rep(c("Individual curve", "RCS curve (spline)", "RCS curve (linear trend)"), each = nrow(pinput)),
                    value = c(pinput$value, pinput$curves_rcs_spline, pinput$curves_rcs_lin)) 


p2 <- ggplot(trend) +
  geom_line(aes(x = age, y = value, group = var, col = var), size = 0.5) +
  scale_color_manual(values = c(palette[3], palette[2], palette[1]))+
  scale_fill_manual(values = c(palette[3], palette[2], palette[1]))+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "", y = "", col = "", alpha = "", title = "Individual curve") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 1),
        legend.justification = c(1.02, 1.08), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, 'cm'),
        legend.spacing.y = unit(0, "pt"),
        legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
        legend.position = c(1, 1)) 
p2

trend2 <- data.frame(age = rep(pinput$age, 4),
                     var = rep(c("Individual curve", "Smoothing spline curve", "Negative exponential model curve", "Hugershoff model curve"), each = nrow(pinput)),
                     value = c(pinput$value, pinput$curves_spline, pinput$curves_negexp, pinput$curves_hug))                

trend2$var <- factor(trend2$var, levels = c("Individual curve", "Smoothing spline curve", "Negative exponential model curve",  "Hugershoff model curve"))



p3 <- ggplot(trend2) +
  geom_line(aes(x = age, y = value, group = var, col = var), size = 0.5) +
  scale_color_manual(values = c(palette[3], palette[4], palette[5], palette[6], palette[7]))+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "", y = "", col = "", alpha = "", title = "Individual curve") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 1),
        legend.justification = c(1.02, 1.08), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, 'cm'),
        legend.spacing.y = unit(0, "pt"),
        legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
        legend.position = c(1, 1)) 
p3

pinput <- data.frame(detrended = c(pinput$value2, pinput$series_spline, pinput$series_negexp, pinput$series_hug, pinput$series_rcs_spline, pinput$series_rcs_lin, pinput$series_rcs),
                     var = rep(c("Individual curve", "Ring width index (smoothing spline)", "Ring width index (negative exponential model)", "Ring width index (Hugershoff model)", "RCS chronology (smoothing spline)", "RCS chronology (linear trend)", "RCS chronology (raw)"), each = nrow(pinput)),
                     year = rep(pinput$year, 7))
p4 <- ggplot(pinput) +
  geom_line(aes(x = year, y = detrended, group = var, col = var), size = 0.5) +
  geom_line(data = subset(pinput, pinput$var == "Mean series"), aes(x = year, y = detrended, group = var), size = .8, col = palette[3]) +
  scale_color_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4]))+
  scale_fill_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4]))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(sec.axis = dup_axis(trans=~.*100, name = ""))+
  labs(x = "", y = "", col = "", alpha = "", title = "Individual curve") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 1),
        legend.justification = c(1.02, 1.08), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, 'cm'),
        legend.spacing.y = unit(0, "pt"),
        legend.box.background = element_rect(colour = "black", fill = alpha("white", 0.5)),
        legend.position = c(1, 1)) 
p4

cairo_pdf("detrending_example.pdf", width = 5, height = 10, pointsize = 10)  
ggarrange(plotlist = list(p2, p3, p4),
          ncol = 1, common.legend = FALSE, align = "v") 
dev.off()

input <- split(pinput, pinput$var)
for(i in c(1:length(input))) {
  input[[i]]$difference <- input[[i]]$detrended - input[[1]]$detrended
}
input <- as.data.frame(do.call(rbind, input))
p5 <- ggplot(input) +
  geom_line(aes(x = year, y = difference, group = var, col = var), size = 0.5) +
 # geom_line(data = subset(pinput, pinput$var == "Mean series"), aes(x = year, y = detrended, group = var), size = .8, col = palette[3]) +
  scale_color_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4]))+
  scale_fill_manual(values = c(palette[3], palette[2], palette[7], palette[1], palette[6], palette[5], palette[4]))+
  scale_x_continuous(expand = c(0,0))+
   labs(x = "Age", y = "Difference to inividual curve", col = "", alpha = "") +
  theme(legend.position = "bottom") +
  theme_bw() 
p5

cairo_pdf("detrending_example3.pdf", width = 8, height = 4, pointsize = 10)  
p5
dev.off()