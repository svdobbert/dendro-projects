# input data
# data: all data
# data_reduced: data by growing season, winter, spring
# data_regional: regional data only, by month
# data_at: monthly air temperature
# data_p: monthly p
# data_era5: ERA5 data
# data_oscelations

# data_all <- list(data,
#                  data_regional,
#                  data_oscelations,
#                  data_era5,
#                  data_at,
#                  data_p)

# remove missing values from input

# Model all
input <- na.omit(data_all[[1]])

finalModel_all <- createModel(input)
summary(finalModel_all)

# Model pre 1995
input <- na.omit(data_all[[1]])
input <- subset(input, input$year < 1995)

finalModel_pre1995 <- createModel(input)
summary(finalModel_pre1995)

# Model after 1995
input <- na.omit(data_all[[1]])
input <- subset(input, input$year >= 1995)

finalModel_post1995 <- createModel(input)
summary(finalModel_post1995)

## Plot relative importance
summary_all <- summary(finalModel_all)
summary_pre1995 <- summary(finalModel_pre1995)
summary_post1995 <- summary(finalModel_post1995)

relative_importance <- as.data.frame(rbind(
    summary_all,
    summary_pre1995,
    summary_post1995
))

relative_importance$model <- c(
    rep("Full model", nrow(summary_all)),
    rep("Pre 1995", nrow(summary_pre1995)),
    rep("Post 1995", nrow(summary_post1995))
)

relative_importance <- subset(
    relative_importance,
    relative_importance$rel.inf > 0
)

relative_importance$group <- sapply(strsplit(relative_importance$var, "[_]"), "[[", 1)
View(relative_importance)

relative_importance$dataset <- rep("Current", nrow(relative_importance))
relative_importance$dataset <- replace(relative_importance$dataset, grep("preyear", relative_importance$var), "PreGrowth")
relative_importance$dataset <- replace(relative_importance$dataset, grep("winter", relative_importance$var), "PreGrowth")

# Create Plot
plot_rel_imp <- ggplot(relative_importance) +
    geom_col_pattern(
        aes(
            x = reorder_within(var, rel.inf, model, ),
            y = rel.inf,
            group = var,
            fill = group,
            pattern = dataset,
        ),
        color = "black",
        pattern_fill = "black",
        pattern_angle = 45,
        pattern_density = 0.1,
        pattern_spacing = 0.05,
        pattern_key_scale_factor = 0.6
    ) +
    scale_pattern_manual(values = c(PreGrowth = "stripe", Current = "none")) +
    guides(
        pattern = guide_legend(override.aes = list(fill = "white")),
        fill = guide_legend(override.aes = list(pattern = "none"))
    ) +
    scale_x_reordered(labels = reorder_func) +
    coord_flip() +
    theme_clean() +
    scale_fill_manual(values = palette) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        x = "",
        y = "Importance Value [%]",
        fill = "",
        pattern = ""
    ) +
    theme(
        legend.background = element_rect(color = NA),
        plot.background = element_rect(color = NA)
    ) +
facet_wrap(~model, scale = "free")
plot_rel_imp

cairo_pdf("relative_importance.pdf", width = 4, height = 8, pointsize = 12)
plot_rel_imp
dev.off()
