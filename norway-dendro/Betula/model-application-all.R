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

iterations <- 100
# Model all
input <- na.omit(data_all[[1]])

# model_all <- list()

# for (i in c(1:iterations)) {
#     model_all[[i]] <- createModel(input)
#     print(paste("Sucess:", i))
# }

# ## Save models
# save(model_all, file = "~/workspaces/dendro/norway-dendro/Betula/output/model_all")


load("~/workspaces/dendro/norway-dendro/Betula/output/model_all")
test.features <- list()
test.target <- list()
predictions <- list()
predictions_all <- list()
model.params_all <- list()
for (i in c(1:iterations)) {
    ## Model testing
    # subset test data
    test.features[[i]] <- subset(model_all[[i]][[3]], select = -c(RingWidth))
    test.target[[i]] <- subset(model_all[[i]][[3]], select = RingWidth)[, 1]

    predictions[[i]] <- predict(model_all[[i]][[1]], newdata = test.features[[i]])
    predictions_all[[i]] <- predict(model_all[[i]][[1]])
    model.params_all[[i]] <- c(
        # RMSE
        sqrt(mean((test.target[[i]] - predictions[[i]])^2)),
        # R2
        cor(test.target[[i]], predictions[[i]])^2,
        # Mean absolute error (MAE) test
        mae(model_all[[i]][[3]]$RingWidth, predictions[[i]]),
        # Mean absolute error (MAE)
        mae(input$RingWidth, predictions_all[[i]])
    )
    print(paste("Sucess:", i))
}
model.params_all <- as.data.frame(do.call(rbind, model.params_all))
colnames(model.params_all) <- c("RMSE", "R2", "MAE_test", "MAE_full")



# Model pre 1995
input <- na.omit(data_all[[1]])
input <- subset(input, input$year < 1995)

# model_pre1995 <- list()

# for (i in c(1:iterations)) {
#     model_pre1995[[i]] <- createModel(input)
#     print(paste("Sucess:", i))
# }

# # Save models
# save(model_pre1995, file = "~/workspaces/dendro/norway-dendro/Betula/output/model_pre1995")

load("~/workspaces/dendro/norway-dendro/Betula/output/model_pre1995")
test.features <- list()
test.target <- list()
predictions <- list()
predictions_all <- list()
model.params_pre1995 <- list()
for (i in c(1:iterations)) {
    ## Model testing
    # subset test data
    test.features[[i]] <- subset(model_pre1995[[i]][[3]], select = -c(RingWidth))
    test.target[[i]] <- subset(model_pre1995[[i]][[3]], select = RingWidth)[, 1]

    predictions[[i]] <- predict(model_pre1995[[i]][[1]], newdata = test.features[[i]])
    predictions_all[[i]] <- predict(model_pre1995[[i]][[1]])
    model.params_pre1995[[i]] <- c(
        # RMSE
        sqrt(mean((test.target[[i]] - predictions[[i]])^2)),
        # R2
        cor(test.target[[i]], predictions[[i]])^2,
        # Mean absolute error (MAE) test
        mae(model_pre1995[[i]][[3]]$RingWidth, predictions[[i]]),
        # Mean absolute error (MAE)
        mae(input$RingWidth, predictions_all[[i]])
    )
    print(paste("Sucess:", i))
}
model.params_pre1995 <- as.data.frame(do.call(rbind, model.params_pre1995))
colnames(model.params_pre1995) <- c("RMSE", "R2", "MAE_test", "MAE_full")

# Model after 1995
input <- na.omit(data_all[[1]])
input <- subset(input, input$year >= 1995)

# model_post1995 <- list()

# for (i in c(1:iterations)) {
#     model_post1995[[i]] <- createModel(input)
#     print(paste("Sucess:", i))
# }

# # Save models
# save(model_post1995, file = "~/workspaces/dendro/norway-dendro/Betula/output/model_post1995")

load("~/workspaces/dendro/norway-dendro/Betula/output/model_post1995")
test.features <- list()
test.target <- list()
predictions <- list()
predictions_all <- list()
model.params_post1995 <- list()
for (i in c(1:iterations)) {
    ## Model testing
    # subset test data
    test.features[[i]] <- subset(model_post1995[[i]][[3]], select = -c(RingWidth))
    test.target[[i]] <- subset(model_post1995[[i]][[3]], select = RingWidth)[, 1]

    predictions[[i]] <- predict(model_post1995[[i]][[1]], newdata = test.features[[i]])
    predictions_all[[i]] <- predict(model_post1995[[i]][[1]])
    model.params_post1995[[i]] <- c(
        # RMSE
        sqrt(mean((test.target[[i]] - predictions[[i]])^2)),
        # R2
        cor(test.target[[i]], predictions[[i]])^2,
        # Mean absolute error (MAE) test
        mae(model_post1995[[i]][[3]]$RingWidth, predictions[[i]]),
        # Mean absolute error (MAE)
        mae(input$RingWidth, predictions_all[[i]])
    )
    print(paste("Sucess:", i))
}
model.params_post1995 <- as.data.frame(do.call(rbind, model.params_post1995))
colnames(model.params_post1995) <- c("RMSE", "R2", "MAE_test", "MAE_full")

# Plot model params
model.params <- as.data.frame(rbind(model.params_all, model.params_post1995, model.params_post1995))
model.params$model <- rep(c("Full model", "Pre 1995", "Post 1995"), each = iterations)

p.model.params <- data.frame(
    y = c(
        model.params$RMSE,
        model.params$R2 * 100,
        model.params$MAE_test,
        model.params$MAE_full
    ),
    x = rep(c(1:iterations), 4),
    model = rep(model.params$model, 4),
    var = rep(c("RMSE", "R2 (*100)", "MAE_test", "MAE_full"), each = nrow(model.params))
)

p.model.params$var <- factor(p.model.params$var, levels = c("R2 (*100)", "RMSE", "MAE_test", "MAE_full"))
p.params <-
    ggplot(p.model.params) +
    geom_col(
        aes(
            x = as.numeric(x),
            y = y,
            group = paste(var, model),
            fill = var
        ),
        position = "dodge"
    ) +
    theme_clean() +
    scale_fill_manual(values = palette2) +
    scale_color_manual(values = palette2) +
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
    facet_wrap(~model, scale = "free", nrow = 4)
p.params

cairo_pdf("model_selection.pdf", width = 25, height = 10, pointsize = 10)
p.params
dev.off()

test <- subset(p.model.params, p.model.params$model == "Full model" & p.model.params$var == "MAE_full")
test2 <- subset(test, test$y == min(test$y))

test <- subset(p.model.params, p.model.params$model == "Full model" & p.model.params$var == "MAE_test")
test2 <- subset(test, test$y == min(test$y))

test <- subset(p.model.params, p.model.params$model == "Full model" & p.model.params$var == "R2 (*100)")
test2 <- subset(test, test$y == max(test$y))

## Savefinal models
finalModels <- list(model_all[[69]], model_pre1995[[79]], model_post1995[[64]])
save(finalModels, file = "~/workspaces/dendro/norway-dendro/Betula/output/final-models")




####

load("~/workspaces/dendro/norway-dendro/Betula/output/final-models")
finalModel_all <- finalModels[[1]]
finalModel_pre1995 <- finalModels[[2]]
finalModel_post1995 <- finalModels[[3]]


## Plot relative importance
summary_all <- summary(finalModel_all[[1]])
summary_pre1995 <- summary(finalModel_pre1995[[1]])
summary_post1995 <- summary(finalModel_post1995[[1]])

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

# relative_importance <- subset(relative_importance, rel.inf > 2)
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

pdf("relative_importance.pdf", width = 10, height = 12, pointsize = 12)
plot_rel_imp
dev.off()





# relative importance for all iterations

####

relative_importance <- list()
for (i in c(1:iterations)) {
    ## Plot relative importance
    relative_importance[[i]] <- as.data.frame(rbind(
        summary(model_all[[i]][[1]]),
        summary(model_pre1995[[i]][[1]]),
        summary(model_post1995[[i]][[1]])
    ))

    relative_importance[[i]]$model <- c(
        rep("Full model", nrow(summary(model_all[[i]][[1]]))),
        rep("Pre 1995", nrow(summary(model_pre1995[[i]][[1]]))),
        rep("Post 1995", nrow(summary(model_post1995[[i]][[1]])))
    )

    relative_importance[[i]] <- subset(
        relative_importance[[i]],
        relative_importance[[i]]$rel.inf > 0
    )

    relative_importance[[i]]$group <- sapply(strsplit(relative_importance[[i]]$var, "[_]"), "[[", 1)

    relative_importance[[i]]$dataset <- rep("Current", nrow(relative_importance[[i]]))
    relative_importance[[i]]$dataset <- replace(relative_importance[[i]]$dataset, grep("preyear", relative_importance[[i]]$var), "PreGrowth")
    relative_importance[[i]]$dataset <- replace(relative_importance[[i]]$dataset, grep("winter", relative_importance[[i]]$var), "PreGrowth")

    relative_importance[[i]] <- subset(relative_importance[[i]], rel.inf > 2)

    relative_importance[[i]]$iteration <- rep(i, nrow(relative_importance[[i]]))
}
relative_importance <- as.data.frame(do.call(rbind, relative_importance))
View(relative_importance)


# Create Plot
# how ring width (y axis) changes as a function of the variable in question
relative_importance <- subset(relative_importance, rel.inf > 10)
plot_rel_imp <- ggplot(relative_importance) +
    geom_boxplot(
        aes(
            x = reorder_within(var, rel.inf, model, ),
            y = rel.inf,
            fill = group,
            col = dataset
        )
    ) +
    scale_x_reordered(labels = reorder_func) +
    scale_color_manual(values = c("grey", "black")) +
    coord_flip() +
    theme_clean() +
    scale_fill_manual(values = palette3) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        x = "",
        y = "Importance Value [%]",
        fill = "",
        pattern = "",
        col = ""
    ) +
    theme(
        legend.background = element_rect(color = NA),
        plot.background = element_rect(color = NA)
    ) +
    facet_wrap(~model, scale = "free")
plot_rel_imp

pdf("relative_importance_boxplot.pdf", width = 12, height = 6, pointsize = 12)
plot_rel_imp
dev.off()

# partial dependence plots
# The partial dependence plots indicate the relationship of the predictor variable “X” with the response variable “Y”, as well as the former’s dependence on the latter.

# get dependance values
# model <- finalModel_all[[1]]
getDependencies <- function(model) {
    var <- unique(summary(model)$var)

    dependance <- list()
    for (i in c(1:length(var))) {
        dependance[[i]] <- data.frame(
            x = plot(model, i.var = c(var[i]), smooth = T)$panel.args[[1]][[1]],
            y = plot(model, i.var = c(var[i]), smooth = T)$panel.args[[1]][[2]],
            var = rep(var[i], 100)
        )
    }
    dependance <- do.call(rbind, dependance)
    dependance
}

dependencies_all <- list()
dependencies_pre1995 <- list()
dependencies_post1995 <- list()
dependencies <- list()
for (i in c(1:iterations)) {
    dependencies_all[[i]] <- getDependencies(model_all[[i]][[1]])
    dependencies_pre1995[[i]] <- getDependencies(model_pre1995[[i]][[1]])
    dependencies_post1995[[i]] <- getDependencies(model_post1995[[i]][[1]])

    dependencies[[i]] <- as.data.frame(rbind(dependencies_all[[i]], dependencies_pre1995[[i]], dependencies_post1995[[i]]))
    dependencies[[i]]$group <- c(
        rep("Full model", nrow(dependencies_all[[i]])),
        rep("Pre 1995", nrow(dependencies_pre1995[[i]])),
        rep("Post 1995", nrow(dependencies_post1995[[i]]))
    )
    dependencies[[i]]$group2 <- paste0(dependencies[[i]]$group, dependencies[[i]]$var, dependencies[[i]]$x)
    dependencies[[i]]$group3 <- paste0(dependencies[[i]]$group, dependencies[[i]]$var)


}
View(dependencies[[2]])



dependencies <- as.data.frame(do.call(rbind, dependencies))

dependencies_mean <- aggregate(dependencies, by = list(dependencies$group2), function(x) mean(x, na.rm = T))
dependencies_mean$var <- aggregate(dependencies, by = list(dependencies$group2), first)$var
dependencies_mean$group <- aggregate(dependencies, by = list(dependencies$group2), first)$group


inputOptCond <- split(dependencies_mean, list(dependencies_mean$var, dependencies_mean$group))
optCond <- lapply(inputOptCond, function(z) {
    subset(z, z$y == max(z$y))
})

rect <- lapply(inputOptCond, function(z) {
    data.frame(
        xmin = ifelse(length(
            na.omit(subset(z, z$y == max(z$y))$x)
        ) > 0,
        min(na.omit(subset(z, z$y == max(z$y))$x)),
        NA
        ),
        xmax = ifelse(length(
            na.omit(subset(z, z$y == max(z$y))$x)
        ) > 0,
        max(na.omit(subset(z, z$y == max(z$y))$x)),
        NA
        )
    )
})

rect <- as.data.frame(do.call(rbind, rect))
rect$var <- sapply(strsplit(rownames(rect), "[.]"), "[[", 1)
rect$group <- sapply(strsplit(rownames(rect), "[.]"), "[[", 2)


# # unscale
# input <- na.omit(data_all[[1]])
# input <- input[-c(1:3)]
# input <- as.data.frame(sapply(input, function(x) replace(x, is.infinite(x), 0)))
# input <- na.omit(input)
# # input <- as.data.frame(scale(input))

# test <- split(dependencies_all, dependencies_all$var)
# x <- test[["AT_mean_summer"]]$x
# y <- input$AT_mean_summer

# max(y)
# min(y)
# plot(model, i.var = c(var[2]), smooth = T)
# max(x)
# min(x)
# unscaled <- x * sqrt((sum((x - mean(y, na.rm = T))^2)) / (length(y) - 1))
# # then add back mean:

# unscaled <- unscaled + mean(y, na.rm = T)

# get density
getDensity <- function(dependenvies, input) {
    var <- unique(dependencies$var)

    density <- list()
    for (i in c(1:length(var))) {
        density[[i]] <- data.frame(
            x = density(as.numeric(input[var[i]][, 1]))$x,
            y = density(input[var[i]][, 1])$y,
            var = rep(var[i], 512)
        )
    }
    density <- do.call(rbind, density)
    density
}

input <- na.omit(data_all[[1]])
density_all <- getDensity(subset(dependencies_mean, dependencies_mean$group == "Full model"), input)

input <- na.omit(data_all[[1]])
input <- subset(input, input$year < 1995)
density_pre1995 <- getDensity(subset(dependencies_mean, dependencies_mean$group == "Pre 1995"), input)

input <- na.omit(data_all[[1]])
input <- subset(input, input$year >= 1995)
density_post1995 <- getDensity(subset(dependencies_mean, dependencies_mean$group == "Post 1995"), input)

density <- as.data.frame(rbind(density_all, density_pre1995, density_post1995))
density$group <- c(
    rep("Full model", nrow(density_all)),
    rep("Pre 1995", nrow(density_pre1995)),
    rep("Post 1995", nrow(density_post1995))
)
View(density)

select <- aggregate(relative_importance$rel.inf, list(relative_importance$var), FUN = mean)
select <- select[order(select$x, decreasing = T), ]
var <- select[c(1:8), ]$Group.1

dependencies_selected <- dependencies_mean[dependencies_mean$var %in% var, ]
density_selected <- density[density$var %in% var, ]


# scale for plot
scale_y <- function(first, second) {
    # scale and shift variables calculated based on desired mins and maxes
    scale <- (max(second) - min(second)) / (max(first) - min(first))
    shift <- min(first) - min(second)

    # scale secondary variable values
    return(second / scale)
}

p_dependencies <- split(dependencies_selected, dependencies_selected$var)
p_density <- split(density_selected, density_selected$var)

density_scaled <- list()
for (i in seq_along(p_dependencies)) {
    density_scaled[[i]] <- scale_y(p_dependencies[[i]]$y, p_density[[i]]$y)
}
density_selected$y_scaled <- unlist(density_scaled)

ri <- relative_importance[relative_importance$var %in% var, ]
ri <- split(ri, list(ri$var, ri$model))
n <- length(unique(dependencies_selected$var)) * length(unique(dependencies_selected$group))
annotations <- data.frame(
    x = rep(-Inf, n),
    y = rep(Inf, n),
    var = rep(unique(dependencies_selected$var), length(unique(dependencies_selected$group))),
    group = rep(unique(dependencies_selected$group), each = length(unique(dependencies_selected$var)))
)
annotations <- split(annotations, list(annotations$var, annotations$group))
annotations <- as.data.frame(do.call(rbind, annotations))
ri <- lapply(ri, function(x) mean(x$rel.inf, na.rm = TRUE))
ri <- unlist(ri)
annotations$label <- paste("RI =", round(ri, digits = 2))
annotations$ri <- ri

dependencies_selected$var2 <- sapply(strsplit(dependencies_selected$var, "[_]"), "[[", 1)
density_selected$var2 <- sapply(strsplit(density_selected$var, "[_]"), "[[", 1)

annotations <- subset(annotations, is.na(annotations$ri) == FALSE)

rect_selected <- rect[rect$var %in% var, ]

annotations$group2 <- paste0(annotations$var, annotations$group)
rect_selected$group2 <- paste0(rect_selected$var, rect_selected$group)
dependencies_selected$group2 <- paste0(dependencies_selected$var, dependencies_selected$group)
density_selected$group2 <- paste0(density_selected$var, density_selected$group)

var2 <- unique(annotations$group2)
dependencies_selected <- dependencies_selected[dependencies_selected$group2 %in% var2,]
density_selected <- density_selected[density_selected$group2 %in% var2,]
rect_selected <- rect_selected[rect_selected$group2 %in% var2,]


partial_dependence <- ggplot(dependencies_selected) +
    geom_rect(data = rect_selected, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +
    geom_area(data = density_selected, aes(x = x, y = y_scaled, fill = var2), alpha = 0.7) +
    #geom_line(aes(x = x, y = y), col = palette[6]) +
    geom_smooth(aes(x = x, y = y - (min(y))), col = "black", method = "gam") +
    scale_fill_manual(values = palette4) +
    facet_grid2(var ~ group, axes = "x", scale = "free", independent = "x", drop = T, render_empty = F) +
    geom_text(data = annotations, aes(x = x, y = y, hjust = -0.1, vjust = 2, label = label)) +
    theme_clean() +
    labs(x = "", y = "", fill = "", title = "Patial dependence plots")
partial_dependence

pdf("partial_dependence_plot.pdf", width = 12, height = 14, pointsize = 12)
partial_dependence
dev.off()

partial_dependence <- ggplot(dependencies_selected) +
    geom_area(data = density_selected, aes(x = x, y = y_scaled, fill = group, group = group), alpha = 0.7) +
    geom_line(aes(x = x, y = y), col = palette[6]) +
    geom_smooth(aes(x = x, y = y - (min(y)), group = group, col = group), col = "black", method = "gam") +
    scale_fill_manual(values = palette4) +
    scale_color_manual(values = palette4) +
    facet_grid2(~var, axes = "x", scale = "free", independent = "x", drop = T, render_empty = F) +
    geom_text(data = annotations, aes(x = x, y = y, hjust = -0.1, vjust = 2, label = label)) +
    theme_clean() +
    labs(x = "", y = "", fill = "", title = "Patial dependence plots")
partial_dependence

pdf("partial_dependence_plot_combined.pdf", width = 12, height = 12, pointsize = 12)
partial_dependence
dev.off()
