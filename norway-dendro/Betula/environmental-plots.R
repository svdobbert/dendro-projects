## Get input
# Data
input <- na.omit(data_all[[1]])
input_pre1995 <- subset(input, input$year < 1995)
input_post1995 <- subset(input, input$year >= 1995)
# variables
inputVar <- var
# optimum conditions from model results
inputOptCond <- optCond
# relative importance


## Plot comparison pre and post 1995
median <- sapply(input[c(4:ncol(input))], function(x) median(x, na.rm = TRUE))
median_pre1995 <- sapply(input_pre1995[c(4:ncol(input_pre1995))], function(x) median(x, na.rm = TRUE))
median_post1995 <- sapply(input_post1995[c(4:ncol(input_post1995))], function(x) median(x, na.rm = TRUE))

median <- data.frame(
    var = colnames(input[c(4:ncol(input))]),
    median_all = median,
    median_pre1995 = median_pre1995,
    median_post1995 = median_post1995
)

median$diff <- median$median_post1995 - median_pre1995
median$diff_perc <- (median$diff / abs(median_pre1995)) * 100

median$group <- sapply(strsplit(median$var, "[_]"), "[[", 1)

median$dataset <- rep("Current", nrow(median))
median$dataset <- replace(median$dataset, grep("preyear", median$var), "PreGrowth")
median$dataset <- replace(median$dataset, grep("winter", median$var), "PreGrowth")


median_selected <- subset(median, median$diff_perc > 10 | median$diff_perc < -10)
var2 <- unique(median_selected$var)
median_diff <- ggplot(median_selected) +
    geom_col_pattern(
        aes(
            x = reorder(var, diff_perc),
            y = as.numeric(diff_perc),
            group = var,
            fill = group,
            pattern = dataset,
        ),
        color = "black",
        pattern_fill = "black",
        pattern_angle = 45,
        pattern_density = 0.1,
        pattern_spacing = 0.01,
        pattern_key_scale_factor = 0.6
    ) +
    scale_pattern_manual(values = c(PreGrowth = "stripe", Current = "none")) +
    guides(
        pattern = guide_legend(override.aes = list(fill = "white")),
        fill = guide_legend(override.aes = list(pattern = "none"))
    ) +
    scale_fill_manual(values = palette6) +
    labs(
        x = "",
        y = "Change post 1995 [%]",
        fill = "",
        pattern = ""
    ) +
    coord_flip() +
    theme_clean() +
    theme(
        legend.background = element_rect(color = NA),
        plot.background = element_rect(color = NA)
    )
median_diff

pdf("median_change_post1995.pdf", width = 10, height = 10, pointsize = 12)
median_diff
dev.off()

## Plot boxplots and optimum conditions
pinput <- data.frame(
    var = rep(colnames(input[c(4:ncol(input))]), each = nrow(input)),
    value = unlist(input[c(4:ncol(input))])
)
pinput$group <- rep("Full model", nrow(pinput))

pinput_pre1995 <- data.frame(
    var = rep(colnames(input[c(4:ncol(input))]), each = nrow(input_pre1995)),
    value = unlist(input_pre1995[c(4:ncol(input_pre1995))])
)
pinput_pre1995$group <- rep("Pre 1995", nrow(pinput_pre1995))

pinput_post1995 <- data.frame(
    var = rep(colnames(input[c(4:ncol(input))]), each = nrow(input_post1995)),
    value = unlist(input_post1995[c(4:ncol(input_post1995))])
)
pinput_post1995$group <- rep("Post 1995", nrow(pinput_post1995))

pinput <- as.data.frame(rbind(pinput, pinput_pre1995, pinput_post1995))
pinput <- pinput[pinput$var %in% inputVar, ]
pinput$group2 <- rep("", nrow(pinput))



# get optimum conditions for plot
pinput_optCond <- inputOptCond[inputOptCond$var %in% inputVar, ]
test <- subset(pinput_optCond, pinput_optCond$var == "P_winter")
pinput_optCond <- data.frame(
    var = pinput_optCond$var,
    value = pinput_optCond$x,
    group = pinput_optCond$group
)
# pinput_optCond <- subset(pinput_optCond, pinput_optCond$group != "Full model")
pinput_optCond$group2 <- rep("", nrow(pinput_optCond))
pinput_optCond <- subset(pinput_optCond, is.na(pinput_optCond$var) == FALSE)

MinMeanSEMMax <- function(x) {
    v <- c(
        ifelse(is.na(sd(x)) == TRUE, mean(x), mean(x) - sd(x) / sqrt(length(x))),
        ifelse(is.na(sd(x)) == TRUE, mean(x), mean(x) - sd(x) / sqrt(length(x))),
        NA,
        ifelse(is.na(sd(x)) == TRUE, mean(x), mean(x) + sd(x) / sqrt(length(x))),
        ifelse(is.na(sd(x)) == TRUE, mean(x), mean(x) + sd(x) / sqrt(length(x)))
    )
    names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
    v
}

optCondBoxplots <- ggplot(subset(pinput, pinput$group != "Full model")) +
    geom_boxplot(
        aes(
            x = var, y = value,
            group = group,
            fill = group,
            col = group,
        ),
        size = 0.8
    ) +
    stat_summary(
        data = subset(pinput_optCond, pinput_optCond$group != "Full model"),
        aes(
            x = var, y = value,
            group = group,
            col = group
        ),
        fun.data = MinMeanSEMMax, geom = "boxplot",
        fill = NA,
        size = 1
    ) +
    scale_fill_manual(values = palette7_light) +
    scale_color_manual(values = palette7) +
    labs(
        x = "",
        y = "",
        fill = "",
        col = "",
        pattern = ""
    ) +
    facet_grid2(var ~ group2, axes = "x", scale = "free", independent = "x", drop = T, render_empty = F) +
    coord_flip() +
    theme_clean() +
    theme(
        legend.background = element_rect(color = NA),
        plot.background = element_rect(color = NA),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
    )
optCondBoxplots

pdf("boxplots_optConditions.pdf", width = 9, height = 14, pointsize = 12)
optCondBoxplots
dev.off()

## Plot relative importance
pinputRelativeImportance <- inputRelativeImportance[inputRelativeImportance$var %in% inputVar, ]
pinputRelativeImportance$group <- pinputRelativeImportance$model
pinputRelativeImportance$group2 <- rep("", nrow(pinputRelativeImportance))

relImp <- ggplot(subset(pinputRelativeImportance, pinputRelativeImportance$group != "Full model")) +
    geom_boxplot(
        aes(
            x = var, y = rel.inf,
            group = group,
            fill = group,
            col = group,
        ),
        size = 0.8
    ) +
    scale_fill_manual(values = palette7_light) +
    scale_color_manual(values = palette7) +
    labs(
        y = "Relative Importance [%]",
        x = "",
        fill = "",
        col = "",
        pattern = ""
    ) +
    facet_grid2(var ~ group2, scale = "free_y", axes = "x", drop = T, render_empty = F) +
    coord_flip() +
    theme_clean() +
    theme(
        legend.background = element_rect(color = NA),
        plot.background = element_rect(color = NA),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
    )
relImp

pdf("boxplots_optConditions_relImp.pdf", width = 11, height = 14, pointsize = 12)
ggpubr::ggarrange(
    plotlist = list(relImp, optCondBoxplots), widths = c(1, 2.5), legend = "right",
    nrow = 1, common.legend = TRUE, labels = c("A", "B")
)
dev.off()

## Plot curves
pinput <- data.frame(
    var = rep(colnames(input[c(4:ncol(input))]), each = nrow(input)),
    value = unlist(input[c(4:ncol(input))]),
    year = rep(input$year, (ncol(input) - 3))
)
pinput <- na.omit(pinput)
pinput <- aggregate(pinput, by = list(pinput$year, pinput$var), mean)
pinput$var <- pinput$Group.2
pinput$Group.1 <- NULL
pinput$Group.2 <- NULL
# pinput <- pinput[pinput$var %in% inputVar, ]
pinput$group <- sapply(strsplit(pinput$var, "[_]"), "[[", 1)
pinput$group2 <- rep("", nrow(pinput))

pinput$RingWidth <- rep(subset(pinput, pinput$var == "RingWidth")$value, length(unique(pinput$var)))
pinput_selected <- pinput[pinput$var %in% c(var, var2), ]


envCond <- ggplot(pinput_selected) +
    geom_vline(xintercept = 1995) +
    geom_vline(xintercept = c(1975, 1980, 1985, 1990, 2000, 2005, 2010, 2015), col = "lightgrey", linetype = "dotted") +
    geom_point(aes(
        x = year,
        y = value, group = var, col = group
    )) +
    geom_line(aes(
        x = year,
        y = value, group = var, col = group
    )) +
    geom_smooth(aes(
        x = year,
        y = value, group = var, col = group
    ), se = FALSE, method = "lm") +
    scale_color_manual(values = palette8) +
    labs(
        y = "",
        x = "",
        fill = "",
        col = "",
        pattern = ""
    ) +
    facet_grid2(var ~ group2, scale = "free_y", drop = T, render_empty = F) +
    theme_clean() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(
        legend.background = element_rect(color = NA),
        plot.background = element_rect(color = NA)
    )
envCond

pdf("environmental_conditions.pdf", width = 13, height = 55, pointsize = 12)
envCond
dev.off()
