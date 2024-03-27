## Color palettes
palette <- c("#DB0201", "#9EA06F", "#17413F", "#FDC99A", "#4E7258", "#A99FB1", "#769ab6" )
palette_light <- rgb(col2rgb(palette)[1,], col2rgb(palette)[2,], col2rgb(palette)[3,], max = 255, alpha = 100)
paletteInd <- c("#DB0201", "#9EA06F", "#17413F", "#FDC99A", "#4E7258", "#A99FB1", "#769ab6" )
colfunc <- colorRampPalette(c("#A99FB1", "#769ab6", "#4E7258", "#17413F"))
paletteTrends <- c("#DB0201","#FDC99A","#9EA06F")

## Labels
labels <- data.frame(
  label = unlist(numbersSpecies),
  species = names(numbersSpecies)
)

labelsGroup <- data.frame(
  label = unlist(numbersSection),
  species = paste0(sapply(strsplit(names(numbersSection), "[.]"), "[[", 1), ".", sapply(strsplit(names(numbersSection), "[.]"), "[[", 2)),
  group  = sapply(strsplit(names(numbersSection), "[.]"), "[[", 3)
)
labels <- subset(labels, labels$label != "0")

