# Load data
load("../input-data/input")
bet <- data 

# replace wrong values
test <- subset(bet,bet$radius == 0)
test <- ifelse(grep("E9ABet2", bet$id), bet$radius, bet$radius+1)
bet[grep("E9ABet2", bet$id),]$radius <- test

# replace small values
test <- subset(bet, bet$value >7.5)
min(test$value)
histogram(test$value, breaks = 30)
bet$value <- replace(bet$value, bet$value < 7.5, 7.561)

# get elevation
elevation <- substr(bet$id, 1, 5)

old <- unique(elevation)
new <- c("0900","0900","0900", "0900", "1100", "1100", "1100", "1100", "1300", "1300", "1300", "1300", "1500", "0700", "0700", "0700", "0700", "0900", "0900", "0900", "0900", "1100", "1100", "1100", "1100")
elevation[elevation %in% old] <- new[match(elevation, old, nomatch = 0)]

# get region and stem section
id <- substr(bet$id, 1, 1)
old <- c("E", "W")
new <- c("Vågå/Innlandet region", "Geiranger/Møre og Romsdal region")
id[id %in% old] <- new[match(id, old, nomatch = 0)]
region <- id
stem <- sapply(strsplit(bet$id, "[_]"), "[[", 1)
stem <- substr(stem, nchar(stem), nchar(stem))
unique(stem)
stem <- replace(stem, stem == "t", 0)

# create ids
id <- substr(bet$id, 1, 1)
id <- paste0("NO.", id, elevation, bet$position, ".", "Bnan0", stem, "a.nB01.0", bet$radius)

# create table
table_bet <- data.frame(id_old = bet$id,
                        id = id,
                        region = region,
                        elevation = as.numeric(elevation),
                        species = bet$species,
                        position = bet$position,
                        segment = rep("base", nrow(bet)),
                        radius = bet$radius,
                        year = bet$year,
                        ringWidth = bet$value
                        )
table_bet <- subset(table_bet, is.na(table_bet$ringWidth) == FALSE)
unique(table_bet$radius)

# write to excel sheet
library("writexl")
write_xlsx(table_bet,"./datasetBetulaNana.xlsx")
min(table_bet$ringWidth)
id <- substr(table_bet$id, 1, 12)