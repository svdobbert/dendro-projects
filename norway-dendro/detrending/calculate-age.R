# restructure dataframe in order to calculate age
restructure <- function(df) {
  maxYear <- max(na.omit(df$year))
  minYear <- min(na.omit(df$year))

  df$id <- paste(df$id, df$species)
  dfInd <- split(df, df$id)
  species <- sapply(dfInd, function(x) first(x$species))
  group <- sapply(dfInd, function(x) first(x$group))
  position <- sapply(dfInd, function(x) first(x$position))

  maxYearInd <- list()
  minYearInd <- list()
  for (i in seq_along(dfInd)) {
    dfInd[[i]] <- dfInd[[i]][order(dfInd[[i]]$year), ]
    maxYearInd[[i]] <- max(na.omit(dfInd[[i]]$year))
    minYearInd[[i]] <- min(na.omit(dfInd[[i]]$year))

    dfInd[[i]] <- c(
      rep(NA, ifelse((minYearInd[[i]]) == minYear,
        0,
        (minYearInd[[i]] - minYear)
      )),
      dfInd[[i]]$value,
      rep(NA, maxYear - maxYearInd[[i]])
    )
  }
  dfInd <- as.data.frame(do.call(rbind, dfInd))
  colnames(dfInd) <- c(minYear:maxYear)
  dfInd$species <- species
  dfInd$group <- group
  dfInd$position <- position

  dfInd
}
inputRestructured <- restructure(input)
nYear <- max(na.omit(input$year)) - min(na.omit(input$year)) +1
inputRestructured[, 1:nYear] <- sapply(inputRestructured[, 1:nYear], function(x) as.numeric(x))

# create dataframe with all Individuals as same length
input <- data.frame(
  value = c(t(inputRestructured[, 1:nYear])),
  year = rep(colnames(inputRestructured[, 1:nYear]), nrow(inputRestructured)),
  id = rep(rownames(inputRestructured), each = nYear),
  group = rep(inputRestructured$group, each = nYear),
  species = rep(inputRestructured$species, each = nYear),
  position = rep(inputRestructured$position, each = nYear)
)


## calculate mean and tbrm for each year 
tbrm <- sapply(inputRestructured[, 1:nYear], function(x) tbrm(x))
mean <- sapply(inputRestructured[, 1:nYear], function(x) mean(na.omit(x)))

inputSpecies <- split(inputRestructured, inputRestructured$species)
tbrm_species <- list()
mean_species <- list()
for (i in seq_along(inputSpecies)) {
  tbrm_species[[i]] <- sapply(inputSpecies[[i]][, 1:nYear], function(x) tbrm(x))
  mean_species[[i]] <- sapply(inputSpecies[[i]][, 1:nYear], function(x) mean(na.omit(x)))
}
names(tbrm_species) <- names(inputSpecies)
names(mean_species) <- names(inputSpecies)

inputSection <- split(inputRestructured, list(inputRestructured$species, inputRestructured$group), drop = TRUE)
tbrm_section <- list()
mean_section <- list()
for (i in seq_along(inputSection)) {
  tbrm_section[[i]] <- sapply(inputSection[[i]][, 1:nYear], function(x) tbrm(x))
  mean_section[[i]] <- sapply(inputSection[[i]][, 1:nYear], function(x) mean(na.omit(x)))
}
names(tbrm_section) <- names(inputSection)
names(mean_section) <- names(inputSection)

getAge <- function(df, name) {
 species <- first(na.omit(df$species))
  list <- split(df, df$id)
  for (i in seq_along(list)) {
  list[[i]] <-  list[[i]][order(list[[i]]$year), ]
  list[[i]]$mean <- mean
  list[[i]]$tbrm <- tbrm
  list[[i]]$mean_species <- mean_species[[species]]
  list[[i]]$tbrm_species <- tbrm_species[[species]]
  list[[i]]$mean_group <- mean_section[[name]]
  list[[i]]$tbrm_group <- tbrm_section[[name]]
}
output <- as.data.frame(do.call(rbind,list))
output
}

inputAge <- split(input, list(input$species, input$group), drop = TRUE)

for (i in names(inputAge)) {
  inputAge[[i]] <- getAge(inputAge[[i]], i)
}

inputAge <- as.data.frame(do.call(rbind, inputAge))

###


inputAge <- split(inputAge, inputAge$id)

inputSubset <- list()
input_age <- list()
for (i in seq_along(inputAge)) {
  inputAge[[i]]$diff_tbrm <- (inputAge[[i]]$tbrm - inputAge[[i]]$value) * (-1)
 inputAge[[i]]$diff_tbrm_species <- (inputAge[[i]]$tbrm_species - inputAge[[i]]$value) * (-1)
  inputAge[[i]]$diff_tbrm_group <- (inputAge[[i]]$tbrm_group - inputAge[[i]]$value) * (-1)
  inputAge[[i]]$perc_tbrm <- (inputAge[[i]]$value / inputAge[[i]]$tbrm) * 100
  inputAge[[i]]$perc_tbrm_species <- (inputAge[[i]]$value / inputAge[[i]]$tbrm_species) * 100
  inputAge[[i]]$perc_tbrm_group <- (inputAge[[i]]$value / inputAge[[i]]$tbrm_group) * 100

  inputAge[[i]]$diff_mean <- (inputAge[[i]]$mean - inputAge[[i]]$value) * (-1)
 inputAge[[i]]$diff_mean_species <- (inputAge[[i]]$mean_species - inputAge[[i]]$value) * (-1)
  inputAge[[i]]$diff_mean_group <- (inputAge[[i]]$mean_group - inputAge[[i]]$value) * (-1)
  inputAge[[i]]$perc_mean <- (inputAge[[i]]$value / inputAge[[i]]$mean) * 100
  inputAge[[i]]$perc_mean_species <- (inputAge[[i]]$value / inputAge[[i]]$mean_species) * 100
  inputAge[[i]]$perc_mean_group <- (inputAge[[i]]$value / inputAge[[i]]$mean_group) * 100

  inputSubset[[i]] <- subset(inputAge[[i]], is.na(inputAge[[i]]$value) == FALSE)

  input_age[[i]] <- data.frame(
    value = c(inputSubset[[i]]$value, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    age = c(1:nrow(inputAge[[i]])),
    id = inputAge[[i]]$id,
    species = inputAge[[i]]$species,
    position = inputAge[[i]]$position,
    group = inputAge[[i]]$group,
    year = c(inputSubset[[i]]$year, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    mean = c(inputSubset[[i]]$mean, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    mean_species = c(inputSubset[[i]]$mean_species, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    mean_group = c(inputSubset[[i]]$mean_group, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    tbrm = c(inputSubset[[i]]$tbrm, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    tbrm_species = c(inputSubset[[i]]$tbrm_species, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    tbrm_group = c(inputSubset[[i]]$tbrm_group, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    perc_tbrm = c(inputSubset[[i]]$perc_tbrm, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    perc_tbrm_species = c(inputSubset[[i]]$perc_tbrm_species, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    perc_tbrm_group = c(inputSubset[[i]]$perc_tbrm_group, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    diff_tbrm = c(inputSubset[[i]]$diff_tbrm, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    diff_tbrm_species = c(inputSubset[[i]]$diff_tbrm_species, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    diff_tbrm_group = c(inputSubset[[i]]$diff_tbrm_group, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    perc_mean = c(inputSubset[[i]]$perc_mean, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    perc_mean_species = c(inputSubset[[i]]$perc_mean_species, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    perc_mean_group = c(inputSubset[[i]]$perc_mean_group, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    diff_mean = c(inputSubset[[i]]$diff_mean, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    diff_mean_species = c(inputSubset[[i]]$diff_mean_species, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]])))),
    diff_mean_group = c(inputSubset[[i]]$diff_mean_group, rep(NA, (nrow(inputAge[[i]]) - nrow(inputSubset[[i]]))))
  )
}
input_age <- as.data.frame(do.call(rbind, input_age))

## Calculate mean by age

# Sample depth
sample_depth <- lapply(c(1:nYear), function(x) subset(input_age, input_age$age == x))
sample_depth <- lapply(sample_depth, function(x) sum(replace(x$value, is.na(x$value) == FALSE, 1)))
input_age$group2 <- paste(input_age$species, input_age$group)

input_age_mean <- aggregate(input_age, by = list(age = input_age$age), function(x) mean(x, na.rm = TRUE))
input_age_mean <- input_age_mean[-c(1, 4:8, 27)]
input_age_mean$sample_depth <- unlist(sample_depth)

# by species and group
input_age_mean_group <- aggregate(input_age, by = list(age = input_age$age, group = input_age$group2), function(x) mean(x, na.rm = TRUE))
input_age_mean_group <- input_age_mean_group[-c(1, 5:8, 28)]
input_age_mean_group$species <- paste(sapply(strsplit(input_age_mean_group$group, "[ ]"), "[[", 1), sapply(strsplit(input_age_mean_group$group, "[ ]"), "[[", 2), sep = " ")
input_age_mean_group$group <- sapply(strsplit(input_age_mean_group$group, "[ ]"), "[[", 3)

sample_depth <- split(input_age, input_age$group2)
for (i in c(1:length(sample_depth))) {
  sample_depth[[i]] <- lapply(c(1:nYear), function(x) subset(sample_depth[[i]], sample_depth[[i]]$age == x))
  sample_depth[[i]] <- lapply(sample_depth[[i]], function(x) sum(replace(x$value, is.na(x$value) == FALSE, 1)))
}
input_age_mean_group$sample_depth <- unlist(as.data.frame(do.call(cbind, sample_depth)))

# by species
input_age_mean_species <- aggregate(input_age, by = list(age = input_age$age, species = input_age$species), function(x) mean(x, na.rm = TRUE))
input_age_mean_species <- input_age_mean_species[-c(1, 5:8, 28)]
sample_depth <- split(input_age, input_age$species)
for (i in c(1:length(sample_depth))) {
  sample_depth[[i]] <- lapply(c(1:nYear), function(x) subset(sample_depth[[i]], sample_depth[[i]]$age == x))
  sample_depth[[i]] <- lapply(sample_depth[[i]], function(x) sum(replace(x$value, is.na(x$value) == FALSE, 1)))
}
input_age_mean_species$sample_depth <- unlist(as.data.frame(do.call(cbind, sample_depth)))

test <- subset(input_age, input_age$species == "B. nana" & is.na(input_age$value) == FALSE)
max(test$age)
mean(test$value)
mean(test$age)

# Tukey's Biweight Robust Mean
input_age_mean$tbrm <- aggregate(input_age, by = list(age = input_age$age), function(x) tbrm(x))$value
input_age_mean_group$tbrm <- aggregate(input_age, by = list(age = input_age$age, group = input_age$group2), function(x) tbrm(x))$value
input_age_mean_species$tbrm <- aggregate(input_age, by = list(age = input_age$age, group = input_age$species), function(x) tbrm(x))$value

# standard error
input_age_mean$se <- aggregate(input_age, by = list(age = input_age$age), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))$value
input_age_mean$perc_tbrm_se <- aggregate(input_age, by = list(age = input_age$age), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))$perc_tbrm
input_age_mean$perc_mean_se <- aggregate(input_age, by = list(age = input_age$age), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))$perc_mean

# standard error
input_age_mean_group$se <- aggregate(input_age, by = list(age = input_age$age, group = input_age$group2), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))$value
input_age_mean_group$perc_tbrm_se <- aggregate(input_age, by = list(age = input_age$age, group = input_age$group2), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))$perc_tbrm_group
input_age_mean_group$perc_mean_se <- aggregate(input_age, by = list(age = input_age$age, group = input_age$group2), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))$perc_mean_group


# standard error
input_age_mean_species$se <- aggregate(input_age, by = list(age = input_age$age, group = input_age$species), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))$value
input_age_mean_species$perc_tbrm_se <- aggregate(input_age, by = list(age = input_age$age, group = input_age$species), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))$perc_tbrm_species
input_age_mean_species$perc_mean_se <- aggregate(input_age, by = list(age = input_age$age, group = input_age$species), function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))$perc_mean_species
