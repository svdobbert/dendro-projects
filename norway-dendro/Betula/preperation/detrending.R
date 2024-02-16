# rename region
region <- growth$region
old <- unique(region)
new <- c("E", "W")
old
new
region[region %in% old] <- new[match(region, old, nomatch = 0)]
growth$region <- region

input <- split(growth, list(growth$position, growth$region))

# detrend
rownames <- list()
po <- list() # ith offset
rcs <- list()
for(i in c(1:length(input))) {
  input[[i]] <- split(input[[i]], input[[i]]$id)
  rownames[[i]] <- input[[i]][[1]]$year
  input[[i]] <- lapply(input[[i]], function(x) x$value)
  input[[i]] <- as.data.frame(do.call(cbind, input[[i]]))
  rownames(input[[i]]) <- rownames[[i]]
  input[[i]] <- input[[i]][, colSums(is.na(input[[i]])) != nrow(input[[i]])]
  po[[i]] <-  data.frame(series = colnames(input[[i]]),
                         pith.offset = unlist(lapply(input[[i]], function(x) first(which(is.na(x) == FALSE))-1))
  )
  po[[i]]$pith.offset <- po[[i]]$pith.offset + 1
  rcs[[i]] <- rcs(input[[i]], po[[i]], biweight = TRUE, rc.out = TRUE, nyrs = 2)$rc
  input[[i]] <- na.omit(cbind(rcs[[i]], rownames[[i]]))
}
