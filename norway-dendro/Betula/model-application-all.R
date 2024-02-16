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
