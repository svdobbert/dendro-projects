numbers <- split(input, list(input$species, input$group))
numbers <- lapply(numbers, function(x) length(unique(x$id)))
View(numbers)

numbersSpecies <- split(input, list(input$species))
numbersSpecies <- lapply(numbersSpecies, function(x) length(unique(x$id)))
View(numbersSpecies)

numbersSection <- split(input, list(input$species, input$group))
numbersSection <- lapply(numbersSection, function(x) length(unique(x$id)))
View(numbersSection)
