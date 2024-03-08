modelTrain <- function(input) {
    # input <- as.data.frame(sapply(input, function(x) as.numeric(x)))
    input <- sapply(input, function(x) replace(x, is.infinite(x), 0))
    input <- na.omit(input)
    input <- as.data.frame(scale(input))

    # Cross validation
    ctrl <- trainControl(
        method = "cv",
        number = 10
    )

    # Tuning Hyper Parameters
    tuneGrid <- expand.grid(
        n.trees = c(20, 50, 100, 200, 300, 400, 500),
        interaction.depth = c(1, 2, 3),
        shrinkage = c(0.3, 0.1, 0.05, 0.01, 0.005),
        n.minobsinnode = c(5, 10)
    )

    # split data
    inTraining <- createDataPartition(input$RingWidth, p = .80, list = FALSE)
    training <- input[inTraining, ]
    testing <- input[-inTraining, ]

    model <- train(
        RingWidth ~ .,
        data = training,
        method = "gbm",
        preProcess = c("center", "scale"),
        trControl = ctrl,
        tuneGrid = tuneGrid,
        verbose = FALSE
    )

    output <- list(model, training, testing)
    output
}

createModel <- function(input) {
    # input <- input[[2]]
    input <- input[-c(1:3)]
    modelOutput <- modelTrain(input)
    model <- modelOutput[[1]]

    # When using boosted regression trees, the relative importance of predictor variables is calculated based on the number of times a variable is selected in the model, weighted by its improvement to the overall model (Friedman 2001; Elith et al. 2008).

    # get selected arguments
    nTrees <- model$bestTune["n.trees"]
    interactionDepth <- model$bestTune["interaction.depth"]
    shrinkage <- model$bestTune["shrinkage"]
    nMinobsinnode <- model$bestTune["n.minobsinnode"]

    # final model using selected arguments from training model
    # input <- as.data.frame(sapply(input, function(x) as.numeric(x)))
    finalModel <- gbm(RingWidth ~ .,
        distribution = "gaussian",
        data = input,
        n.trees = nTrees[1][1, ],
        interaction.depth = interactionDepth[1][1, ],
        shrinkage = shrinkage,
        n.minobsinnode = nMinobsinnode,
        cv.folds = 10,
    )

    ## Step 1
    # remove variables with little importance
    threshold <- 1
    reducedModel <- subset(summary(finalModel), summary(finalModel)$rel.inf > threshold)
    var <- reducedModel$var

    input_reduced <- as.data.frame(cbind(RingWidth = input$RingWidth, input[var]))

    modelOutput <- modelTrain(input_reduced)
    model <- modelOutput[[1]]

    # get selected arguments
    nTrees <- model$bestTune["n.trees"]
    interactionDepth <- model$bestTune["interaction.depth"]
    shrinkage <- model$bestTune["shrinkage"]
    nMinobsinnode <- model$bestTune["n.minobsinnode"]

    # final model using selected arguments from training model
    # input <- as.data.frame(sapply(input, function(x) as.numeric(x)))
    finalModel2 <- gbm(RingWidth ~ .,
        distribution = "gaussian",
        data = input_reduced,
        n.trees = nTrees[1][1, ],
        interaction.depth = interactionDepth[1][1, ],
        shrinkage = shrinkage,
        n.minobsinnode = nMinobsinnode,
        cv.folds = 10,
    )

    ## Step 2
    # remove variables with little importance
    threshold <- 1
    reducedModel <- subset(summary(finalModel2), summary(finalModel2)$rel.inf > threshold)
    var <- reducedModel$var

    input_reduced <- as.data.frame(cbind(RingWidth = input_reduced$RingWidth, input_reduced[var]))

    modelOutput <- modelTrain(input_reduced)
    model <- modelOutput[[1]]

    # When using boosted regression trees, the relative importance of predictor variables is calculated based on the number of times a variable is selected in the model, weighted by its improvement to the overall model (Friedman 2001; Elith et al. 2008).

    # get selected arguments
    nTrees <- model$bestTune["n.trees"]
    interactionDepth <- model$bestTune["interaction.depth"]
    shrinkage <- model$bestTune["shrinkage"]
    nMinobsinnode <- model$bestTune["n.minobsinnode"]

    # final model using selected arguments from training model
    # input <- as.data.frame(sapply(input, function(x) as.numeric(x)))
    finalModel3 <- gbm(RingWidth ~ .,
        distribution = "gaussian",
        data = input_reduced,
        n.trees = nTrees[1][1, ],
        interaction.depth = interactionDepth[1][1, ],
        shrinkage = shrinkage,
        n.minobsinnode = nMinobsinnode,
        cv.folds = 10,
    )


    ## Step 3
    # remove variables with little importance
    threshold <- 1
    reducedModel <- subset(summary(finalModel3), summary(finalModel3)$rel.inf > threshold)
    var <- reducedModel$var

    input_reduced <- as.data.frame(cbind(RingWidth = input_reduced$RingWidth, input_reduced[var]))

    modelOutput <- modelTrain(input_reduced)
    model <- modelOutput[[1]]
    training <- modelOutput[[2]]
    testing <- modelOutput[[3]]

    # When using boosted regression trees, the relative importance of predictor variables is calculated based on the number of times a variable is selected in the model, weighted by its improvement to the overall model (Friedman 2001; Elith et al. 2008).

    # get selected arguments
    nTrees <- model$bestTune["n.trees"]
    interactionDepth <- model$bestTune["interaction.depth"]
    shrinkage <- model$bestTune["shrinkage"]
    nMinobsinnode <- model$bestTune["n.minobsinnode"]

    # final model using selected arguments from training model
    # input <- as.data.frame(sapply(input, function(x) as.numeric(x)))
    finalModel4 <- gbm(RingWidth ~ .,
        distribution = "gaussian",
        data = input_reduced,
        n.trees = nTrees[1][1, ],
        interaction.depth = interactionDepth[1][1, ],
        shrinkage = shrinkage,
        n.minobsinnode = nMinobsinnode,
        cv.folds = 10,
    )
    output <- list(
        finalModel4, training, testing
    )
    output
}
