md.varImp <- function(model, scale = TRUE) {
    out <- tryCatch(
        {
            caret::varImp(model, scale=scale)
        },
        error=function(msg) {
            cat(paste0("===> ERROR: (md.varImp) ",match.call()[[1]]," (error): ", msg, "\r\n"))
            return(NULL)
        },
        warning=function(msg) {
            cat(paste0("===> ERROR: (md.varImp) ",match.call()[[1]]," (warning): ", msg, "\r\n"))
            return(NULL)
        },
        finally={}
    )    
    return(out)
}


# https://www.rdocumentation.org/packages/caret/versions/6.0-76/topics/varImp
# https://www.rdocumentation.org/packages/caret/versions/6.0-76/topics/filterVarImp
md.variable_importance <- function(model)
{
    ## plot.title <- paste0("varImp - ", title, "")
    ## fill_options <- c("forestgreen", "deepskyblue4", "tomato3")
    ## color <- sample(fill_options, 12, replace = TRUE)
    ## color <- sample(fill_options, 1)

    importance <- NULL

    imp_perc = md.varImp(model, TRUE)
    imp_no = md.varImp(model, FALSE)

    if (!is.null(imp_perc) && !is.null(imp_no)) { 
        ## obj <- ggplot(imp, top = 20) + geom_bar(stat = "identity", fill = color) + 
        ##     ggtitle(plot.title) + theme(axis.text.x = element_text(size = 16, 
        ##     face = "bold"), axis.title.x = element_text(size = 16, face = "bold"), 
        ##     axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 14, 
        ##         face = "bold"), plot.title = element_text(size = 16, face = "bold"))

        imp <- data.frame(imp_perc$importance)

        ## Take Overall if available otherwise take first values row
        if("Overall" %in% names(imp)){
            importance_perc = imp[order(-imp$Overall), ,drop = FALSE]
        }else{
            importance_perc = imp[order(-imp[, 1]), ]
        }
        importance_perc$features <- rownames(importance_perc)
        importance_perc$rank <- 1:nrow(importance_perc)
        colnames(importance_perc)[1] <- "score_perc"

        importance_perc$score_perc <- round(as.numeric(as.character(importance_perc$score_perc)), 2)
        importance_perc$score_perc[is.infinite(importance_perc$score_perc) | is.nan(importance_perc$score_perc) | is.na(importance_perc$score_perc) ] <- NA

        importance_perc <- importance_perc[!is.na(importance_perc$score_perc), ]
        importance_perc$score_perc[is.infinite(importance_perc$score_perc) | is.nan(importance_perc$score_perc) | is.na(importance_perc$score_perc) ] <- -10000


        imp <- data.frame(imp_no$importance)
        ## Take Overall if available otherwise take first values row
        if("Overall" %in% names(imp)){
            importance_no = imp[order(-imp$Overall), ,drop = FALSE]
        }else{
            importance_no = imp[order(-imp[, 1]), ]
        }
        importance_no$features <- rownames(importance_no)
        colnames(importance_no)[1] <- "score_no"

        importance_no$score_no <- round(as.numeric(as.character(importance_no$score_no)), 2)
        importance_no$score_no[is.infinite(importance_no$score_no) | is.nan(importance_no$score_no) | is.na(importance_no$score_no) ] <- NA 

        importance_no <- importance_no[!is.na(importance_no$score_no), ]
        importance_no$score_no[is.infinite(importance_no$score_no) | is.nan(importance_no$score_no) | is.na(importance_no$score_no) ] <- NA

        importance <- dplyr::full_join(importance_perc, importance_no, by = c("features"))
        if(is.na(importance)){
            cat(paste0("===> ERROR: md.variable_importance NA Values found\r\n"))
            print(importance_perc)
            print(importance_no)
            print(importance)
            ## Delete NA values
            # importance <- importance[!is.na(importance)]
            importance <- NULL
        }
    }

    return(importance)
}

md.strip.left.aligned <- function(which.given, which.panel, factor.levels, 
    ...)
    {
    # lattice::panel.rect(0, 0, 1, 1, col='transparent', border=0)
    lattice::panel.text(x = 0, y = 0.4, pos = 4, lab = factor.levels[which.panel[which.given]], 
        cex = 0.47, col = "black", font = 1, srt = 5)
}

md.trainingFeaturePlots <- function(training, title)
{
    # ensure the results are repeatable
    set.seed(1234)
    
    plot.title <- paste0("Feature plot - ", title, "")
    
    data.without_outcome <- subset(training, select = -c(outcome))
    # The preProcess function helps determine values for predicator
    # transforms on the training set and can be applied to this and future
    # sets.  This is important as nnets and svms can require scaled and/or
    # centered data which this function supports
    data.trainplot = predict(preProcess(data.without_outcome, method = "range"),  data.without_outcome)
    
    # The featurePlot highlights variables that provide separation with
    # regard to the 'high' classification
    obj <- featurePlot(x = data.trainplot, y = training$outcome, "box", 
        scales = list(x = list(relation = "free", rot = 90, cex = 0.45), 
            y = list(relation = "free", log = TRUE, rot = 0, cex = 0.35)), 
        strip = md.strip.left.aligned, par.settings = list(axis.line = list(col = "grey")), 
        labels = c("", "separation score"), layout = c(4, 4), main = plot.title, 
        auto.key = list(columns = 2))
    print(obj)
}

md.confusionMatrix.matrix <- function(predictions, testing) {
    out <- tryCatch(
        {
          caret::confusionMatrix(
            data = table(factor(predictions, union(predictions, testing$outcome)), 
            reference = factor(testing$outcome, union(predictions, testing$outcome))),
            positive = "high"
          )
        },
        error=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (error): ", msg, "\r\n"))
            return(NULL)
        },
        warning=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (warning): ", msg, "\r\n"))
            return(NULL)
        },
        finally={}
    )    
    return(out)
}

md.confusionMatrix <- function(predictions, testing) {
    set.seed(1234)
    results <- NULL

    matrix = md.confusionMatrix.matrix(predictions, testing)

    if (!is.null(matrix)) { 
        results <- matrix
    }else{
        cat(paste0("===> ERROR: md.testModel.hp.confusionMatrix in NULL: ",model))
    }

    return(results)
}

## https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
md.predictModel.predict.prob <- function(training_model, testing_data, type) {
    out <- tryCatch(
        {
            predict(training_model,  testing_data, type = type)
        },
        error=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (error): ", msg, "\r\n"))
            return(NULL)
        },
        warning=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (warning): ", msg, "\r\n"))
            return(NULL)
        },
        finally={}
    )    
    return(out)
}

md.predictModel.predict <- function(training_model, testing_data) {
    out <- tryCatch(
        {
            predict(training_model, testing_data)
        },
        error=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (error): ", msg, "\r\n"))
            return(NULL)
        },
        warning=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (warning): ", msg, "\r\n"))
            return(NULL)
        },
        finally={}
    )    
    return(out)
}

md.predictModel <- function(training_model, testing_data) {
    set.seed(1234)

    ## Predicting Class Probabilities
    pred_prob <- md.predictModel.predict.prob(training_model, testing_data, "prob")
    pred_raw <- md.predictModel.predict(training_model, testing_data)

    return(list(pred_prob = pred_prob, pred_raw = pred_raw))
}


md.testModelROC.roc <- function(testing, predict_model) {
    out <- tryCatch(
        {
           roc(testing$outcome, predict_model[, "high"], levels = levels(testing$outcome))
        },
        error=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (error): ", msg, "\r\n"))
            return(NULL)
        },
        warning=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (warning): ", msg, "\r\n"))
            return(NULL)
        },
        finally={}
    )    
    return(out)
}

md.testModelROC <- function(predict_model, testing_data, title)
{
    set.seed(1234)
    roc_title <- paste0("ROC - ", title)

    auc_p <- NULL
    roc_p <- NULL

    roc_p <- md.testModelROC.roc(testing_data, predict_model)   

    if (!is.null(roc_p)) { 
        ## Compute the area under the ROC curve
        auc_p <- as.numeric(pROC::auc(roc_p))

        ## # Draw ROC curve.
        ## obj <- plot(roc_p, 
        ##      legacy.axes = TRUE,
        ##      print.thres.pattern = "Cutoff: %.2f (Sp = %.2f, Sn = %.2f)",
        ##      print.thres = "best", 
        ##      main = paste0("", roc_title))
        ## print(obj)
        ## # to get threshold and accuracy
        ## obj <- coords(roc_p, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
        ## print(obj)

    }else{
        print(paste0("===> ERROR: md.testModelROC.hp.roc ",title))
    }

    return(list(roc_p = roc_p, auc_p = auc_p))
}


md.trainModel.hp.train <- function(trControl, training, model, tuneLength) {
    out <- tryCatch(
        {
           train(factor(outcome)~.
                                    , training
                                    , method = model
                                    , trControl = trControl
                                    #  , metric = targetUsed
                                    , preProcess = NULL
                                    # , preProcess = c("center", "scale")
                                    # , tuneGrid = createGrid(model, tune_length, training),
                                    , trace = FALSE
                                    , tuneLength = tuneLength)
        },
        error=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (error): ", msg, "\r\n"))
            return(NULL)
        },
        warning=function(msg) {
            cat(paste0("===> ERROR: ",match.call()[[1]]," (warning): ", msg, "\r\n"))
            return(NULL)
        },
        finally={}
    )    
    return(out)
}


md.trainModel <- function(training, model, title)
{
    set.seed(1234)

    results <- NULL

    # set CV params
    kFolds <- 10
    cvRepeats <- 3
    tuneLength <- 3

    # targetUsed = "ROC" #Accuracy when N>>P, Balanced Accuracy when P>>N, Kappa when unbalanced classes, ROC when dealing with general purpose binary classification
    # samplingMethod = "repeatedcv" #adaptive_cv is good and faster, repeatedcv is best (caretEnsemble) and reliable ($besttune error)

    # seeds <- vector(mode = "list", length = nrow(training) + 1)
    # seeds <- lapply(seeds, function(x) 1:20)
    seeds <- hp.set.seed.cv(1234, kFolds, cvRepeats, tuneLength)
    
    ## MaxNWts <- 5 * (ncol(predictors) + 1) + 5 + 1)
    
    trControl <- trainControl(
        method = 'repeatedcv', 
        number = kFolds,
        repeats = cvRepeats,
        seeds = seeds,
        summaryFunction = multiClassSummary,
        verboseIter = FALSE,
        savePredictions = TRUE,
        classProbs = TRUE,
        allowParallel = TRUE
        # MaxNWts = MaxNWts
    )

    model.execution <-
        tryCatch(
            garbage <- capture.output(results <- train(factor(outcome)~.,
                                  training,
                                    method = model,
                                    trControl = trControl,
                                    #  metric = "ROC",
                                    preProcess = NULL
                                    #  preProcess = c("center", "scale")
                                    # , tuneGrid = createGrid(model, tune_length, training),
                                    , tuneLength = tuneLength
            )),
            error = function(e) print(e)
        )

    if(!inherits(model.execution, "error")){
        params <- results$modelInfo$parameters$parameter
        paramValues <- apply(results$results[,params,drop = FALSE],2,function(x) length(unique(x)))
        if(any(paramValues > 1)){   
            obj <- plot(results, main = title)
            print(obj)
        }else{
            cat(paste0("===> ERROR: md.trainModel: ", model, " ", title,"\r\n"))
            print(results)
        }
    }

    return(results)
}
