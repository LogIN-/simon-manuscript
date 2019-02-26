#!/usr/bin/Rscript --vanilla
#
# Author: LogIN-


# Clear work space
rm(list = ls(all = TRUE)) 

# Set some default workspace variables
options(stringsAsFactors=FALSE)
options(width=512)
options(save.defaults = list(precheck = TRUE, compress = TRUE, safe = TRUE))

# GLOBAL variables 
SEED = 1234
RESET = FALSE

DATABASE_CREDS = list(
    username = "simon-approach",
    password = "simon-approach",
    host = "localhost",
    dbname = "simon-approach")

# Ensure the results are repeatable
set.seed(SEED)
require(pacman)

## Load main R packages
packages = c("devtools", "jsonlite","doMC","reshape2","plyr","dplyr","DBI","RMySQL", "feather", "caret", "corrplot")
p_load(char = packages)

devtools::install_github("LogIN-/mulset", subdir = 'R-package')
require(mulset)

# Packages for the model training
caret_deps <- c("BradleyTerry2", "e1071", "earth", "fastICA", "gam", "ipred", 
    "kernlab", "klaR", "MASS", "ellipse", "mda", "mgcv", "mlbench", "MLmetrics", 
    "nnet", "party", "pls", "pROC", "proxy", "randomForest", 
    "RANN", "spls", "subselect", "pamr", "superpc", "Cubist", "testthat")
p_load(char = caret_deps)


cpu_cores <- detectCores(logical = TRUE)
cpu_cores <- as.numeric(cpu_cores)
CORES = cpu_cores - 2
# set up the parallel processing
registerDoMC(CORES)  #use X cores

memfree <- as.numeric(system("grep MemTotal /proc/meminfo | awk '{print $2}'",  intern=TRUE))
memfree <- round((memfree / 1024) - 5000)
options(java.parameters = paste0("-Xmx" ,memfree, "m"))

if(.Platform$OS.type != "unix") {
    memory.limit(size=memfree) 
}

args <- commandArgs(TRUE)
processing_id <- as.numeric(args[1])
if(is.null(processing_id) || is.na(processing_id)){
    processing_id <- 1
}

source('functions/helpers.R')
source('functions/database.R')

source('functions/models.R')
source('functions/roc.R')
source('functions/graphs.R')

## Truncate Database
if(RESET == TRUE){
    db.clear()
}

## Create output directories needed for the saving of output files
output_directories  = c('pdf', 'summary', 'models', 'data', 'roc', 'confusion_matrix', 'importance', 'combined', 'logs')
for (output_dir in output_directories) {
    if(RESET == TRUE){
        unlink(paste0("output/",output_dir), recursive=TRUE)
    }
    dir.create(file.path("output", output_dir), showWarnings = FALSE)
}

## Ristrict processing by features_hash or models
features_hash <- NULL
models_restrict <- NULL

cat(paste0("===> INFO: (start) Total cores allocated: ", CORES , " Total memory: ", memfree, "MB\r\n"))

# get all model names for classification
models_to_process <- modelLookup()[modelLookup()$forClass, c(1)]
models_to_process <- unique(models_to_process)

# Don't process following models
removeModels <- c(
    "bartMachine", "tan", "extraTrees", "hdrda", "RWeka", 
    "J48", "JRip", "LMT", "PART", "OneR", "oblique.tree","xyf", 
    "pda2", "Rborist", "xgbTree", "xgbLinear", "mlpSGD", "gaussprPoly", "gamSpline", "randomGLM", "gaussprLinear", "spls", "glmStepAIC", "parRF"
    )

#remove all slow and failed models from model list
models_to_process <- models_to_process[!models_to_process %in% removeModels]

log_output <- paste0("output/logs/output_",processing_id,"_",format(Sys.time(), "%d-%m-%Y-T%H-%M"),".log")
SINKCON <- file(log_output)

if(CORES > 8){
    cat(paste0("===> INFO: (start) LOG file: ",log_output,"\r\n"))
    sink(SINKCON, append=TRUE)
    sink(SINKCON, append=TRUE, type="message")
}

## Make mulset data partitions
initailData <-read.csv("./data/data_transposed.csv", header=TRUE, stringsAsFactors=FALSE)

cat(paste("===> Calculating resamples for dataset using mulset \r\n"))
resamples <- mulset(initailData, exclude = c("donor_id", "outcome"), include = c("samples", "samples_count"), maxIntersections = 250)
resamples_total <- length(resamples)
cat(paste("===> Found total: ",resamples_total," resamples using mulset before filters\r\n"))

resamples_total <- 0
for(i in seq_along(resamples)){
    ## Remove some of the resamples that doesn't follow criteria below
    if(resamples[[i]]$feature_count >= 5 && resamples[[i]]$samples_count >= 15){
        resamples_total <- resamples_total + 1
    }else{
        resamples[[i]]$features_hash <- NULL
    }
}


cat(paste("===> Calculating feature sets for: ", processing_id, "\r\n"))
cat(paste("===> Need to initially process ", resamples_total, " feature sets\r\n"))

if(resamples_total < 1){
    cat(paste("===> Terminating script execution for Processing ID: ",processing_id," not enough feature sets to process..."))
    next
}

dfRows <- resamples_total
# loop each feature collection and make analysis
for(step in 0:dfRows) {
    step = step + 1

    resample <- resamples[[step]]
    resample$data_source <- "initial"


    ## Check if data-frame row exists
    if(is.null(resample$features_hash)){
        next
    }else{
        cat(paste0("Processing step. Data Row found... : ", step, "\r\n"))
    }

    if(!is.null(features_hash) && resample$features_hash != features_hash){
        cat(paste0("===> Skipping because features_hash limitation ",resample$features_hash," Step counter: ",step," \r\n"))
        next
    }

    # ensure the results are repeatable
    set.seed(SEED)

    subset <- list(samples_count = NULL, results = NULL, results_raw = NULL)
    subset$results_raw <- initailData[resample$samples, ]
    subset$results_raw <- subset$results_raw[, c("outcome", "donor_id", resample$features)]

    # Remove missing values from data frame
    subset$results <- subset$results_raw[complete.cases(subset$results_raw), ]
    subset$samples_count <- length(unique(subset$results$donor_id))
    subset$results$outcome <- mapvalues(subset$results$outcome, from = c("1", "0"), to = c("high", "low"))
    subset$results$outcome <- as.factor(subset$results$outcome)

    if(subset$samples_count < 10){
        cat(paste0("===> Skipping (not enough subject for analysis): ",resample$features_hash," \r\n"))
        next
    }

    data_input <- subset$results
    data_input <- subset(data_input, select = -c(donor_id) )

    unique_title <- paste0(processing_id," D: ",resample$samples_count," F: ",resample$feature_count," S: ",step,"")
    cat(paste0("===> Analyzing: Total Donors: ", resample$samples_count, " Total Features: ",resample$feature_count," Hash: ",resample$features_hash,"\r\n"))

    pdf_path = paste0("output/pdf/", processing_id , "_",resample$features_hash,"_d_", resample$samples_count, "_f_", resample$feature_count, "_s_",step,".pdf")
    pdf(file=pdf_path)


    data_input.without_outcome <- subset(data_input, select = -c(outcome) )
    data <- hp.createPartitions(data_input)

    data$training$outcome <- factor(
        data$training$outcome,levels = levels(data$training$outcome)
    )
    data$testing$outcome <- factor(
        data$testing$outcome,levels = levels(data$testing$outcome)
    )

    ## Save Data to database
    save_data <- list(
        resample = resample,
        samples_count = subset$samples_count, 
        results = subset$results, 
        results_raw = subset$results_raw,
        data_training = data$training, 
        data_testing = data$testing, 
        validation = data$validation)
     

    feature_set_data <- db.saveRawDataInformation(save_data, processing_id)
    rm(save_data)

    feature_set_id <- feature_set_data$fs_id
    feature_set_donors_testing <- feature_set_data$donors_testing

    skip_model <- FALSE
    if(is.null(feature_set_id)){
        cat(paste0("===> INFO: Skipping.. (not enough subject for analysis in dataset): ",feature_set_donors_testing," - ",feature_set_id," - ",resample$features_hash," \r\n"))
        skip_model <- TRUE
    }

    if(skip_model == FALSE && feature_set_donors_testing < 10){
        cat(paste0("===> INFO: Skipping.. (not enough subject for analysis in TEST dataset): ",feature_set_donors_testing," - ",feature_set_id," - ",resample$features_hash," \r\n"))
        skip_model <- TRUE
    }

    if(skip_model == TRUE){
        next
    }
   
    ## Save feature set to database
    db.saveFeatureSet(resample, data_input, feature_set_id)

    # 5. Make feature plots
    # md.trainingFeaturePlots(data$training, unique_title)

    results.model_training_fit = list()
    results.model_prediction = list()
    results.roc_auc = list()
    results.confusion_matrix = list()
    results.variable_importance = list()
    results.time_taken = list()

    models_to_process <- rev(models_to_process)
    for (model in models_to_process) {
        ## Skip unwanted models
        if(!is.null(models_restrict)){
            if(model %in% models_restrict == TRUE){

            }else{
                cat(paste0("===> INFO: Skipping model: ",model," - ",resample$features_hash," \r\n"))
                next
            }
        }

        ## Check if model is supported
        model_info <- getModelInfo(model = model, regex = FALSE)[[1]]
        if("Classification" %!in% model_info$type){
            cat(paste0("===> INFO: Only Classification models are supported. Current model: ", model, " Class Type: ", model_info$type, "\r\n"))
            next
        }
        if(is.null(model_info$prob) || class(model_info$prob) != "function"){
            cat(paste0("===> INFO: Only probabilistic models are supported. Current model: ", model, " Model Label: ", model_info$label, "\r\n"))
            next
        }
        ## Check if specific model for current feature set is already processed
        model_processed <- db.checkIfModelProcessed(feature_set_id, model, processing_id)

        if(model_processed == TRUE){
            cat(paste0("===> INFO: Skipping. Model is already processed. Model: ", model, "\r\n"))
            next
        }
        hp.detach("RSNNS")

        success <- TRUE

        ## START
        title <- paste0(model," - ",unique_title,"")
        
        cat(paste0("===> Started Model ",model," analysis at: ", Sys.time(), " STEP: ",step," Hash: ",resample$features_hash,"\r\n"))
        cat(paste0("===> ",title,"\r\n\r\n"))

        training_model <- md.trainModel(data$training, model, title)

        # Create model_list
        if (!is.null(training_model)) {
            title <- paste0(model," - ",unique_title,"")
            cat(paste0("Model ",model," Training finished: ",title,"\r\n"))

            ## Define results variables
            results.model_training_fit[[model]] <- training_model
            results.model_prediction[[model]] <- NULL
            results.roc_auc[[model]] <- NULL
            results.confusion_matrix[[model]] <- NULL
            results.variable_importance[[model]] <- NULL

            model_details_db_id <- 0

            predict_model <- md.predictModel(training_model, data$testing)

            if (!is.null(predict_model$pred_prob)) { 
                results.model_prediction[[model]] <- predict_model

                roc <-  md.testModelROC(predict_model$pred_prob, data$testing, title)

                if (!is.null(roc$roc_p)) { 
                    results.roc_auc[[model]] <- roc
                }

                temp.conf <-  md.confusionMatrix(predict_model$pred_raw, data$testing)
                if (!is.null(temp.conf)) { 
                    results.confusion_matrix[[model]] <- temp.conf
                }

                ## Save collected info to DB
                model_details_db_id <- db.saveMethodAnalysisData(
                    training_model,
                    results.confusion_matrix[[model]], 
                    resample,
                    model,
                    step, 
                    results.roc_auc[[model]],
                    feature_set_id,
                    1)

                temp.Imp <- md.variable_importance(training_model)
                if (!is.null(temp.Imp)) { 
                    results.variable_importance[[model]] <- temp.Imp

                    db.saveVariableImportance(temp.Imp,
                        resample, 
                        model,
                        model_details_db_id)
                }

            }else{
                cat(paste0("Model ",model," Training unsuccessful: ",title,"\r\n"))
                success <- FALSE
            } ## predict_model

        }else{
            success <- FALSE
        }

        if(success == FALSE){
            ## Save failed model so we don't process it again
            model_details_db_id <- db.saveMethodAnalysisData(
                NULL,
                NULL, 
                resample, 
                model,
                step, 
                NULL,
                feature_set_id,
                0)
        }else{
            save_data <- list(
                model_training_fit = results.model_training_fit[[model]],
                model_prediction = results.model_prediction[[model]], 
                roc_auc = results.roc_auc[[model]], 
                confusion_matrix = results.confusion_matrix[[model]],
                variable_importance = results.variable_importance[[model]]
            )
            ## Save data
            save_path <- gsub(" ", "_", paste0("output/models/", paste0(model , "_", resample$features_hash,"_", model_details_db_id), ".RData"), fixed = TRUE)
            save(save_data, file = save_path)
            rm(save_data) 
        }

        # hp.detach(model)
        rm(training_model)

    } ## END models
        
    ## Calculate feature_set features details
    db.recalculateDatasetFeatures(feature_set_id, processing_id)

    if(length(results.model_training_fit) > 1){
        ## Compare all models in with graph
        # graph.compare_models(results.model_training_fit, unique_title)
        
        ## Write CSV output
        #  saveRDS(list.models,"data/models.RDS")
        ## graph.csv_training_table(results.model_training_fit, results.roc_auc, results.confusion_matrix, results.variable_importance)
    }

    rm(results.model_training_fit)
    rm(results.roc_auc)
    rm(results.confusion_matrix)
    rm(results.variable_importance)
    dev.off()

    rm(resample)
} ## END Features json loop

cat(paste0("==> INFO: End looping features for: ",processing_id,"\r\n"))
cat(paste0("======> INFO: PROCESSING END..\r\n"))
