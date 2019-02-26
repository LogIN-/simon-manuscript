
'%!in%' <- function(x,y)!('%in%'(x,y))

hp.detach <- function(package_name) {
    search_item <- paste("package", package_name, sep = ":")
    
    while (search_item %in% search()) {
        unload_package <- tryCatch({
            detach(search_item,
                   character.only = TRUE,
                   unload = TRUE)
        },
        error = function(msg) {
            cat(
                paste0(
                    "===> ERROR: (unload_package) ",
                    match.call()[[1]],
                    " (error): ",
                    msg,
                    "\r\n"
                )
            )
            return(NULL)
        },
        warning = function(msg) {
            cat(
                paste0(
                    "===> ERROR: (unload_package) ",
                    match.call()[[1]],
                    " (warning): ",
                    msg,
                    "\r\n"
                )
            )
            return(NULL)
        },
        finally = {
            
        })
    }
    
    ## dso_filenames <- dir(tempdir(), pattern = .Platform$dynlib.ext)
    ## filenames  <- dir(tempdir())
    ## for (i in seq(dso_filenames)) {
    ##     if (package_name == dso_filenames[i]) {
    ##         dyn.unload(file.path(tempdir(), dso_filenames[i]))
    ##     }
    ## }
    ## for (i in seq(filenames)) {
    ##     if (file.exists(file.path(tempdir(), filenames[i])) &
    ##         nchar(filenames[i]) < 42) {
    ##         # some files w/ long filenames that didn't like to be removeed
    ##         if (package_name == filenames[i]) {
    ##             file.remove(file.path(tempdir(), filenames[i]))
    ##         }
    ##     }
    ## }
}

# The dataset requires defined training and test subsets so let's
# remove some of the variables that don't see to add to the value and
# create these
hp.createPartitions <- function(data, split = 0.75, use.validation= FALSE)
{
    set.seed(1234)
    validation <- NULL
    
    # Split data: 75% training, 25% testing.
    inTrain <- createDataPartition(y = data$outcome, p = split, list = FALSE)

    training <<- data[inTrain,]
    testing <<- data[-inTrain,]

    # If using validation set, split data: 45% training, 30% validation, 25% testing.
    if(use.validation) {
        inValidation <<- createDataPartition(y = data$outcome, p=.4, list=FALSE)   
        validation <<- training[inValidation,]
        training <<- training[-inValidation,]
    }

    list(training = training, testing = testing, validation = validation)
}

hp.saveSummary.summary <- function(model) {
    out <- tryCatch({
        summary(model)
    },
    error = function(msg) {
        cat(paste0("===> ERROR: ", match.call()[[1]], " (error): ", msg, "\r\n"))
        return(NULL)
    },
    warning = function(msg) {
        cat(paste0("===> ERROR: ", match.call()[[1]], " (warning): ", msg, "\r\n"))
        return(NULL)
    },
    finally = {
        
    })
    return(out)
}

hp.saveSummary <- function(save_path, model)
{
    save_path <- gsub(" ", "_", save_path, fixed = TRUE)
    
    model.summary <- hp.saveSummary.summary(model)
    
    if (!is.null(model.summary)) {
        sink(save_path)
            print(model.summary)
        sink()
    } else{
        sink(save_path)
            print(model)
        sink()
    }
}
# description: generate list of seeds for caret::trainControl. This is
#     useful for parallel processing as seeds wont be reproducible.
# args:
#        init.seed: starting seed
#           kFolds: number of folds#
#        cvRepeats: number of repeats for cross validation
#       tuneLength: number of tuning parameter combos

# returns: a list of seeds for use in caret::trainControl
hp.set.seed.cv <- function(init.seed, kFolds, cvRepeats, tuneLength)
{    
    set.seed(init.seed)
    seeds <-
        vector(mode = "list", length = kFolds * cvRepeats + 1)#length is = (n_repeats*nresampling)+1
    for (i in 1:(kFolds * cvRepeats)) {
        seeds[[i]] <- sample.int(n = 1000, tuneLength * cvRepeats)
    }
    
    seeds[[kFolds * cvRepeats + 1]] <-
        sample.int(1000, 1)#for the last model
    
    return(seeds)
}
