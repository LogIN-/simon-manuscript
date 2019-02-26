graph.csv_training_table <- function(t2, results.roc, results.confMatrix, results.VarImp, save.data.name) {
    # https://rstudio.github.io/DT/
    # https://rstudio.github.io/DT/extensions.html
    require(DT)
    require(magrittr)
    # preallocate data types
    i = 1; MAX = length(t2);
    x1 <- character() # Name
    x2 <- numeric()   # R2
    x3 <- numeric()   # RMSE
    x4 <- numeric()   # time [s]
    x5 <- character() # long model name
    # fill data and check indexes and NA with loop/lapply 
    for (i in 1:length(t2)) {
        x1[i] <- t2[[i]]$method
        x2[i] <- as.numeric(round(getTrainPerf(t2[[i]])$TrainAccuracy,4))
        x3[i] <- as.numeric(round(getTrainPerf(t2[[i]])$TrainKappa,4))
        x4[i] <- as.numeric(t2[[i]]$times$everything[3])
        x5[i] <- t2[[i]]$modelInfo$label
    }

    # coerce to data frame
    df1 <- data.frame(x1,x2,x3,x4,x5, stringsAsFactors=FALSE)

    # call web output with correct column names
    out_table <- DT::datatable(df1,  options = list(
            columnDefs = list(list(className = 'dt-left', targets = c(0,1,2,3,4,5))),
            pageLength = MAX,
            order = list(list(2, 'desc'))),
            colnames = c('Num', 'Name', 'Accuracy', 'Kappa', 'time [s]', 'Model name'),
                caption = paste('Classification results from caret models',Sys.time()),
                class = 'cell-border stripe')  %>%         
                formatRound('x2', 3) %>%  
                formatRound('x3', 3) %>%
                formatRound('x4', 3) %>%
                formatStyle(2,
                background = styleColorBar(x2, 'steelblue'),
                backgroundSize = '100% 90%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center'
    )
    # write.csv(out_table, paste0("output/data/", save.data.name ,"_table_" ,format(Sys.time(), "%d-%m-%Y-T%H-%M"), ".csv"))
}

graph.compare_models <- function(results.models, unique_title) {
    # numResamp <- unlist(lapply(results.models, function(x) length(x$control$index)))
    # print(numResamp)
    # create resamples object
    resultValues <- resamples(results.models)
    # print(resultValues)
    hp.saveSummary(paste0("output/combined/", unique_title ,"_", format(Sys.time(), "%d-%m-%Y-T%H-%M")), resultValues)

    xy_plot <- tryCatch(
        {
            xyplot(resultValues, 
                  main=list(
                    label=paste0("xy_plot - ",unique_title),
                    cex=0.75),
                  xlab=list(
                    cex=0.75),
                  ylab=list(
                    cex=0.75),
                  scales=list(cex=0.5)
                )
        },
        error=function(cond) {
            return(NULL)
        },
        warning=function(cond) {
            message("ERROR: xy_plot:")
            message(cond)
            return(NULL)
        },
        finally={}
    )   

    if(!is.null(xy_plot)){
        print(xy_plot)
        rm(xy_plot)
    }

    for (metric in resultValues$metrics) {
        dot_plot_v1 <- tryCatch(
            {
                dotplot(resultValues, metric = metric, main=paste0("dot_plot_v1 - ",unique_title))
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

        if(!is.null(dot_plot_v1)){
            print(dot_plot_v1)
            rm(dot_plot_v1)
        }

        metric_diff <- tryCatch(
            {
                diff(resultValues, metric = metric)
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

        if(!is.null(metric_diff)){
            hp.saveSummary(paste0("output/combined/", unique_title ,"_rocDiffs_",metric,"_", format(Sys.time(), "%d-%m-%Y-T%H-%M")), resultValues)
            print(metric_diff)

            dot_plot_v2 <- tryCatch(
                {
                    dotplot(metric_diff, metric = metric, main=paste0("dot_plot_v2 - ",unique_title))
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

            if(!is.null(dot_plot_v2)){
                print(dot_plot_v2)
                rm(dot_plot_v2)
            }
            rm(metric_diff)
        }

        rm(metric)
    }
}
