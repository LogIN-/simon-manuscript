db.clear  <- function(){
    driver <- dbDriver("MySQL");
    conn <- dbConnect(driver, user=DATABASE_CREDS$username, password=DATABASE_CREDS$password, host=DATABASE_CREDS$host, dbname=DATABASE_CREDS$dbname, DBMSencoding = "UTF-8");
    on.exit(dbDisconnect(conn))
    
    clear_database <- "TRUNCATE `model_details`;"
    dbGetQuery(conn, clear_database)

    clear_database <- "TRUNCATE `model_variables`;"
    dbGetQuery(conn, clear_database)

    clear_database <- "TRUNCATE `model_features`;"
    dbGetQuery(conn, clear_database)

    clear_database <- "TRUNCATE `feature_sets`;"
    dbGetQuery(conn, clear_database)
}

db.saveFeatureSet <- function(json_data, data, feature_set_id){ 
    driver <- dbDriver("MySQL");
    conn <- dbConnect(driver, user=DATABASE_CREDS$username, password=DATABASE_CREDS$password, host=DATABASE_CREDS$host, dbname=DATABASE_CREDS$dbname, DBMSencoding = "UTF-8");
    on.exit(dbDisconnect(conn))

    data_source <- dbEscapeStrings(conn, json_data$data_source)
    features_hash <- json_data$features_hash

    feature_data <- as.data.frame(json_data$features)
    colnames(feature_data) <- c("feature_name")

    feature_data$split_difference_median <- NA

    data_main <- melt(data, id=c("outcome"))

    for (feature in feature_data$feature_name){

        data_t <- data_main[data_main$variable %in% feature,]
        if(nrow(data_t) > 0){
            ## https://www.skillsyouneed.com/num/percent-change.html
            data_median <- aggregate(cbind(value) ~ variable + outcome, data = data_t, FUN = function(x) round(median(as.numeric(as.character(x)))))
            
            # 2. Calculate outcome difference in median values of feature
            split_difference_h <-  as.numeric(as.character(data_median[data_median$outcome == 'high', ]$value))
            split_difference_l <-  as.numeric(as.character(data_median[data_median$outcome == 'low', ]$value))

            split_difference_median <-  (split_difference_h - split_difference_l)
            split_diff <- (split_difference_median / split_difference_h) * 100
            feature_data$split_difference_median[feature_data$feature_name == feature] <- round(split_diff, 2)
        }
    }

    # Begin the query
    query <- "INSERT IGNORE INTO `model_features` (`id`, `fs_id`, `feature_name`, `split_difference_median`) VALUES "
    # Finish it with
    query <- paste0(query, paste(sprintf("(NULL, '%s', '%s', '%s')", feature_set_id, feature_data$feature_name, feature_data$split_difference_median), collapse = ","))

    rs <- dbSendQuery(conn, query)
    dbClearResult(rs)
}

db.saveRawDataInformation <- function(save_data, processing_id){

    driver <- dbDriver("MySQL");
    conn <- dbConnect(driver, user=DATABASE_CREDS$username, password=DATABASE_CREDS$password, host=DATABASE_CREDS$host, dbname=DATABASE_CREDS$dbname, DBMSencoding = "UTF-8");
    on.exit(dbDisconnect(conn))

   
    proportions_training_p <- prop.table(table(save_data$data_training$outcome))
    proportions_training_no <- table(save_data$data_training$outcome)

    if("high" %in% names(proportions_training_p))
    {
        training_h_p <- round(proportions_training_p[["high"]], 2)
        training_h_no <- round(proportions_training_no[["high"]], 2)
        if("low" %in% names(proportions_training_p))
        {
            training_l_p <- round(proportions_training_p[["low"]], 2)
            training_l_no <- round(proportions_training_no[["low"]], 2)
        }else{
            training_l_p <-NULL
            training_l_no <-NULL
        }
    }else{
        training_h_p <- NULL
        training_h_no <-NULL
    }

    if(is.null(training_h_p) || is.null(training_l_p)){
        return(list(
            fs_id = NULL,
            donors_testing = NULL)
        )
    }

    proportions_testing_p <- prop.table(table(save_data$data_testing$outcome))
    proportions_testing_no <- table(save_data$data_testing$outcome)

    if("high" %in% names(proportions_testing_p))
    {
        testing_h_p <- round(proportions_testing_p[["high"]], 2)
        testing_h_no <- round(proportions_testing_no[["high"]], 2)
        if("low" %in% names(proportions_testing_p))
        {
            testing_l_p <- round(proportions_testing_p[["low"]], 2)
            testing_l_no <- round(proportions_testing_no[["low"]], 2)
        }else{
            testing_l_p <-NULL
            testing_l_no <-NULL
        }
    }else{
        testing_h_p <- NULL
        testing_h_no <-NULL
    }

    raw_donors_count <- as.numeric(as.character(save_data$samples_count))
    features <- as.numeric(save_data$resample$feature_count)

    data_source <- dbEscapeStrings(conn, save_data$resample$data_source)

    donors_testing <- as.numeric(testing_h_no + testing_l_no)
    # Begin the query
    query <- paste0("INSERT INTO 
    `feature_sets` 

    (`id`, `features_hash`, `processing_id`, `data_source`, `raw_donors_count`, `features`, `training_h_p`, `training_h_no`, 
    `training_l_p`, `training_l_no`, `testing_h_p`, `testing_h_no`, `testing_l_p`, `testing_l_no`) 

    VALUES 

    (NULL, '",save_data$resample$features_hash,"', '",processing_id,"', '",data_source,"', '",raw_donors_count,"', '",features,"', '",training_h_p,"', '",training_h_no,"', '",training_l_p,"', 
    '",training_l_no,"', '",testing_h_p,"', '",testing_h_no,"', '",testing_l_p,"', '",testing_l_no,"')

    ON DUPLICATE KEY UPDATE id=LAST_INSERT_ID(id)")
        
    rs <- dbSendQuery(conn, query)

    dbClearResult(rs)

    id <- dbGetQuery(conn, "select last_insert_id();")[1,1]


    dir.create(paste0("output/data/",id), showWarnings = FALSE)

    path <- paste0("output/data/",id,"/results.feather")
    write_feather(save_data$results, path)

    path <- paste0("output/data/",id,"/results_raw.feather")
    write_feather(save_data$results_raw, path)

    path <- paste0("output/data/",id,"/data_training.feather")
    write_feather(save_data$data_training, path)

    path <- paste0("output/data/",id,"/data_testing.feather")
    write_feather(save_data$data_testing, path)

    return(list(
        fs_id = id,
        donors_testing = donors_testing)
    )
}

## https://taichimd.us/mediawiki/index.php/MySQL
db.saveMethodAnalysisData <- function(training_model, confmatrix, json_data, method, step, roc, feature_set_id, status){
    driver <- dbDriver("MySQL");
    conn <- dbConnect(driver, user=DATABASE_CREDS$username, password=DATABASE_CREDS$password, host=DATABASE_CREDS$host, dbname=DATABASE_CREDS$dbname, DBMSencoding = "UTF-8");
    on.exit(dbDisconnect(conn))

    uid <- 0 

    step <- step
    method <- dbEscapeStrings(conn, method)

    if(!is.null(training_model)){
        train_pref <- getTrainPerf(training_model)

        t_auc <- as.numeric(round(train_pref$TrainAUC, 4))
        t_accuracy <- as.numeric(round(train_pref$TrainAccuracy, 4))
        t_kappa <- as.numeric(round(train_pref$TrainKappa, 4))
        t_sensitivity <- as.numeric(round(train_pref$TrainSensitivity, 4))
        t_specificity <- as.numeric(round(train_pref$TrainSpecificity, 4))

        time <- round(as.numeric(max(training_model$times$everything)))
    }else{
        t_auc <- NULL
        t_accuracy <- NULL
        t_kappa <- NULL
        t_sensitivity <- NULL
        t_specificity <- NULL

        time <- NULL
    }

    if(!is.null(confmatrix)){
        p_sensitivity <- round(as.numeric(confmatrix$byClass["Sensitivity"]), 4)
        p_specificity <- round(as.numeric(confmatrix$byClass["Specificity"]), 4)
        positive_ctr <- confmatrix$positive
        p_accuracy <- round(as.numeric(confmatrix$overall[1]), 4)
        p_kappa <-round(as.numeric(confmatrix$overall[2]), 4)
    }else{
        p_sensitivity <- NULL
        p_specificity <- NULL
        positive_ctr <- NULL
        p_accuracy <- NULL
        p_kappa <- NULL
        status <- 0
    }

    if(!is.null(roc) & !is.null(roc$auc_p)){
        p_auc <- round(as.numeric(roc$auc_p), 4)
    }else{
        p_auc <- NULL
    }

    if(is.null(p_auc) & is.null(p_accuracy)){
        status <- 0
    }

    ## Checked Prediction Scores
    if(status == 1){
        if(is.null(p_sensitivity) & is.null(p_specificity) & is.null(p_kappa)){
            status <- 0
        }else if(is.null(p_auc)){
            status <- 3
        }else if(p_specificity == 1 || p_sensitivity == 1){
            status <- 3
        }else if(p_sensitivity == 0 || p_specificity == 0){
            status <- 3
        }else if(p_sensitivity < 0.5 || p_specificity < 0.5 || p_auc < 0.5){
            status <- 3
        }
    }
    ## Checked Training Scores
    if(status == 1 || status == 3){
        if(is.null(t_sensitivity) & is.null(t_specificity) & is.null(t_kappa)){
            status <- 0
        }else if(is.null(t_auc)){
            status <- 0
        }else if(t_specificity == 1 || t_sensitivity == 1){
            status <- 0
        }else if(t_sensitivity == 0 || t_specificity == 0){
            status <- 0
        }else if(t_sensitivity < 0.5 || t_specificity < 0.5 || t_auc < 0.5){
            status <- 2
        }
    }

    query <- paste0("INSERT IGNORE INTO `model_details` 

        (`id`, `uid`, `fs_id`, `status`, `step`, `method`,
        `p_accuracy`, `p_kappa`, `p_auc`, `p_sensitivity`, `p_specificity`, 
        `t_accuracy`, `t_kappa`, `t_auc`, `t_sensitivity`, `t_specificity`, 
        `positive_ctr`, `time`, `features_hash_var_imp`, `timestamp`)

        VALUES 

        (NULL, NULL, '",feature_set_id,"', '",status,"', '",step,"', '",method,"', 
        '",p_accuracy,"', '",p_kappa,"', '",p_auc,"','",p_sensitivity,"','",p_specificity,"',
        '",t_accuracy,"', '",t_kappa,"', '",t_auc,"','",t_sensitivity,"','",t_specificity,"',
        '",positive_ctr,"', '",time,"', NULL, NOW() );")

    rs <- dbSendQuery(conn, query)
    dbClearResult(rs)

    id <- dbGetQuery(conn, "select last_insert_id();")[1,1]

    return(id)
}

db.checkIfModelProcessed <- function (feature_set_id, method, processing_id){
    driver <- dbDriver("MySQL");
    conn <- dbConnect(driver, user=DATABASE_CREDS$username, password=DATABASE_CREDS$password, host=DATABASE_CREDS$host, dbname=DATABASE_CREDS$dbname, DBMSencoding = "UTF-8");
    on.exit(dbDisconnect(conn))

    method <- dbEscapeStrings(conn, method)

    query <- paste0("SELECT * 
        FROM model_details

        INNER JOIN feature_sets
            ON model_details.fs_id = feature_sets.id

        WHERE fs_id = ",feature_set_id," 
            AND feature_sets.processing_id = ",processing_id,"
            AND method = '",method,"' 
        LIMIT 1;")
    
    response <- dbSendQuery(conn, query)
    results <- unique(dbFetch(response, n=-1))
    row_count <- nrow(results)


    if(row_count == 0){
        status <- FALSE
    }else{
        status <- TRUE
    }
    return (status)
}

db.saveVariableImportance <- function(imp, json_data, method, model_details_db_id){
    require(digest)
    driver <- dbDriver("MySQL");
    conn <- dbConnect(driver, user=DATABASE_CREDS$username, password=DATABASE_CREDS$password, host=DATABASE_CREDS$host, dbname=DATABASE_CREDS$dbname, DBMSencoding = "UTF-8");
    on.exit(dbDisconnect(conn))

    data_source <- dbEscapeStrings(conn, json_data$data_source)
    method <- dbEscapeStrings(conn, method)
    features_hash <- json_data$features_hash
    model_details_db_id <-as.numeric(model_details_db_id)
    
    # Begin the query
    query <- "INSERT INTO `model_variables` 
    (`id`, `md_id`, `uid`, `feature_name`, `score_perc`, `score_no`, `rank`, `timestamp`) VALUES"
    # Finish it with
    query <- paste0(query, paste(sprintf("(NULL, '%s', NULL, '%s', '%s', '%s', '%s', NOW() )", model_details_db_id, imp$features, imp$score_perc, imp$score_no, imp$rank), collapse = ","))

    rs <- dbSendQuery(conn, query)
    dbClearResult(rs)


    features <- imp[order(imp$features, decreasing=F),]$features
    sql <- paste(features, collapse = '\',\'')
    features_hash_var_imp <- digest(sql, algo="md5", serialize=F)

    ## Save var_im_feature_hash
    query <- paste0("UPDATE `model_details` SET `features_hash_var_imp` = '",features_hash_var_imp,"' WHERE `model_details`.`id` = ",model_details_db_id,";")
    rs <- dbSendQuery(conn, query)
    dbClearResult(rs)
}


db.recalculateDatasetFeatures <- function(fs_id, processing_id){

    driver <- dbDriver("MySQL");
    conn <- dbConnect(driver, user=DATABASE_CREDS$username, password=DATABASE_CREDS$password, host=DATABASE_CREDS$host, dbname=DATABASE_CREDS$dbname, DBMSencoding = "UTF-8");
    on.exit(dbDisconnect(conn))

    dataset_features <- NULL

    query <- paste0("SELECT
                        model_variables.feature_name,
                        ROUND(model_variables.score_perc) AS score,
                        model_variables.rank,
                        (SELECT COUNT(*) FROM model_details

                            LEFT JOIN feature_sets
                                ON model_details.fs_id = feature_sets.id

                            WHERE model_details.status IN (1,3)

                                AND model_details.fs_id = ",fs_id,"
                                AND feature_sets.processing_id = ",processing_id,"

                        ) AS m_counts,
                        temp_count.r_counts AS r_counts,
                        model_features.split_difference_median AS split_difference_median

                    FROM model_variables

                    INNER JOIN (SELECT
                                model_variables.feature_name,
                                COUNT(model_variables.feature_name) as r_counts

                                FROM model_variables

                                LEFT JOIN model_details
                                    ON model_variables.md_id = model_details.id

                                LEFT JOIN feature_sets
                                    ON model_details.fs_id = feature_sets.id

                                WHERE 
                                    model_details.fs_id = ",fs_id,"
                                    AND feature_sets.processing_id = ",processing_id,"
                                    AND model_details.status IN (1,3)

                                GROUP BY model_variables.feature_name) temp_count
                                ON model_variables.feature_name = temp_count.feature_name

                    LEFT JOIN model_details
                        ON model_variables.md_id = model_details.id

                    LEFT JOIN feature_sets
                        ON model_details.fs_id = feature_sets.id

                    LEFT JOIN model_features
                        ON model_details.fs_id = model_features.fs_id
                        AND model_variables.feature_name = model_features.feature_name

                    WHERE model_details.fs_id = ",fs_id," 
                        AND feature_sets.processing_id = ",processing_id,"
                        AND model_details.status IN (1,3)

                    ORDER BY model_variables.feature_name DESC")
    response <- dbSendQuery(conn, query)

    results <- dbFetch(response, n=-1)

    if(nrow(results) > 0){
        results$indataset_occurance <- round((results$r_counts/results$m_counts)*100)
        ## Calculate MEAN of score and Rank Variables
        dataset_features <- aggregate(cbind(score, rank, split_difference_median) ~ feature_name + indataset_occurance, data = results, FUN = function(x) round(mean(as.numeric(as.character(x)))))
        
        for(step in 1:nrow(dataset_features)) {
            feature <- dataset_features[step,]

            update_query <- paste0("UPDATE model_features 
                SET indataset_occurance = '",feature$indataset_occurance,"', score_mean = '",feature$score,"', 
                rank_mean = '",feature$rank,"', split_difference_mean = '",feature$split_difference_median,"' 
                WHERE model_features.fs_id = ",fs_id," AND model_features.feature_name = '",feature$feature_name,"';")

            rs <- dbSendQuery(conn, update_query)
            dbClearResult(rs)
        }
    }
}
