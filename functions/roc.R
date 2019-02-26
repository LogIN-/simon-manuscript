plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$outcome == 'high', "TP", v)
  v <- ifelse(df$pred >= threshold & df$outcome == 'low', "FP", v)
  v <- ifelse(df$pred < threshold & df$outcome == 'high', "FN", v)
  v <- ifelse(df$pred < threshold & df$outcome == 'low', "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=outcome, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}

# https://github.com/joyofdata/joyofdata-articles/blob/master/roc-auc/calculate_roc.R
# roc <- calculate_roc(predictions, 1, 2, n = 100)
calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$outcome == 1) / sum(df$outcome == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$outcome == 0) / sum(df$outcome == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$pred >= threshold & df$outcome == 0) * cost_of_fp + 
      sum(df$pred < threshold & df$outcome == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}

# https://github.com/joyofdata/joyofdata-articles/blob/master/roc-auc/calculate_roc.R
# roc <- calculate_roc(predictions, 1, 2, n = 100)
# https://github.com/joyofdata/joyofdata-articles/blob/master/roc-auc/plot_roc.R
# plot_roc(roc, 0.7, 1, 2)
plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  require(gridExtra)
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)

  grid.arrange(p_roc, p_cost, ncol=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}
## This function averages the class probability values per sample
## across the hold-outs to get an averaged ROC curve

roc_compute <- function(object, best_only = TRUE, ...) {
  caret:::requireNamespaceQuietStop("pROC")
  caret:::requireNamespaceQuietStop("plyr")

  if(object$modelType != "Classification")
    cat("===> ERROR: ROC curves are only available for classification models")
    return(NULL)

  if(!any(names(object$modelInfo) == "levels"))
    cat("===> ERROR: The model's code is required to have a 'levels' module. See http://topepo.github.io/caret/custom_models.html#Components")
    return(NULL)

  lvs <- object$modelInfo$levels(object$finalModel)
  if(length(lvs) != 2) 
    cat("===> ERROR: ROC curves are only implemented here for two class problems")
    return(NULL)
  
  ## check for predictions
  if(is.null(object$pred)) 
    cat("===> ERROR: The out of sample predictions are required. See the `savePredictions` argument of `trainControl`")
    return(NULL)
  
  if(best_only) {
    object$pred <- merge(object$pred, object$bestTune)
  }
  ## find tuning parameter names
  p_names <- as.character(object$modelInfo$parameters$parameter)
  p_combos <- object$pred[, p_names, drop = FALSE]
  
  ## average probabilities across resamples
  object$pred <- plyr::ddply(.data = object$pred, 
                             .variables = c("obs", "rowIndex", p_names),
                             .fun = function(dat, lvls = lvs) {
                               out <- mean(dat[, lvls[1]])
                               names(out) <- lvls[1]
                               out
                             })
  
  make_roc <- function(x, lvls = lvs, nms = NULL, ...) {
    out <- pROC::roc(response = x$obs,
                     predictor = x[, lvls[1]],
                     levels = rev(lvls))
    
    out$model_param <- x[1,nms,drop = FALSE]
    out
  }
  out <- plyr::dlply(.data = object$pred, 
                     .variables = p_names,
                     .fun = make_roc,
                     lvls = lvs,
                     nms = p_names)
  if(length(out) == 1)  out <- out[[1]]
  out
}
## https://www.r-bloggers.com/roc-curves-in-two-lines-of-r-code/
## https://www.r-bloggers.com/auc-meets-the-wilcoxon-mann-whitney-u-statistic/
# U_illustration_part1(test_set$bad_widget, glm_response_scores)
U_illustration_part1 <- function(labels, scores){
  # put cases in order by score
  sort_order <- order(scores)
  labels <- labels[sort_order]
  scores <- scores[sort_order]
  
  # count the cases
  n <- length(labels)
  
  # find overall rank for each case by score
  ranks <- rank(scores)
  
  # start with an empty plot
  plot(c(0, n), c(0, n), type='n',
       xlab="rank", ylab="case", asp=1)
  
  # draw a grid in the background
  abline(h=0:n, col="lightblue")
  abline(v=0:n, col="lightblue")
  
  # plot bars representing ranks of all cases
  mapply(rectangle, x=0, y=(n - 1):0,  # starting from the top 
         width=ranks, height=1, 
         density=NA, border="black", lwd=2, col=c("red", "green")[1 + labels])
  
  legend("topright", legend=c("negative case", "positive case"), 
         text.col=c("red", "green"), bty='o', box.lwd=1, inset=0.1)
}
