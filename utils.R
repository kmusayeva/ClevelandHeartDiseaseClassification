
# Computec pairwise Cramér’s V
compute_cramer_v_matrix <- function(data) {
  vars <- names(data)
  n <- length(vars)
  mtx <- matrix(NA, n, n)
  rownames(mtx) <- colnames(mtx) <- vars
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      tbl <- table(data[[i]], data[[j]])
      mtx[i, j] <- suppressWarnings(DescTools::CramerV(tbl))
    }
  }
  mtx
}

# PLots confusion matrix
plot_confusion_matrix <- function(actual, predicted, eval_measure, title, positive_label = "1") {
  
  cm <- confusionMatrix(predicted, actual, positive = positive_label, )
  
  sensi <- round(cm$byClass[1], 2) # sensitivity
  
  speci <- round(cm$byClass[2], 2) # specificity
  
  cm_df <- as.data.frame(cm$table)
  
  colnames(cm_df) <- c("Predicted", "Ground_truth", "Freq")
  
  
  cm_df$Label <- with(cm_df, ifelse(Predicted == positive_label & Ground_truth == positive_label, "TP",
                                    ifelse(Predicted == positive_label & Ground_truth != positive_label, "FP",
                                           ifelse(Predicted != positive_label & Ground_truth == positive_label, "FN", "TN"))))
  
  
  p <- ggplot(data = cm_df, aes(x = Ground_truth, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(Label, "\n", Freq)), size = 7, color = "black") +
    scale_fill_gradient(low = "#E8F0F9", high = "#4682B4") +
    theme_minimal(base_size = 17) +
    labs(title = title, x = "Ground_truth", y = "Predicted") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
    xlab("Ground truth") +
    labs(title = paste(title, "\n\n MCC:", eval_measure, ", Sensi:", sensi, ", Speci:", speci))+
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  
  
  p
  
}



plot_missingness_distribution <- function(dat, var) {
    tmp <- sym(var)
    gg_miss_fct(dat, fct = !!tmp) +
      ylab(NULL) +  
      theme(
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size=14),
        legend.title=element_text(size=14)
      )
    

}


# Evaluates model performance
evaluate_model_fit <- function(ground_truth, probs) {
  
  threshold <- 0.5
  
  pred_class <- factor(ifelse(probs > threshold, 1, 0), levels=c(1, 0)) 
  
  sensi <- round(sens_vec(truth = ground_truth, estimate = pred_class), 2)
  
  speci <- round(spec_vec(truth = ground_truth, estimate = pred_class), 2)
  
  f1 <- round(f_meas_vec(truth = ground_truth, estimate = pred_class), 2)
  
  aucroc <- round(roc_auc_vec(truth = ground_truth, probs), 2)
  
  matcc <- mcc_vec(ground_truth, pred_class)
  
  acc <- accuracy_vec(ground_truth, pred_class)
  
  list("acc"=acc, "sensi" = sensi, "speci" = speci, "f1" = f1, "aucroc" = aucroc, "matcc" = matcc, "pred_class"=pred_class)
  
  }



xgb_tune <- function(params_list, dtrain, nrounds = 1000) {
  cv_results <- list()
  
  for(i in 1:nrow(params_list)){
    
    params <- list(
      objective = "binary:logistic",
      eval_metric = "error", 
      booster = "dart",  
      eta = params_list$eta[i],
      max_depth = params_list$max_depth[i],
      subsample = params_list$subsample[i],
      colsample_bytree = params_list$colsample_bytree[i],
      gamma = params_list$gamma[i],             
      min_child_weight = params_list$min_child_weight[i] 
    )
    
    cv <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = nrounds,
      nfold = 10,
      early_stopping_rounds = 50,  # Adjust based on experimentation
      maximize = FALSE,
      verbose = 0,
      prediction = TRUE
    )
    
    best_iter <- cv$best_iteration
    best_error <- cv$evaluation_log[best_iter, test_error_mean]
    best_accuracy <- 1 - best_error
    
    cv_results[[i]] <- data.table(
      eta = params$eta,
      max_depth = params$max_depth,
      subsample = params$subsample,
      colsample_bytree = params$colsample_bytree,
      gamma = params$gamma,
      min_child_weight = params$min_child_weight,
      best_iteration = best_iter,
      accuracy = best_accuracy
    )
  }
  rbindlist(cv_results)
}



best_threshold <- function(ground_truth, probs) {
  
    roc_obj <- roc(response = ground_truth, predictor = probs)
    
    coords(roc_obj, "best", best.method = c("youden", "closest.topleft"))
  
    }




plot_roc <- function(df, title) {
  
  roc_curve(df, ground_truth, probs) %>%
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(lty = 4) +
    coord_equal() +
    theme_minimal(base_size = 18)+
    theme(
      plot.title = element_text(hjust = 0.5, size = 20), 
      panel.spacing = unit(1.2, "lines"),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size=20),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
      ) +
    labs(title = title)

}


plot_wrong_predictions <- function(df, target, var, what_var) {
  
   tmp <- sym(var)
  
   ggplot(df, aes(x = !!tmp, fill = !!sym(target))) +
    geom_density(alpha = 0.4) +
    
    # Rug plot for incorrect predictions
    geom_rug(data = subset(df, outcome %in% c("False Positive", "False Negative")),
             aes(color = outcome),
             sides = "b", alpha = 0.7) +
    
    # Manual fill colors for stroke = 0 and 1
    scale_fill_manual(values = c("0" = "#A6CEE3", "1" = "#33A02C"),
                      labels = c("Negative", "Positive"),
                      name = "Disease Status") +
    
    # Manual colors for rug (incorrect predictions)
    scale_color_manual(values = c("False Positive" = "blue", "False Negative" = "red"),
                       name = "Incorrect Prediction") +
    
    labs(x = what_var, y = "Density") +
    theme_minimal(base_size = 14)
  
}



plot_continuous_nicely <- function(df, var, title, col) {
  
  var_eval <- sym(var)
  
  # Histogram of rest_blood_pressure
  p_hist <- ggplot(df, aes(x = !!var_eval)) +
    geom_histogram(fill = col, bins=50)+
    geom_vline(xintercept = median(dat[[var]], na.rm = TRUE),
               color = "red", linetype = "dashed", size = 0.7) +
    labs(x = title, y = "") +
    theme_minimal(base_size = 18)
  
  
  # Calculate the quantiles
  quantiles <- quantile(df[[var]], probs = c(0, 0.25, 0.75, 1), na.rm=TRUE)
  
  # Create a data frame with quantile names and values
  quantile_df <- data.frame(
    quantile = factor(c("Min", "Q1", "Q3", "Max"),
                      levels = c("Min", "Q1", "Q3", "Max")),
    value = round(as.numeric(quantiles))
  )
  
  p_box <- ggplot(df, aes(x = !!var_eval, y = 1)) +
    geom_boxplot(fill = col, width = 0.2, outlier.shape = NA) +
    geom_jitter(width = 0.1, height = 0.05, color = col, alpha = 0.2) +
    geom_text(data = quantile_df, 
              aes(x = value-1, y = 1, label = round(value, 2)),
              size = 5, vjust = 0, nudge_x = 0.8, check_overlap = TRUE) +
    theme_minimal(base_size = 15) +
    theme(
          plot.title = element_text(hjust = 0.5),
          axis.title  = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
      
  
  # Combine the histogram and boxplot vertically.
  egg::ggarrange(p_hist, p_box, heights = 2:1, draw=FALSE)
  
      }


plot_exposure_disease <- function(df, disease_var, exposure_var, exposure_title, disease_title) {
  
  disease_var_eval <- sym(disease_var)
  
  exposure_var_eval <- sym(exposure_var)
  
  
  p1 <- ggplot(df, aes(x = !!disease_var_eval, y = !!exposure_var_eval, color = !!disease_var_eval)) + 
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 1.5)+
    # scale_fill_brewer(palette = "Set3") +
    # scale_color_brewer(palette = "Set2") +
    labs(x = disease_title, y = exposure_title, color = disease_var) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") 
  
  
  mhats <- df %>% group_by(!!disease_var_eval) %>% summarise(mhat = mean(!!exposure_var_eval)) 
  
  
  p2 <- ggplot(df, aes(x=!!exposure_var_eval, fill=!!disease_var_eval)) +  geom_density(alpha=0.4) +
    geom_vline(data = mhats, aes(xintercept=mhat, color=!!disease_var_eval), linetype="dashed")+
    # scale_fill_brewer(palette = "Set3") +
    # scale_color_brewer(palette = "Set2") +
    labs(x = exposure_title, y = "Density", color = disease_var) +

    theme_minimal(base_size = 14) 
  
  
  egg::ggarrange(p1, p2, widths = c(2, 1), draw=FALSE)
  
  
  }



plot_odds_ratio <- function(results, var, ref_group, col) {
  
  # results <- results %>% filter(term != "(Intercept)")
  
  ggplot(results, aes(x = term, y = estimate)) +
    geom_bar(stat="identity", fill = col) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, color = "#556B2F") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "#FF4040", linewidth=1) +
    theme_minimal(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5), 
          title = element_text(size=18),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=17),
          axis.text.x = element_text(angle=27, hjust = 1, size=18) ) +
    labs(x = "",
         y = "OR for heart disease",
         title = paste("OR", var, "\n reference group:", ref_group))

    
      }

