# ============================================================================
# utils.R
# Utility functions for model analysis
# ============================================================================

calculate_vot <- function(model, model_name) {
  coefs <- coef(model)
  
  # Extract coefficients
  cost_new <- coefs["cost_new"]
  cost_new_NYC <- coefs["cost_new_NYC"]
  auto_tt <- coefs["auto_tt"]
  auto_tt_NYC <- coefs["auto_tt_NYC"]
  transit_ivt <- coefs["transit_ivt"]
  transit_ivt_NYC <- coefs["transit_ivt_NYC"]
  transit_at <- coefs["transit_at"]
  transit_at_NYC <- coefs["transit_at_NYC"]
  transit_et <- coefs["transit_et"]
  transit_et_NYC <- coefs["transit_et_NYC"]
  transit_wt <- coefs["transit_wt"]
  transit_wt_NYC <- coefs["transit_wt_NYC"]
  nonauto_tt <- coefs["non_auto_tt"]
  nonauto_tt_NYC <- coefs["non_auto_tt_NYC"]
  
  # Total cost for NYC
  cost_total_NYC <- cost_new + cost_new_NYC
  
  # Calculate VOT = (time coef / cost coef) * 60
  # For Other (non-NYC)
  vot_auto_other <- (auto_tt / cost_new) * 60
  vot_transit_ivt_other <- (transit_ivt / cost_new) * 60
  vot_transit_at_other <- (transit_at / cost_new) * 60
  vot_transit_et_other <- (transit_et / cost_new) * 60
  vot_transit_wt_other <- (transit_wt / cost_new) * 60
  vot_nonauto_other <- (nonauto_tt / cost_new) * 60
  
  # For NYC
  vot_auto_NYC <- ((auto_tt + auto_tt_NYC) / cost_total_NYC) * 60
  vot_transit_ivt_NYC <- ((transit_ivt + transit_ivt_NYC) / cost_total_NYC) * 60
  vot_transit_at_NYC <- ((transit_at + transit_at_NYC) / cost_total_NYC) * 60
  vot_transit_et_NYC <- ((transit_et + transit_et_NYC) / cost_total_NYC) * 60
  vot_transit_wt_NYC <- ((transit_wt + transit_wt_NYC) / cost_total_NYC) * 60
  vot_nonauto_NYC <- ((nonauto_tt + nonauto_tt_NYC) / cost_total_NYC) * 60
  
  # Print results
  cat("\n", paste(rep("=", 65), collapse=""), "\n")
  cat("VALUE OF TIME -", model_name, "\n")
  cat(paste(rep("=", 65), collapse=""), "\n\n")
  
  cat(sprintf("%-20s %15s %15s\n", "Variable", "NYC", "Other"))
  cat(paste(rep("-", 55), collapse=""), "\n")
  cat(sprintf("%-20s $%14.2f $%14.2f\n", "VOT (auto_tt)", vot_auto_NYC, vot_auto_other))
  cat(sprintf("%-20s $%14.2f $%14.2f\n", "VOT (transit_ivt)", vot_transit_ivt_NYC, vot_transit_ivt_other))
  cat(sprintf("%-20s $%14.2f $%14.2f\n", "VOT (transit_wt)", vot_transit_wt_NYC, vot_transit_wt_other))
  cat(sprintf("%-20s $%14.2f $%14.2f\n", "VOT (transit_at)", vot_transit_at_NYC, vot_transit_at_other))
  cat(sprintf("%-20s $%14.2f $%14.2f\n", "VOT (transit_et)", vot_transit_et_NYC, vot_transit_et_other))
  cat(sprintf("%-20s $%14.2f $%14.2f\n", "VOT (nonauto_tt)", vot_nonauto_NYC, vot_nonauto_other))
}  


save_model_results <- function(model, summary_model, model_name = NULL, file = NULL, data = NULL, sig_level = 0.05) {
  # ---- Packages ----
  if (!requireNamespace("broom",     quietly = TRUE) ||
      !requireNamespace("openxlsx",  quietly = TRUE) ||
      !requireNamespace("dplyr",     quietly = TRUE) ||
      !requireNamespace("tidyr",     quietly = TRUE)) {
    stop("Please install packages: broom, openxlsx, dplyr, tidyr")
  }
  library(broom); library(dplyr); library(tidyr); library(openxlsx)
  
  # ---- Default filename and model name from object name ----
  obj_name <- deparse(substitute(model))
  if (is.null(file)) {
    file <- paste0(obj_name, "_results.xlsx")
  }
  if (is.null(model_name)) {
    model_name <- obj_name
  }
  
  # ---- Tables ----
  coef_tbl <- tidy(model, conf.int = TRUE)
  fit_tbl  <- glance(model)
  
  # ---- Diagnostics table ----
  if (!is.null(summary_model$diagnostics)) {
    diag_df <- as.data.frame(summary_model$diagnostics)
    diag_df$Test <- rownames(diag_df)
    diag_df <- diag_df %>% select(Test, everything())
    rownames(diag_df) <- NULL
  } else {
    diag_df <- data.frame(Test = "No diagnostics available")
  }
  
  # ---- Performance metrics ----
  aug <- augment(model)
  perf_tbl <- aug %>% 
    summarise(
      RMSE = sqrt(mean(.resid^2, na.rm = TRUE)),
      MAE  = mean(abs(.resid), na.rm = TRUE)
    ) %>% 
    pivot_longer(everything(),
                 names_to = "metric",
                 values_to = "value")
  
  # ---- Calculate VOT ----
  coefs <- coef(model)
  
  # Helper function to check significance
  is_significant <- function(var_name) {
    row <- coef_tbl %>% filter(term == var_name)
    if (nrow(row) == 1 && !is.na(row$p.value)) {
      return(row$p.value < sig_level)
    }
    return(FALSE)
  }
  
  # Extract coefficients
  cost_new <- coefs["cost_new"]
  cost_new_NYC <- coefs["cost_new_NYC"]
  auto_tt <- coefs["auto_tt"]
  auto_tt_NYC <- coefs["auto_tt_NYC"]
  transit_ivt <- coefs["transit_ivt"]
  transit_ivt_NYC <- coefs["transit_ivt_NYC"]
  transit_at <- coefs["transit_at"]
  transit_at_NYC <- coefs["transit_at_NYC"]
  transit_et <- coefs["transit_et"]
  transit_et_NYC <- coefs["transit_et_NYC"]
  transit_wt <- coefs["transit_wt"]
  transit_wt_NYC <- coefs["transit_wt_NYC"]
  nonauto_tt <- coefs["non_auto_tt"]
  nonauto_tt_NYC <- coefs["non_auto_tt_NYC"]
  
  # Total cost for NYC
  cost_total_NYC <- cost_new + cost_new_NYC
  
  # Calculate VOT = (time coef / cost coef) * 60
  # For Other (non-NYC)
  vot_auto_other <- (auto_tt / cost_new) * 60
  vot_transit_ivt_other <- (transit_ivt / cost_new) * 60
  vot_transit_at_other <- (transit_at / cost_new) * 60
  vot_transit_et_other <- (transit_et / cost_new) * 60
  vot_transit_wt_other <- (transit_wt / cost_new) * 60
  vot_nonauto_other <- (nonauto_tt / cost_new) * 60
  
  # For NYC
  vot_auto_NYC <- ((auto_tt + auto_tt_NYC) / cost_total_NYC) * 60
  vot_transit_ivt_NYC <- ((transit_ivt + transit_ivt_NYC) / cost_total_NYC) * 60
  vot_transit_at_NYC <- ((transit_at + transit_at_NYC) / cost_total_NYC) * 60
  vot_transit_et_NYC <- ((transit_et + transit_et_NYC) / cost_total_NYC) * 60
  vot_transit_wt_NYC <- ((transit_wt + transit_wt_NYC) / cost_total_NYC) * 60
  vot_nonauto_NYC <- ((nonauto_tt + nonauto_tt_NYC) / cost_total_NYC) * 60
  
  # Helper function to format VOT (show "$--" if not significant)
  format_vot <- function(vot_value, time_var, time_var_NYC = NULL, is_nyc = FALSE) {
    if (is_nyc) {
      if (is_significant(time_var)) {
        return(paste0("$", round(vot_value, 2)))
      } else {
        return("$--")
      }
    } else {
      if (is_significant(time_var)) {
        return(paste0("$", round(vot_value, 2)))
      } else {
        return("$--")
      }
    }
  }
  
  # ---- Prediction and R-squared calculation ----
  pred_r2 <- NA
  pred_tbl <- NULL
  
  if (!is.null(data)) {
    # Get predicted log_s_s0
    pred_log_s_s0 <- predict(model, newdata = data)
    
    # Create prediction dataframe
    pred_tbl <- data %>%
      select(market_id, origin_county, Pop_group, Trip_purpose, Time_period,
             destination_county, origin_region, destination_region, mode,
             Trip_num, log_s_s0) %>%
      mutate(
        pred_log_s_s0 = pred_log_s_s0
      )
    
    # Calculate predicted share and Trip_num
    # log_s_s0 = log(share / share_0)
    # => share / share_0 = exp(log_s_s0)
    # => share = share_0 * exp(log_s_s0)
    # For each market: sum(share) + share_0 = 1
    # => share_0 + share_0 * sum(exp(log_s_s0)) = 1
    # => share_0 * (1 + sum(exp(log_s_s0))) = 1
    # => share_0 = 1 / (1 + sum(exp(log_s_s0)))
    
    pred_tbl <- pred_tbl %>%
      group_by(market_id) %>%
      mutate(
        # Calculate exp(pred_log_s_s0) for each alternative
        exp_pred = exp(pred_log_s_s0),
        # Sum of exp(pred_log_s_s0) within market
        sum_exp_pred = sum(exp_pred),
        # Calculate share_0 = 1 / (1 + sum(exp(pred_log_s_s0)))
        pred_share_0 = 1 / (1 + sum_exp_pred),
        # Calculate predicted share = share_0 * exp(pred_log_s_s0)
        pred_share = pred_share_0 * exp_pred,
        # Get total trips in market (All_trips = Trip_num / share)
        # First calculate actual share from actual log_s_s0
        exp_actual = exp(log_s_s0),
        sum_exp_actual = sum(exp_actual),
        actual_share_0 = 1 / (1 + sum_exp_actual),
        actual_share = actual_share_0 * exp_actual,
        # All_trips = Trip_num / actual_share (should be same for all in market)
        All_trips = sum(Trip_num) / sum(actual_share),
        # Predicted Trip_num = pred_share * All_trips
        pred_Trip_num = pred_share * All_trips,
        # Market total trips for null model
        market_total_trips = sum(Trip_num),
        n_alternatives = n()
      ) %>%
      ungroup()
    
    # Null model: predicted Trip_num = market_total_trips / n_alternatives (equal share)
    pred_tbl <- pred_tbl %>%
      mutate(
        null_pred_Trip_num = market_total_trips / n_alternatives
      )
    
    # Calculate R-squared for estimated model vs null model
    # R^2 = 1 - SS_res / SS_tot
    # SS_res = sum((actual - predicted)^2)
    # SS_tot = sum((actual - mean)^2)
    
    actual_trips <- pred_tbl$Trip_num
    pred_trips <- pred_tbl$pred_Trip_num
    null_pred_trips <- pred_tbl$null_pred_Trip_num
    
    # R-squared for estimated model
    ss_res_model <- sum((actual_trips - pred_trips)^2, na.rm = TRUE)
    ss_tot <- sum((actual_trips - mean(actual_trips, na.rm = TRUE))^2, na.rm = TRUE)
    pred_r2 <- 1 - ss_res_model / ss_tot
    
    # R-squared for null model
    ss_res_null <- sum((actual_trips - null_pred_trips)^2, na.rm = TRUE)
    null_r2 <- 1 - ss_res_null / ss_tot
    
    # Clean up prediction table for output (select key columns)
    pred_tbl_output <- pred_tbl %>%
      select(
        market_id, origin_county, Pop_group, Trip_purpose, Time_period,
        destination_county, origin_region, destination_region, mode,
        Trip_num, log_s_s0, pred_log_s_s0,
        actual_share, pred_share, pred_Trip_num, null_pred_Trip_num
      )
    
    # Add R-squared to performance metrics
    perf_tbl <- bind_rows(
      perf_tbl,
      data.frame(metric = "R2_Trip_num_Model", value = pred_r2),
      data.frame(metric = "R2_Trip_num_Null", value = null_r2)
    )
    
    # Add to fit table
    fit_tbl$R2_Trip_num_Model <- pred_r2
    fit_tbl$R2_Trip_num_Null <- null_r2
  }
  
  # ---- Create Summary Row ----
  n_obs <- nobs(model)
  n_trips <- if (!is.null(data)) sum(data$Trip_num, na.rm = TRUE) else NA
  adj_r2 <- summary_model$adj.r.squared
  
  summary_row <- data.frame(
    Model = model_name,
    Num.Obs = n_obs,
    Num.Trips = n_trips,
    Adj.R.Square = round(adj_r2, 4),
    R2_Trip_num_Model = if (!is.na(pred_r2)) round(pred_r2, 4) else NA,
    VOT_auto_tt_NYC = format_vot(vot_auto_NYC, "auto_tt", "auto_tt_NYC", TRUE),
    VOT_auto_tt_Other = format_vot(vot_auto_other, "auto_tt"),
    VOT_transit_ivt_NYC = format_vot(vot_transit_ivt_NYC, "transit_ivt", "transit_ivt_NYC", TRUE),
    VOT_transit_ivt_Other = format_vot(vot_transit_ivt_other, "transit_ivt"),
    VOT_transit_wt_NYC = format_vot(vot_transit_wt_NYC, "transit_wt", "transit_wt_NYC", TRUE),
    VOT_transit_wt_Other = format_vot(vot_transit_wt_other, "transit_wt"),
    VOT_transit_at_NYC = format_vot(vot_transit_at_NYC, "transit_at", "transit_at_NYC", TRUE),
    VOT_transit_at_Other = format_vot(vot_transit_at_other, "transit_at"),
    VOT_transit_et_NYC = format_vot(vot_transit_et_NYC, "transit_et", "transit_et_NYC", TRUE),
    VOT_transit_et_Other = format_vot(vot_transit_et_other, "transit_et"),
    VOT_nonauto_tt_NYC = format_vot(vot_nonauto_NYC, "non_auto_tt", "non_auto_tt_NYC", TRUE),
    VOT_nonauto_tt_Other = format_vot(vot_nonauto_other, "non_auto_tt")
  )
  
  # ---- VOT detailed table (with significance indicator) ----
  vot_tbl <- data.frame(
    Variable = c("VOT (auto_tt)", "VOT (transit_ivt)", "VOT (transit_wt)", 
                 "VOT (transit_at)", "VOT (transit_et)", "VOT (nonauto_tt)"),
    NYC = c(
      format_vot(vot_auto_NYC, "auto_tt", "auto_tt_NYC", TRUE),
      format_vot(vot_transit_ivt_NYC, "transit_ivt", "transit_ivt_NYC", TRUE),
      format_vot(vot_transit_wt_NYC, "transit_wt", "transit_wt_NYC", TRUE),
      format_vot(vot_transit_at_NYC, "transit_at", "transit_at_NYC", TRUE),
      format_vot(vot_transit_et_NYC, "transit_et", "transit_et_NYC", TRUE),
      format_vot(vot_nonauto_NYC, "non_auto_tt", "non_auto_tt_NYC", TRUE)
    ),
    Other = c(
      format_vot(vot_auto_other, "auto_tt"),
      format_vot(vot_transit_ivt_other, "transit_ivt"),
      format_vot(vot_transit_wt_other, "transit_wt"),
      format_vot(vot_transit_at_other, "transit_at"),
      format_vot(vot_transit_et_other, "transit_et"),
      format_vot(vot_nonauto_other, "non_auto_tt")
    )
  )
  
  # ---- Write workbook ----
  wb <- createWorkbook()
  
  addWorksheet(wb, "Summary");      writeData(wb, "Summary", summary_row)
  addWorksheet(wb, "Coefficients"); writeData(wb, "Coefficients", coef_tbl)
  addWorksheet(wb, "Fit");          writeData(wb, "Fit", fit_tbl)
  addWorksheet(wb, "Diagnostics");  writeData(wb, "Diagnostics", diag_df)
  addWorksheet(wb, "VOT");          writeData(wb, "VOT", vot_tbl)
  addWorksheet(wb, "Metrics");      writeData(wb, "Metrics", perf_tbl)
  
  # Add Prediction sheet if data was provided
  if (!is.null(pred_tbl_output)) {
    addWorksheet(wb, "Prediction")
    writeData(wb, "Prediction", pred_tbl_output)
  }
  
  saveWorkbook(wb, file, overwrite = TRUE)
  message("Model results saved to: ", normalizePath(file))
  
  invisible(list(
    file = file,
    summary_row = summary_row,
    vot = vot_tbl,
    diagnostics = diag_df,
    prediction = if (exists("pred_tbl_output")) pred_tbl_output else NULL,
    R2_Trip_num_Model = pred_r2,
    R2_Trip_num_Null = if (exists("null_r2")) null_r2 else NA
  ))
}