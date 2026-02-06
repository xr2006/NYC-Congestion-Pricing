rm (list=ls())    #clears environment
cat("/014")       #allows the use of Ctrl+L for clearing the console
# install.packages('plyr')
# install.packages('reshape2')
# install.packages('AER')
# install.packages(c("broom", "openxlsx"))  # if not installed
library(broom)
library(openxlsx)
library(plyr)
library(reshape2)
library(AER)
library(dplyr)

setwd("/Users/ryan/Documents/01.Research Projects/18.NYC Congestion Pricing")

# Input Data----
data_est = read.csv("2.Joint mode and destination modeling/(1226) choice_dataset_v2.csv")
source("2.Joint mode and destination modeling/(1226) utils.R")

county_vars <- c('County_09001', 'County_09005', 'County_09009',
                 'County_34003', 'County_34013', 'County_34017', 'County_34019',
                 'County_34021', 'County_34023', 'County_34025', 'County_34027',
                 'County_34029', 'County_34031', 'County_34035', 'County_34037',
                 'County_34039', 'County_36005', 'County_36027', 'County_36047',
                 'County_36059', 'County_36061.0', 'County_36061.1', 'County_36071', # Note that we need to use 36061.0 instead of 36061-0
                 'County_36079', 'County_36081', 'County_36085', 'County_36087',
                 'County_36103', 'County_36111', 'County_36119') # one county in PA is set as the reference level
county_rhs <- paste(county_vars, collapse = "+")


#----
# Create BLP-Style Instruments ----
# Create instruments based on SIMILAR markets (not same market)
# Group markets by origin region or destination region

data_est <- data_est %>%
  # IV based on average cost in OTHER markets with same ORIGIN
  group_by(origin_county, mode) %>%
  mutate(
    iv_cost_same_origin = (sum(cost_new * Trip_num) - cost_new * Trip_num) / 
      (sum(Trip_num) - Trip_num + 1)
  ) %>%
  ungroup() %>%
  
  # IV based on average cost in OTHER markets with same DESTINATION  
  group_by(destination_county, mode) %>%
  mutate(
    iv_cost_same_dest = (sum(cost_new * Trip_num) - cost_new * Trip_num) / 
      (sum(Trip_num) - Trip_num + 1)
  ) %>%
  ungroup() %>%
  
  # IV based on average cost for same MODE across all markets
  group_by(mode) %>%
  mutate(
    iv_cost_same_mode = (sum(cost_new * Trip_num) - cost_new * Trip_num) / 
      (sum(Trip_num) - Trip_num + 1)
  ) %>%
  ungroup()

# Create NYC interactions
data_est$iv_cost_same_origin_NYC <- data_est$iv_cost_same_origin * data_est$From_NYC
data_est$iv_cost_same_dest_NYC <- data_est$iv_cost_same_dest * data_est$From_NYC
data_est$iv_cost_same_mode_NYC <- data_est$iv_cost_same_mode * data_est$From_NYC


#----


# IPDL (final version) ----
endog_vars <- "cost_new + cost_new_NYC"
exog_vars <- paste(
  "auto_tt", "transit_at", "transit_et", "transit_wt", "transit_ivt", "transit_nt", "non_auto_tt",
  "constant_driving", "constant_transit", "constant_ondemand", "constant_biking", "constant_walking",
  "auto_tt_NYC", "transit_at_NYC", "transit_et_NYC", "transit_wt_NYC", "transit_ivt_NYC", "non_auto_tt_NYC",
  "log_s_s.county.", "log_s_s.mode.",
  county_rhs, "factor(market_id)",
  sep = " + "
)
# instruments <- paste(
#   "iv_cost_other_alt", "iv_auto_tt_other_alt", "iv_transit_ivt_other_alt",
#   "iv_nonauto_tt_other_alt", "iv_cost_NYC", "iv_auto_tt_NYC",
#   "iv_transit_ivt_NYC", "iv_nonauto_NYC",
#   sep = " + "
# )
instruments <- paste(
   "iv_cost_same_dest", "iv_cost_same_mode",
   "iv_cost_same_dest_NYC", "iv_cost_same_mode_NYC",
  sep = " + "
)

rhs1 <- paste(endog_vars, exog_vars, sep = " + ") # exclude intercept
rhs2 <- paste(instruments, exog_vars, sep = " + ")

form_string <- paste("log_s_s0 ~ ", rhs1, "|", rhs2)
iv_formula <- as.formula(form_string)
start_time <- Sys.time()
IPDL <- ivreg(iv_formula, data = data_est, weights=data_est$Trip_num)
summary_IPDL = summary(IPDL, diagnostics = TRUE)
end_time <- Sys.time()
end_time - start_time

calculate_vot(IPDL, "ALL")

save_model_results(IPDL, summary_IPDL, file = "2.Joint mode and destination modeling/Model Results 1226/IPDL_Full.xlsx", data=data_est)

