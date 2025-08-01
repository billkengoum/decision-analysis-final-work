## Load required packages
library(decisionSupport)
library(ggplot2)

library(readr)
getwd()

#Read the unified input table
input_data <- read.csv("C:/Users/willy/Desktop/COCAPIG/fsdfg/PC.csv")
input_data
# Convert to estimate object and draw one set of inputs into the environment
estimates <- as.estimate(input_data)
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (nm in colnames(x)) {
    assign(nm, as.numeric(x[1, nm]), envir = .GlobalEnv)
  }
}

make_variables(estimate_read_csv(paste("C:/Users/willy/Desktop/COCAPIG/fsdfg/PC.csv")))


# 2. Define a single model function that computes both scenarios# 

model_function <- function(x, varnames) {
  ### 2.1  cocoa cost in monoculture
  cocoa_bean_co1 <- vv(cocoa_bean_co, var_CV = var_CV, n = n_years)
  tran_cost_co1 <- vv(tran_cost_co, var_CV = var_CV, n = n_years)
  fer_cost_co1 <- vv(fertiliser_cost_co, var_CV = var_CV, n = n_years)
  lab_cost_co1 <- vv(labor_co, var_CV = var_CV, n = n_years)
  farm_man_co1 <- vv(farm_man_co, var_CV = var_CV, n = n_years)
  
  cocoa_co_tc <- cocoa_bean_co1+tran_cost_co1+fer_cost_co1+
              farm_man_co1+lab_cost_co1
  
    ### 2.1.1 risk  effects in cocoa production
  dry_year <- chance_event(
    chance       = dry_year_co_d,
    value_if     = Coco_Yield_co * (1 - dry_year_co_e),
    value_if_not = Coco_Yield_co,
    n            = n_years )
  
  heavt_rain <- chance_event(
    chance       = heavy_rain_co_d,
    value_if     = Coco_Yield_co * (1 - heavy_rain_co_e),
    value_if_not = Coco_Yield_co,
    n            = n_years )
  
  draugh_year <- chance_event(
    chance       = draught_co_d,
    value_if     = Coco_Yield_co * (1 - draught_co_e),
    value_if_not = Coco_Yield_co,
    n            = n_years )
  
  pest_year <- chance_event(
    chance       = pest_co_d,
    value_if     = Coco_Yield_co * (1 - pest_co_e),
    value_if_not = Coco_Yield_co,
    n            = n_years )
  
  cocoa_total_risk <- dry_year + heavt_rain + draugh_year + pest_year
  
  
  ### 2.1.2 cocoa revenue after loss
  cocoa_revenue_with_risks <- vv(
    var_mean = (Coco_Yield_co * Coco_Price_co) - (cocoa_total_risk),
    var_CV   = var_CV,
    n        = n_years )
  
  cocoa_revenue_No_risk <-  vv(
    var_mean = (Coco_Yield_co * Coco_Price_co),
    var_CV   = var_CV,
    n        = n_years )
 
  ### cocoa Cashflow & NPV
  cocoa_Cashflow <- cocoa_revenue_with_risks - cocoa_co_tc
  CumCashflow_cocoa <- cumsum(cocoa_Cashflow)
  NPV_cocoa         <- discount(cocoa_Cashflow,
                                      discount_rate = discount_rate,
                                      calculate_NPV = TRUE)
  
  # 2.2 integrated production
  
  ### 2.2.1 cocoa and pig initial cost
  farm_inst_cp1 <- vv(farm_inst_cp, var_CV = var_CV, n = n_years)
  mat_eq_cp1 <- vv(mat_eq_cp, var_CV = var_CV, n = n_years)
  water_prov_cp1 <- vv(water_prov_cp, var_CV = var_CV, n = n_years)
 
  cocoa_cp_tci <- farm_inst_cp1 + mat_eq_cp1 + water_prov_cp1
  
  ### 2.2.2 cocoa and pig Operational costs 
  biop_cp1 <- vv(biop_cp, var_CV = var_CV, n = n_years)
  pig_feed_cp1 <- vv(pig_feed_cp, var_CV = var_CV, n = n_years)
  veter_cost_cp1 <- vv(veter_cost_cp, var_CV = var_CV, n = n_years)
  shared_lab_cp1 <- vv(shared_lab_cp, var_CV = var_CV, n = n_years)
  tran_cost_cp1 <- vv(tran_cost_cp, var_CV = var_CV, n = n_years) 
  cocoa_bean_cp1 <- vv(cocoa_bean_cp, var_CV = var_CV, n = n_years)
  husk_cp1 <- vv(husk_cp, var_CV = var_CV, n = n_years)
  biof_cp1 <- vv(biof_cp, var_CV = var_CV, n = n_years)
  
  cocoa_cp_tco <- biop_cp1 + pig_feed_cp1 + veter_cost_cp1 + shared_lab_cp1 +
                   tran_cost_cp1 + cocoa_bean_cp1 + husk_cp1 + biof_cp1 
  
  cocoa_and_pig_total_cost_in_year_1 <- cocoa_cp_tci + cocoa_cp_tco

  ##  2.2.3 Risk adjustment in integrated cocoa and pig
  
  theft_year <- chance_event(
    chance       = theft_cp_d,
    value_if     = Pig_Yield_cp * (1 - theft_cp_e),
    value_if_not = Pig_Yield_cp,
    n            = n_years )
  
  pest_year <- chance_event(
    chance      = pest_cp_d,
    value_if    = Pig_Yield_cp * (1 - pest_cp_e),
    n           = n_years)
  
  dry_year <- chance_event(
    chance      = dry_year_cp_d,
    value_if    = Coco_Yield_cp * (1- dry_year_cp_e) ,
    n           = n_years)
  
  heavy_rain_year <-  chance_event(
    chance      = heavy_rain_cp_d,
    value_if    = Coco_Yield_cp * (1- heavy_rain_cp_e) ,
    n           = n_years)
  
  draugh_year <- chance_event(
    chance      = dry_year_cp_d,
    value_if    = Coco_Yield_cp * (1- dry_year_cp_e) ,
    n           = n_years)
  
  Integrated_cp_total_risks <-  theft_year + pest_year + dry_year + heavy_rain_year + draugh_year
  
    ### 2.3integrated cocoa and pig revenue
  integrated_revenue_No_risks <- vv( var_mean = (Pig_Yield_cp * pig_Price_cp) +
                     (Coco_Yield_cp * Coco_Price_cp),
                     var_CV   = var_CV,
                     n        = n_years )
  
  integrated_revenue_with_risks <- vv( var_mean = ((Pig_Yield_cp * pig_Price_cp) +
                                  (Coco_Yield_cp * Coco_Price_cp)) - Integrated_cp_total_risks,
                                  var_CV   = var_CV,
                                  n        = n_years )
  
    ### Integrated pig and cocoa Cashflow & NPV
  Cashflow_integrated_cocoa_and_pig    <- vv(integrated_revenue_with_risks - (cocoa_cp_tci), 
                                         var_CV   = var_CV,
                                          n        = n_years ) 
    CumCashflow_integrated_cocoa_and_pig <- cumsum(Cashflow_integrated_cocoa_and_pig)
  NPV_Integrated_cocoa_and_pig         <- discount(Cashflow_integrated_cocoa_and_pig,
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  ### Decision difference: Profit, cashflow, cumulative
  Cashflow_Decision    <- Cashflow_integrated_cocoa_and_pig - cocoa_Cashflow
  CumCashflow_Decision <- cumsum(Cashflow_Decision)
  NPV_Decision         <- NPV_Integrated_cocoa_and_pig - NPV_cocoa
  
  
  ## Return all outputs
  return(list(
    NPV_cocoa        = NPV_cocoa,
    NPV_Integrated_cocoa_and_pig      = NPV_Integrated_cocoa_and_pig,
    NPV_Decision           = NPV_Decision,
    cocoa_Cashflow   = cocoa_Cashflow,
    Cashflow_integrated_cocoa_and_pig = Cashflow_integrated_cocoa_and_pig,
    Cashflow_Decision      = Cashflow_Decision,
    CumCashflow_cocoa   = CumCashflow_cocoa,
    CumCashflow_integrated_cocoa_and_pig = CumCashflow_integrated_cocoa_and_pig,
    CumCashflow_Decision      = CumCashflow_Decision
  ))
}


# Run a single Monte Carlo simulation for both scenarios
combined_simulation <- mcSimulation(
  estimate = estimate_read_csv("PC.csv"),
  model_function    = model_function,
  numberOfModelRuns = 10000,
  functionSyntax    = "plainNames" )

combined_simulation

# Plot both NPV distributions together
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("NPV_cocoa","NPV_Integrated_cocoa_and_pig"),
  method              = "hist_simple_overlay", base_size = 7,
  x_axis_name = "Outcome distribution (Fcfa/ha)")

plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("NPV_cocoa","NPV_Integrated_cocoa_and_pig"),
  method              = "smooth_simple_overlay", base_size = 7,
  x_axis_name = "Outcome distribution (Fcfa/ha)")
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = "NPV_cocoa",
  method              = "boxplot_density", base_size = 7,
  x_axis_name = "Outcome distribution (Fcfa/ha)")
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = "NPV_Integrated_cocoa_and_pig",
  method              = "boxplot_density", base_size = 7,
  x_axis_name = "Outcome distribution (Fcfa/ha)")

# Compound figure of NPV and cashflow for cocoa
compound_figure(
  # mcSimulation_object = combined_simulation,
  model = model_function,
  input_table = input_data,
  decision_var_name = "NPV_cocoa",
  cashflow_var_name = "cocoa_Cashflow",
  model_runs = 1000, 
  distribution_method = 'smooth_simple_overlay')

# Combine inputs and both NPVs into one data frame
df_evpi <- data.frame(
           combined_simulation$x,
           NPV_cocoa   = combined_simulation$y[, "NPV_cocoa"],
           NPV_Integrated_cocoa_and_pig = combined_simulation$y[, "NPV_Integrated_cocoa_and_pig"] )



# Calculate EVPI for all inputs, starting at NPV_cocoa
EVPI_results <- multi_EVPI(
                mc            = df_evpi,
                first_out_var = "NPV_cocoa" )

# Inspect the EVPI table
print(EVPI_results)

# Plot EVPI for both scenarios side by side
plot_evpi(
         EVPIresults   = EVPI_results,
         decision_vars = c("NPV_cocoa", "NPV_Integrated_cocoa_and_pig"))


#### Cashflow (Annual Profit) Comparison for Both Scenarios ####

# Plot the annual profit (“cashflow”) time series for both scenarios
plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("cocoa_Cashflow", "Cashflow_integrated_cocoa_and_pig"),
  x_axis_name         = "Year",
  y_axis_name         = "Annual Profit (Fcfa/ha)",
  color_25_75         = "blue",
  color_5_95          = "lightblue",
  color_median        = "darkblue",
  facet_labels        = c("cocoa", "integrated_cocoa_and_pig"))

plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("CumCashflow_cocoa", "CumCashflow_integrated_cocoa_and_pig"),
  x_axis_name         = "Year",
  y_axis_name         = "Annual Profit (Fcfa/ha)",
  color_25_75         = "blue",
  color_5_95          = "lightblue",
  color_median        = "darkblue",
  facet_labels        = c("cocoa", "integrated_cocoa_and_pig"))


# Identify the exact column names for Year-n_years
cocoa_cols <- grep("^cocoa_Cashflow\\.", names(combined_simulation$y),
             value = TRUE)
integrated_cocoa_and_pig_cols  <- grep("^Cashflow_integrated_cocoa_and_pig\\.", names(combined_simulation$y),
             value = TRUE)

final_year_cocoa_cols <- cocoa_cols[grepl(paste0("\\.", n_years, "$"), cocoa_cols)][1]
final_year_integrated_cocoa_and_pig_cols  <- integrated_cocoa_and_pig_cols[ grepl(paste0("\\.", n_years, "$"), integrated_cocoa_and_pig_cols)][1]

# Extract those columns as numeric vectors
cocoa_final <- combined_simulation$y[[final_year_cocoa_cols]]
integrated_cococa_and_pig_final <- combined_simulation$y[[final_year_integrated_cocoa_and_pig_cols ]]

# Compute the 30th and 70th percentiles for Year-n_years
cocoa_q <- quantile(cocoa_final, probs = c(0.3, 0.7))
integrated_cocoa_and_pig_q  <- quantile(integrated_cococa_and_pig_final,  probs = c(0.3, 0.7))


# Print the results
cat("cocoa year", n_years, "Profit 30th & 70th percentiles:\n")
    print(cocoa_q)
cat("\nIntercropping Year", n_years, "Profit 30th & 70th percentiles:\n")
    print(int_q)


#### Additional Result Graphs ####

# Side-by-side boxplots of NPV
NPV_df <- data.frame(
          cocoa   = combined_simulation$y[, "NPV_cocoa"],
          integrated_pig_and_cocoa = combined_simulation$y[, "NPV_Integrated_cocoa_and_pig"])

NPV_long <- pivot_longer(
            NPV_df,
            cols      = everything(),
            names_to  = "Scenario",
            values_to = "NPV")

ggplot(NPV_long, aes(x = Scenario, y = NPV, fill = Scenario)) +
       geom_boxplot(alpha = 0.6) +
       theme_minimal(base_size = 14) +
       labs(
       title = "NPV Distribution: cocoa vs. integrated_pig_and_cocoa",
       x     = NULL,
       y     = "Net Present Value (Fcfa/ha)"
       ) +
       scale_fill_manual(values = c("grey40", "forestgreen")) +
       theme(legend.position = "none")


# Grab columns
cocoa_cols <- grep("^CumCashflow_cocoa",   names(combined_simulation$y),
                  value = TRUE)
integrated_pig_and_cocoa_cols  <- grep("^CumCashflow_integrated_cocoa_and_pig", names(combined_simulation$y), 
                  value = TRUE)
dec_cols  <- grep("^CumCashflow_Decision",      names(combined_simulation$y), 
                  value = TRUE)

# Sanity check
if (!(length(cocoa_cols)==n_years && length(integrated_pig_and_cocoa_cols)==n_years && 
      length(dec_cols)==n_years)) {
      stop(sprintf("Expect %d years, got %d mono, %d int, %d dec",
      n_years, length(mono_cols), length(integrated_pig_and_cocoa_cols), length(dec_cols)))}

# Compute medians per year
cocoa_med_cum <- apply(combined_simulation$y[, cocoa_cols], 2, median)
integrated_pig_and_cocoa_med_cum  <- apply(combined_simulation$y[, integrated_pig_and_cocoa_cols],  2, median)
dec_med_cum  <- apply(combined_simulation$y[, dec_cols],  2, median)


# Build long data.frame for just cocoa & integrated_cocoa_and_pig
year_seq <- seq_len(n_years)
cum_df <- data.frame(
          Year       = rep(year_seq, times = 2),
          Cumulative = c(cocoa_med_cum, integrated_pig_and_cocoa_med_cum),
          Scenario   = rep(c("cocoa","integrated_pig_and_cocoa"), each = n_years))

# Plot it
ggplot(cum_df, aes(x = Year, y = Cumulative, color = Scenario)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      theme_minimal(base_size = 14) +
      labs(
      title = "Median Cumulative Profit Over Time",
      x     = "Year",
      y     = "Cumulative Profit (USD/ha)"
      ) +
      scale_color_manual(values = c("grey40","forestgreen"))


