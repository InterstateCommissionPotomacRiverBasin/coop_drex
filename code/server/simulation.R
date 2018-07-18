#------------------------------------------------------------------
#------------------------------------------------------------------
# Simulate CO-OP system operations 
# - this is the "main" code
#------------------------------------------------------------------
#------------------------------------------------------------------
#
#--------------------------------------------------------------------------------
# Define the simulation date range and "todays" date
#--------------------------------------------------------------------------------
# The simulation period is now defined in global.R but will be moved.
#    Might want to add debug code to 
#    verify that date_today is within the data date range.
# 
date_today <- as.Date("1930-07-15") # later to be reactive
sim_n <- as.numeric(as.POSIXct(date_today) - as.POSIXct(date_start),
                    units = "days")
#
#--------------------------------------------------------------------------------
# Make the reservoir objects and reservoir time series df's
#--------------------------------------------------------------------------------
source("code/server/reservoirs_make.R", local = TRUE) 
# sen.ts.df - initialized with first day of ops time series
# jrr.ts.df - initialized with first day of ops time series
#
#--------------------------------------------------------------------------------
# Make the Potomac River flow data and flow time series dataframes
#--------------------------------------------------------------------------------
source("code/server/potomac_flows_init.R", local = TRUE)
# potomac.data.df - filled with all nat flow, trib flow data
# potomac.ts.df - initialized with first day of flows
#
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Main program - run the daily simulation
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
for (i in 1:sim_n) {
   date_sim <- as.Date(date_start + i)
   #
   #-----------------------------------------------------------------------------
   # First grab the demand forecasts
   #-----------------------------------------------------------------------------
   #   - right now just use values from input demand time series
   #   - eventually, would use CO-OP demand models
   #   - also will depend on restriction status, ie reservoir levels
   #
   demands.fc.df <- forecasts_demands_func(date_sim, demands.daily.df)
   # demand_fc_0_day <- demands_fc[1]
   # demand_fc_9_day <- demands_fc[10]
   #
   #-----------------------------------------------------------------------------
   # Update Potomac flow ts with yesterday's obs flows & new fc's
   #-----------------------------------------------------------------------------
   #   - right now use empirical eq. for 9-day and lfalls obs for 0-day
   #        (Seneca release based on today like old PRRISM)
   #   - eventually use LFFS for 9-day and upstr gages for 1-day
   #
   potomac.ts.df <- forecasts_flows_func(date_sim, 
                                         potomac.ts.df,
                                         demands.fc.df,
                                 potomac.data.df)
   #
   #-----------------------------------------------------------------------------
   # Compute today's reservoir withdrawal & release requests
   #-----------------------------------------------------------------------------
   #   There are no withdrawals from Sen or JRR
   sen_withdr_req <- 0
   jrr_withdr_req <- 0
   #
   #-----------------------------------------------------------------------------
   # Compute today's reservoir releases and storage levels
   #-----------------------------------------------------------------------------
   # last 2 function inputs are withdr_req & rel_req
      sen.ts.df <- reservoir_ops_today_func(sen, sen.ts.df, 
                                            sen_withdr_req,
                                            20) 
      jrr.ts.df <- reservoir_ops_today_func(jrr, jrr.ts.df,
                                            jrr_withdr_req,
                                            300)
} # end of simulation loop
#
# *************************************************
# This is temporary: *************
graph_range <- date_today + 30
# Convert data to "long" format for graphing:
potomac.data.df <- potomac.data.df %>%
  gather(key = "location", 
         value = "flow_mgd", -date_time) %>%
  dplyr::filter(date_time <= date_end)
# dplyr::filter(date_time <= date_today)
# temp.df <- data.frame(date_time = date_today + 9, 
#                       location = "test", 
#                       flow_mgd = 999,
#                       stringsAsFactors = FALSE)
# *************************************************

 
