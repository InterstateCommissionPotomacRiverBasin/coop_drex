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
date_today <- as.Date("1930-03-02") # later to be reactive
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
# Make the Potomac input data and flow time series dataframes
#--------------------------------------------------------------------------------
source("code/server/potomac_flows_init.R", local = TRUE)
# potomac.data.df - filled with all nat flow, trib flow data
# potomac.ts.df - initialized with first day of flows
#    - contains lfalls_obs, sen_outflow, jrr_outflow
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Main program - run the daily simulation
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#
# Need to get my bearings - at beginning of new time step:
#   - potomac.ts.df is updated thru yesterday
#   - need lagged jrr_outflow from 
for (sim_i in 2:sim_n + 1) { # start the simulation on the 2nd day
   date_sim <- as.Date(date_start + sim_i - 1)
   #
   #-----------------------------------------------------------------------------
   # First grab the demand forecasts
   #-----------------------------------------------------------------------------
   #  - right now just use values from input demand time series
   #  - eventually, would use CO-OP demand models
   #  - also will depend on restriction status, ie res levels
   #
   demands.fc.df <- forecasts_demands_func(date_sim, demands.daily.df)
   # This df has a length of 15, with cols date_time, demands_fc
   demand_fc_0_day <- demands.fc.df$demands_fc[1]
   demand_fc_9_day <- demands.fc.df$demands_fc[10]
   #
   #-----------------------------------------------------------------------------
   # Compute today's res releases assuming no water supply (ws) need
   #-----------------------------------------------------------------------------
   # last 2 function inputs are withdr_req & ws_rel_req
   #  - don't need flow fcs yet - assuming normal res wq releases
   #  - this will provide today's natural sen release to
   #     calculate lfalls_obs without ws releases.
   #
   sen.ts.df <- reservoir_ops_today_func(date_sim, sen, 
                                         sen.ts.df, 
                                         10, 0) 
   jrr.ts.df <- reservoir_ops_today_func(date_sim, jrr,
                                         jrr.ts.df,
                                         120, 0)
   #
   #-----------------------------------------------------------------------------
   # Do prelim update of potomac.ts.df assuming no ws releases
   #-----------------------------------------------------------------------------
   #
   # Collect data/fc's for today
   potomac_data_today <- subset(potomac.data.df, date_time == date_sim)
   lfalls_nat_today <- potomac_data_today$lfalls_nat[1]
   demand_today <- potomac_data_today$demands_total_unrestricted[1]
   # Collect necessary res outflows
   sen_outflow_today_no_ws <- last(sen.ts.df$outflow)
   # Maybe there's a better way to get today's 
   #   impact of jrr release on Little Falls
   jrr.ts.temp <- jrr.ts.df %>%
     dplyr::mutate(jrr_9day_lagged = lag(outflow, n = 9, 
                                     default = 129)) %>%
     dplyr::filter(date_time == date_sim)
   jrr_outflow_today_no_ws <- jrr.ts.temp$outflow[1]
   jrr_outflow_9day_lagged <- jrr.ts.temp$jrr_9day_lagged[1]
   # Now have enough to calculate prelim lfalls fc for today
   lfalls_obs_today_no_ws <- lfalls_nat_today - demand_today +
     sen_outflow_today_no_ws + jrr_outflow_9day_lagged
   # Finally create 1-row df and add to time series
   potomac.ts.today <- data.frame(date_time = date_sim, 
                                  lfalls_nat = lfalls_nat_today,
                                  demand = demand_today,
                                  lfalls_obs = lfalls_obs_today_no_ws,
                                  sen_outflow = sen_outflow_today_no_ws,
                                  jrr_outflow = jrr_outflow_today_no_ws)
   potomac.ts.df <- rbind(potomac.ts.df, potomac.ts.today)
   #
   #-----------------------------------------------------------------------------
   # Compute today's ws needs
   #-----------------------------------------------------------------------------
   # 
   lfalls_flowby <- 100 
   mos_0day <- 40 # margin of safety for Seneca release
   mos_9day <- 0 # margin of safety for N Br release
   #
   # Compute ws need today - for Seneca release
   need <- lfalls_flowby + mos_0day - lfalls_obs_today_no_ws
   ws_need_0day <- if_else(need > 0, need, 0, missing = 0)
   #
   # Compute ws need in 9 days - for N Br release
   # First fc flow at L Falls in 9 days
   lfalls_nat_9days <- 288*exp(0.0009*lfalls_nat_today)
   lfalls_nat_9days <- if_else(lfalls_nat_9days < lfalls_obs_today_no_ws,
                               lfalls_nat_9days, lfalls_obs_today_no_ws)
   #  - sen_outflow_today is ok since it assumes no ws releases
   #  - likewise for jrr_outflow_today, 
   demand_9days <- demand_today # may improve later!!!
   sen_outflow_9days <- sen_outflow_today_no_ws # may improve later!!!
   lfalls_obs_9days <- lfalls_nat_9days + sen_outflow_9days +
     jrr_outflow_today_no_ws - demand_9days
   # For fc of demand in 9 days can do better than demand_today
   #   but this will suffice for now
   need <- lfalls_flowby + mos_9day - lfalls_obs_9days
   ws_need_9day <- if_else(need > 0, need, 0, missing = 0)
   #
   #-----------------------------------------------------------------------------
   # Compute today's reservoir withdrawal & release requests
   #-----------------------------------------------------------------------------
   #   There are no withdrawals from Sen or JRR
   sen_withdr_req <- 0
   jrr_withdr_req <- 0
   #
      sen.ts.df <- reservoir_ops_today_func(date_sim, sen,
                                            sen.ts.df,
                                            sen_withdr_req,
                                            ws_need_0day)
      jrr.ts.df <- reservoir_ops_today_func(date_sim, jrr,
                                            jrr.ts.df,
                                            jrr_withdr_req,
                                            ws_need_9day)
   #
   #-----------------------------------------------------------------------------
   # Do final update of potomac.ts.df for today
   #-----------------------------------------------------------------------------
   # Calculate the reservoir releases, taking into account ws needs
      sen_outflow_today <- last(sen.ts.df$outflow)
      jrr_outflow_today <- last(jrr.ts.df$outflow)
   # Update today's lfalls fc, taking into account
   #   today's Seneca ws release
      lfalls_obs_today <- lfalls_obs_today_no_ws - 
        sen_outflow_today_no_ws + sen_outflow_today
   #
} # end of simulation loop
#
# *************************************************
# This is temporary: *************
graph_range <- date_today + 30
# Convert data to "long" format for graphing:
potomac.graph.df <- potomac.data.df %>%
  gather(key = "location", 
         value = "flow_mgd", -date_time) %>%
  dplyr::filter(date_time <= date_end)
# dplyr::filter(date_time <= date_today)
# temp.df <- data.frame(date_time = date_today + 9, 
#                       location = "test", 
#                       flow_mgd = 999,
#                       stringsAsFactors = FALSE)
# *************************************************

 
