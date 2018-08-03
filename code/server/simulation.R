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
# date_start and date_end are for now defined in global.R.
# potomac.data.df is a placeholder for the various data sources
#   - it's loaded with data from date_start to date_end by potomac_flows_init.R.
# 
date_today <- as.Date("1930-07-15") # later to be reactive
sim_n <- as.numeric(as.POSIXct(date_today) - as.POSIXct(date_start),
                    units = "days")
# temp for QAing:
# sim_n <- 2
#
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Main program - run the daily simulation
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#
for (sim_i in 2:sim_n + 1) { # start the simulation on the 2nd day
  date_sim <- as.Date(date_start + sim_i - 1)
  #
  #-----------------------------------------------------------------------------
  # 0. First grab the demand forecasts
  #-----------------------------------------------------------------------------
  #  - right now just use values from input demand time series
  #  - eventually, would use CO-OP demand models
  #  - also will depend on restriction status, ie res levels
  #
  demands.fc.df <- forecasts_demands_func(date_sim, demands.daily.df)
  # This df has a length of 15, with cols date_time, demands_fc
  #
  #-----------------------------------------------------------------------------
  # 1. Compute today's res releases assuming no water supply (ws) need
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
  # Grab some results for use as input in next step
  sen_outflow_today_no_ws <- last(sen.ts.df$outflow)
  jrr_outflow_today_no_ws <- last(jrr.ts.df$outflow)
   #
   #-----------------------------------------------------------------------------
   # 2. Do prelim update of flows in potomac.ts.df 
   #    - this adds fc's of today's flows
   #    - later intend to also add future flows - ie 15-day fcs
   #    - in this step, assuming no ws releases
   #-----------------------------------------------------------------------------
   #
   potomac.ts.df <- forecasts_flows_func(date_sim,
                                        demands.fc.df,
                                        sen_outflow_today_no_ws,
                                        jrr_outflow_today_no_ws,
                                        potomac.ts.df)
  # Grab some results for use as input in next step
  lfalls_obs_today_no_ws <- last(potomac.ts.df$lfalls_obs)
  lfalls_obs_fc9_no_ws <- last(potomac.ts.df$lfalls_obs_fc9)

   #-----------------------------------------------------------------------------
   # 3. Compute today's ws needs
   #-----------------------------------------------------------------------------
   # 
   # Later these values will be read from a parameter file
   #    in /input/parameters/
   lfalls_flowby <- 100 
   mos_0day <- 40 # margin of safety for Seneca release
   mos_9day <- 0 # margin of safety for N Br release
   #
   # Compute ws need today - for Seneca release
   need <- lfalls_flowby + mos_0day - lfalls_obs_today_no_ws
   ws_need_0day <- if_else(need > 0, need, 0, missing = 0)
   #
   # Compute ws need in 9 days - for N Br release
   need <- lfalls_flowby + mos_9day - lfalls_obs_fc9_no_ws
   ws_need_9day <- if_else(need > 0, need, 0, missing = 0)
   #
   #-----------------------------------------------------------------------------
   # 4. Compute today's reservoir releases, taking into account ws needs
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
   # Grab some results for use as input in next step
      sen_outflow_today <- last(sen.ts.df$outflow)
      jrr_outflow_today <- last(jrr.ts.df$outflow)
   #
   #-----------------------------------------------------------------------------
   # 5. Do final update of flows in potomac.ts.df
   #   -  taking into account possible changes in res releases
   #-----------------------------------------------------------------------------
   # On the one hand, there are repetitive aspects 
   #   to use of the same function twice (here and step 2).
   # On the other hand, it seems more consistent 
   #   to use just one function to update flows.
      potomac.ts.df <- forecasts_flows_func(date_sim,
                                           demands.fc.df,
                                           sen_outflow_today,
                                           jrr_outflow_today,
                                           potomac.ts.df)
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

 
