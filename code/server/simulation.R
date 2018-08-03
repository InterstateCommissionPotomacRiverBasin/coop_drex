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
# We want to simulate up to date_today, and see things graphed
#   up thru date_today + (not yet implemented) some forecasts (fcs) 
#   up thru some period - maybe 15 days out into the future?
date_today <- as.Date("1930-02-15") # later to be reactive
sim_n <- as.numeric(as.POSIXct(date_today) - as.POSIXct(date_start),
                    units = "days")
# 
mos_0day <- 40 # margin of safety for Seneca release
mos_9day <- 0 # margin of safety for N Br release
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
  sen.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                        res = sen, 
                                        res.ts.df = sen.ts.df, 
                                        withdr_req = 0,
                                        ws_rel_req = 0) 
  jrr.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                        res = jrr,
                                        res.ts.df = jrr.ts.df,
                                        withdr_req = 0,
                                        ws_rel_req = 0)
   #
   #-----------------------------------------------------------------------------
   # 2. Do prelim update of flows in potomac.ts.df 
   #    - this adds fc's of today's flows
   #    - later intend to also add future flows - ie 15-day fcs
   #    - in this step, assuming no ws releases
   #-----------------------------------------------------------------------------
   #
   potomac.ts.df <- forecasts_flows_func(date_sim = date_sim,
                                         demands.fc.df = demands.fc.df,
                                         sen_outflow_today = last(sen.ts.df$outflow),
                                         jrr_outflow_today = last(jrr.ts.df$outflow),
                                         flows.ts.df = potomac.ts.df)
  # Grab some results for use as input in next step
  lfalls_obs_today_no_ws <- last(potomac.ts.df$lfalls_obs)
  lfalls_obs_fc9_no_ws <- last(potomac.ts.df$lfalls_obs_fc9)
   #-----------------------------------------------------------------------------
   # 3. Compute today's ws needs
   #-----------------------------------------------------------------------------
   #
   # Compute ws need today - for Seneca release
   #
   ws_need_0day <- estimate_need_func(lfalls_flow = lfalls_obs_today_no_ws,
                                mos = mos_0day)
   ws_need_9day <- estimate_need_func(lfalls_flow = lfalls_obs_fc9_no_ws,
                                     mos = mos_9day)
   #
   # Compute ws need in 9 days - for N Br release
   need <- lfalls_flowby + mos_9day - lfalls_obs_fc9_no_ws
   ws_need_9day <- if_else(need > 0, need, 0, missing = 0)
   #
   #-----------------------------------------------------------------------------
   # 4. Compute today's reservoir releases, taking into account ws needs
   #-----------------------------------------------------------------------------
   #   There are no withdrawals from Sen or JRR
   #
      sen.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                            res = sen, 
                                            res.ts.df = sen.ts.df,
                                            withdr_req = 0,
                                            ws_rel_req = ws_need_0day)
      jrr.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                            res = jrr, 
                                            res.ts.df = jrr.ts.df,
                                            withdr_req = 0,
                                            ws_rel_req = ws_need_9day)
   #
   #-----------------------------------------------------------------------------
   # 5. Do final update of flows in potomac.ts.df
   #   -  taking into account possible changes in res releases
   #-----------------------------------------------------------------------------
   # On the one hand, there are repetitive aspects 
   #   to use of the same function twice (here and step 2).
   # On the other hand, it seems more consistent 
   #   to use just one function to update flows.
      potomac.ts.df <- forecasts_flows_func(date_sim = date_sim,
                                            demands.fc.df = demands.fc.df,
                                            sen_outflow_today = last(sen.ts.df$outflow),
                                            jrr_outflow_today = last(jrr.ts.df$outflow),
                                            flows.ts.df = potomac.ts.df)
} # end of simulation loop
#
# *************************************************
# Prepare ts for graphing:
potomac.graph.df0 <- left_join(potomac.ts.df, 
                               potomac.data.df, 
                               by = "date_time") %>%
  dplyr::select(date_time, lfalls_nat = lfalls_nat.x, 
                por_nat, demand, lfalls_obs,
                sen_outflow, jrr_outflow)
potomac.graph.df <- potomac.graph.df0 %>%
  gather(key = "location", 
         value = "flow_mgd", -date_time) 
# dplyr::filter(date_time <= date_today)
# temp.df <- data.frame(date_time = date_today + 9, 
#                       location = "test", 
#                       flow_mgd = 999,
#                       stringsAsFactors = FALSE)
# *************************************************

 
