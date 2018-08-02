#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Define a function that provides the necessary flow forecasts
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# date_sim = current date in the simulation
# demands = forecasts of demands from date_sim to date_sim + 14
# data.df = potomac.data.df - wide format
#    this contains natural river flows, trib flows, other?
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# Returns a vector of 15 Potomac River flows at Little Falls,
#   beginning with date_sim and ending with 14 days hence.
#--------------------------------------------------------------------------------
#
# date_sim = as.Date("1930-03-15")
# data.df = potomac.data.df
#
# Remember that potomac.ts.df is only updated thru yesterday
# It's tempting to do a preliminary update for today.
# But then what about 9 days hence?
#
forecasts_flows_func <- function(date_sim,
                                 sen_outflow_today,
                                 jrr_outflow_today,
                                 data.df,
                                 demands.fc.df, 
                                 flows.ts.df){
  #------------------------------------------------------------------------------
  # First the 1-day fc of lfalls_obs
  #------------------------------------------------------------------------------
  # Later check to see to what date lfalls_obs is correct at!
  # Grab today's flow fc - temporarily from the data.df
  #
  # for QAing:
  flows.ts.df <- potomac.ts.df
  #
  # Imagine what today's flow would be if the Seneca release
  #   didn't consider ws needs:
  flows.today <- flows.ts.df %>% 
    dplyr::filter(date_time == date_sim) %>%
    dplyr::mutate(sen_outflow = sen_outflow_today,
                  jrr_outflow_lagged = lag(jrr_outflow, 9, default = 120),
                  lfalls_obs = lfalls_nat - demand +
                    sen_outflow + jrr_outflow_lagged) %>% 
#    dplyr::select(date_time, lfalls_nat, demand, lfalls_obs,
#                  sen_outflow, jrr_outflow)
  flows <- rep(999, 15)
  flows[1] <- flows.today$lfalls_obs[1]
  #
  #------------------------------------------------------------------------------
  # Next the 9-day fc of lfalls_obs
  #------------------------------------------------------------------------------
  # this is our empirical formula
  # in ops, we would estimate lfalls_nat from gage data
  # in future should have better fcs
  lfalls_nat <- flows.today$lfalls_nat[1]
  # sen_outflow today - in absense of ws need - should do 
  sen_outflow_9days <- sen_outflow_today
  # in ops, demand in 9 days is provided by our demand model
  demand_9days <- demands.fc.df$demands_fc[10]
  #
  flow_9day_pre <- 288*exp(0.0009*lfalls_nat)
  flow_9day <- if_else(flow_9day_pre <= lfalls_nat,
                       flow_9day_pre, lfalls_nat*1.0) # lfalls_nat is integer
  
  flow_9day <- flow_9day + jrr_outflow_today +
    sen_outflow_9days - demand_9days
  flows[10] <- flow_9day
  return(flows)
}