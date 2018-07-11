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
forecasts_flows_func <- function(date_sim, 
                                 flows.ts.df,
                                 demands.fc.df, 
                                 data.df){
  flows <- data.df %>% # at start this is wide potomac.data.df
    dplyr::filter(date_time >= date_sim - 1, # grab yesterday's obs
                  date_time < date_sim + 15) %>% # grab fc's
    dplyr::mutate(lfalls_from_upstr = lag(por_nat, 2) +
                    lag(below_por, 2)) %>%
    dplyr::select(date_time, lfalls_nat, lfalls_from_upstr, por_nat)
  flows.updated <- dplyr::left_join(flows, demands.fc.df, 
                                  by = "date_time") %>%
    dplyr::mutate(lfalls_obs = lfalls_nat - demands_fc)
  return(flows.updated)
}