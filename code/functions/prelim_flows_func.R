#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# A function that updates potomac.ts.df with yesterday & fc's
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
#--------------------------------------------------------------------------------
#
# date_sim = as.Date("1930-03-15")
# data.df = potomac.data.df
prelim_flows_func <- function(date_sim,
                              sen.ts.df,
                              jrr.ts.df,
                              flows.ts.df,
                              demands.df, 
                              flow.data.df){
  #------------------------------------------------------------------------------
  # Strip records of yesterday and today from potomac.ts.df
  #--------------------------------------------------------------------------------
  # For QAing only:
  flows.ts.df <- potomac.ts.df
  demands.df <- demands.daily.df
  flow.data.df <- potomac.data.df
  #
  flows.ts.df <- flows.ts.df %>%
    dplyr::filter(date_time < date_sim)
  #------------------------------------------------------------------------------
  # "Read" yesterday's observations & update potomac.ts.df
  #--------------------------------------------------------------------------------
  # The problems are:
  #  just what NEEDS updating?
  #  how do I REWRITE yesterday in potomac.ts.df?
  demand_yesterday <- flow.data.df$demands_total_unrestricted[sim_i - 1]
  lfalls_nat_yesterday <- flow.data.df$lfalls_nat[sim_i - 1]
  flows.yesterday <- flow.data.df[sim_i - 1,] %>%
    dplyr::mutate(sen_outflow = sen.ts.df$outflow[sim_i - 1],
                  jrr_outflow = jrr.ts.df$outflow[sim_i - 1],
                  lfalls_obs = lfalls_nat +
                     lag(jrr_outflow, 0) +
                     lag(sen_outflow, 0) -
                     demand_yesterday,
                  demand = demand_yesterday)
  flows.yesterday <- flows.yesterday %>%
    dplyr::mutate(
                  sen_outflow = sen.ts.df$outflow[sim_i - 1],
                  jrr_outflow = jrr.ts.df$outflow[sim_i - 1]) # %>%
  flows.yesterday <- flows.yesterday %>%
      dplyr::select(date_time, lfalls_nat, demand, 
                  lfalls_obs, sen_outflow, jrr_outflow)
  #
  flows.ts.df <- rbind(flows.ts.df, flows.yesterday)
  #
  #------------------------------------------------------------------------------
  # Forecast today's flows & demands & write to potomac.ts.df
  #--------------------------------------------------------------------------------
  # sim_i <- 15
  demand_today <- demands.df[sim_i, 2]
  flows.today <- flow.data.df[sim_i,] %>%
    dplyr::mutate(lfalls_obs = lfalls_nat +
                    lag(jrr_outflow, 0) +
                    lag(sen_outflow, 0) -
                    demand_today,
                  demand = demand_today,
                  sen_outflow = sen.ts.df$outflow[sim_i],
                  jrr_outflow = jrr.ts.df$outflow[sim_i]) %>%
    dplyr::select(date_time, lfalls_nat, demand, 
                  lfalls_obs, sen_outflow, jrr_outflow)
  #
  flows.ts.df <- rbind(flows.ts.df, flows.today)
  #------------------------------------------------------------------------------
  # Get 9-day flow fc
  #--------------------------------------------------------------------------------
#   flows <- data.df %>% # at start this is wide potomac.data.df
#     dplyr::filter(date_time = date_sim - 1) # grab yesterday's obs
# #
#       dplyr::mutate(lfalls_from_upstr = lag(por_nat, 2) +
#                     lag(below_por, 2)) %>%
#     dplyr::select(date_time, lfalls_nat, lfalls_from_upstr, por_nat)
#   flows.updated <- dplyr::left_join(flows, demands.fc.df, 
#                                   by = "date_time") %>%
#     dplyr::mutate(lfalls_obs = lfalls_nat - demands_fc)
  return(flows.updated)
}