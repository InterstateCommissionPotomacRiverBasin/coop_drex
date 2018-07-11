#------------------------------------------------------------------
#------------------------------------------------------------------
# Create two dataframes: Potomac River inflow & outflow data
#    and Potomac River simulated flow time series
#------------------------------------------------------------------
#------------------------------------------------------------------
#
#--------------------------------------------------------------------------------
# Create dataframe of the flow data needed to simulate Potomac River flows,
#   ie the "natural" river flows, trib inflows, withdrawals
#--------------------------------------------------------------------------------
potomac.data.df <- flows.daily.mgd.df %>%
  dplyr:: select(date_time, por_nat, below_por, lfalls_nat) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
#
# For the moment need to be careful - didn't add enough demand data
demands.daily.df <- demands.daily.df %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
#
potomac.data.df <- left_join(potomac.data.df, 
                             demands.daily.df,
                             by = "date_time") %>%
  select(date_time,por_nat, below_por, 
         lfalls_nat, demands_total_unrestricted)
#--------------------------------------------------------------------------------
# Create and initialize dataframe of Potomac simulated flow time series
#--------------------------------------------------------------------------------
potomac.ts.df <- potomac.data.df[1,] %>%
  mutate(lfalls_mgd = lfalls_nat - demands_total_unrestricted)

# Make the 9-day flow forecast, using our old empirical eq., also used in PRRISM
#--------------------------------------------------------------------------------
#
# Switch to "wide" format to make forecasts.
# Right now don't want to graph LFalls 9-day fc, 
#   just use it to determine jrr release
 # potomac.fc.df <- potomac.data.df %>%
 #    tidyr:: spread(key = "location", value = "flow_mgd", sep = NULL) %>%
 #   dplyr:: mutate(lfalls_fc_9days = 288.79*exp(0.0009*lfalls_nat))
 # #
 # potomac.final.df <- potomac.fc.df %>%
 #   gather(key = "location", value = "flow_mgd", -date_time)
#
