#------------------------------------------------------------------
#------------------------------------------------------------------
# Create a dataframe of "natural" Potomac River flows, 
# ie without res releases.
# Also, compute Potomac River 9-day flow forecasts (fc).
#------------------------------------------------------------------
#------------------------------------------------------------------
#
#--------------------------------------------------------------------------------
# Create the dataframe of "natural" flows
#--------------------------------------------------------------------------------
potomac.df <- flow.daily.mgd.df %>%
  dplyr:: select(date_time, por_nat, below_por, lfalls_nat) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start) %>%
  # Convert data to "long" format for graphing:
 gather(key = "location", value = "flow_mgd", -date_time)
#
#--------------------------------------------------------------------------------
# Make the 9-day flow forecast, using our old empirical eq., also used in PRRISM
#--------------------------------------------------------------------------------
#
# Switch to "wide" format to make forecasts.
# Right now don't want to graph LFalls 9-day fc, 
#   just use it to determine jrr release
 potomac.fc.df <- potomac.df %>%
    tidyr:: spread(key = "location", value = "flow_mgd", sep = NULL) %>%
   dplyr:: mutate(lfalls_fc_9days = 288.79*exp(0.0009*lfalls_nat))
 #
 potomac.final.df <- potomac.fc.df %>%
   gather(key = "location", value = "flow_mgd", -date_time)
#
