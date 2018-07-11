#------------------------------------------------------------------------------
# Info found online:
  # "global.R is a script that is executed before
  # the application launch. For this reason, it can include the same
  # pieces of code to execute the reactive independent processes,
  # but it also has an additional capability: the objects 
  # generated in global.R can be used both in server.R and UI.R"
#------------------------------------------------------------------------------
# The script, import_data.R, load in all available time series data. 
#  Right now the directory with time series data (set by paths.R)
#  has 2 years of data, from 1929-10-01 to 1931-09-30.
# date_tsdata_start <- as.Date("1929-10-01")
# date_tsdata_end <- as.Date("1931-09-30")

source("code/global/load_packages.R", local = TRUE)
source("config/paths.R", local = TRUE)
source("code/global/import_data.R", local = TRUE)
#
#-----------------------------------------------------------------
# Block temporarily pasted into global.R - for ease of debugging
#-----------------------------------------------------------------
# Define the simulation period - later do this reactively:
date_start <- as.Date("1930-01-01")
date_end <- as.Date("1930-05-31")
#
source("code/classes/reservoir_class.R", local = TRUE)
source("code/functions/reservoir_ops_init_func.R", local = TRUE)
source("code/functions/reservoir_ops_today_func.R", local = TRUE)
source("code/functions/forecasts_demands_func.R", local = TRUE)
source("code/functions/forecasts_flows_func.R", local = TRUE)
source("code/server/potomac_flows.R", local = TRUE)
source("code/server/simulation.R", local = TRUE)
#-----------------------------------------------------------------
# End block temporarily pasted into global.R
#-----------------------------------------------------------------
#
#------------------------------------------------------------------------------
plot.height <- "340px"
plot.width <- "95%"
#------------------------------------------------------------------------------




