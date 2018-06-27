#
# The simulation period was defined in global.R. Might want to add debug code to 
# verify that this is within the data date range.
# 
date_today <- as.Date("1930-01-15")
sim_n <- as.numeric(as.POSIXct(date_today) - as.POSIXct(date_start),
                    units = "days")
#
# Make reservoir objects:
source("code/server/reservoirs_make.R", local = TRUE) 
#
# Initialize reservoir dataframes:
#
# Run daily simulation of reservoir operations
 for (i in 1:10) {
   date_sim <- as.Date(date_start + i)
   sen.ops.df <- reservoir_ops_today_func(sen, sen.ops.df, sen_withdr_req, sen_ws_rel_req)
   jrr.ops.df <- reservoir_ops_today_func(jrr, jrr.ops.df, jrr_withdr_req, jrr_ws_rel_req)
   }
 
