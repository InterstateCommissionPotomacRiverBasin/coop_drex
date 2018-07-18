#
#
# Define a function that takes a reservoir ops dataframe and
#   adds one row, ie, one day to the time series 
reservoir_ops_today_func <- function(res, res.ts.df, 
                                     withdr_req, ws_rel_req){
  #
  # Implement the water balance eq. for s = beginning of period (BOP) storage:
  #   s(i+1) = s(i) + inflow(i) - w(i)
  #   taking into account constraints:
  #       0 <= s <= cap
  #       w(i) = withdr_req(i), or = s + inflow if not enough water
  #       spill(i) = excess water, over capacity
  # (Note the loop will write 1 extra row at end with date_time = <NA>)
    cap <- res@capacity
    flowby <- res@flowby
    w_req <- withdr_req
# Get the last row of the res.ops.df, "yesterday", 
    # and read its values:
    yesterday.df <- tail(res.ts.df,1)
    yesterday_date <- yesterday.df[1,1] # yesterday's date
    stor <- yesterday.df[1,2] # yesterday's storage
    inflow <- yesterday.df[1,3] # yesterday's inflow
    outflow <- yesterday.df[1,4] # yesterday's release over dam
    w <- yesterday.df[1,5] # yesterday's  withdrawal from intake in reservoir
# calculate today's BOP storage
    stor <- stor + inflow - outflow - w # today's bop storage
    rel_min <- ifelse(flowby > ws_rel_req, flowby, ws_rel_req)
# calculate a new row, "today", of the res.ops.df:
    newrow.df <- subset(res@inflows, 
                        date_time == yesterday_date + 1) %>%
      mutate(stor = stor,
             inflow = inflows,
             available = stor + inflow - w_req,
             # rel_min = case_when(flowby > ws_rel_req ~ flowby,
             #                     flowby <= ws_rel_req ~ ws_rel_req),
             outflow = case_when(
               cap - available <= -rel_min ~ available - cap, # spill
               cap - available > -rel_min & available > rel_min ~ rel_min,
               cap - available > -rel_min & available <= rel_min ~ available),
             w = case_when(stor + inflow >= w_req ~ w_req,
                           stor + inflow < w_req ~ stor + inflow),
             storage = stor
      ) %>%
      select(date_time, storage, inflow, outflow, w, available)
    # add the new row, today, to res.ops.df:
    
    res.ts.df <- rbind(res.ts.df, newrow.df)
  return(res.ts.df)
}