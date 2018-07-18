#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# A function that initializes the dfs holding the reservoir time series
#--------------------------------------------------------------------------------
#----------------------- ------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# res - the reservoir class object which contains alot of info:
#          res@name is the reservoir name
#          res@capacity is the reservoir capacity
#          res@stor0 is the initial storage on day 1 of the simulation
#          res@flowby is the minimum environmental flowby over the dam
#          res@inflows is a dataframe containing the daily inflow time series
# withdr_req - the requested withdrawal from an intake in the reservoir
# ws_rel_req - the requested release over the dam for ws purposes
#     if ws_rel_req > flowby then rel = ws_rel_req (assuming enough water)
#     if ws_rel_req <= flowby then rel = flowby (assuming enough water)
#
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# dataframe with columns: date_time, stor, inflow, rel, withdr
#
#--------------------------------------------------------------------------------
reservoir_ops_init_func <- function(res, withdr_req, ws_rel_req){
  #
  # Implement the water balance eq. for s = beginning of period (BOP) storage:
  #   s(i+1) = s(i) + inflow(i) - w(i)
  #   taking into account constraints:
  #       0 <= s <= cap
  #       w(i) = withdr_req(i), or = s + inflow if not enough water
  #       spill(i) = excess water, over capacity
#  withdr_req <- 5
#  ws_rel_req <- 15
#  rel_req <- ws_rel_req
  cap <- res@capacity
  flowby <- res@flowby
  rel_min <- ifelse(flowby > ws_rel_req, flowby, ws_rel_req)
  res0.df <- res@inflows[1,] %>%
    dplyr::mutate(stor = res@stor0, 
                  w = withdr_req,
                  inflow = inflows) %>%
    dplyr::mutate(available = stor + inflow - w,
                  # rel_min = case_when(flowby > rel_req ~ flowby,
                  #                     flowby <= rel_req ~ rel_req),
                  outflow = case_when(
                    cap - available <= -rel_min ~ available - cap, # spill
                    cap - available > rel_min & available > rel_min ~ rel_min,
                    cap - available > rel_min & available <= rel_min ~ available),
                  w = case_when(stor + inflow >= w ~ w,
                                stor + inflow < w ~ stor + inflow),
                  storage = stor
    ) %>%
    dplyr::select(date_time, storage, inflow = inflows,
                  outflow, w, available)
  return(res0.df)
}
