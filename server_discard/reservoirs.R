#
cap <- 120 # reservoir capacity
s0 <- 100 # initial reservoir storage
withdr_req <- 20 # withdrawal
lts <- length(inflows.df[,1]) # length of input time series
#
# construct a reservoir df:
#  - stor is beginning of day storage
#  - inflow is inflow
#  - w is withdrawal
sen.df <- inflows.df %>%
  dplyr::mutate(inflow = lsen_in, s = 100, w = withdr_req, spill = 0) %>%
  dplyr::select(date_time, s, inflow, spill, w)
#
# Implement the water balance eq. for s = beginning of period (BOP) storage:
#   s(i+1) = s(i) + inflow(i) - w(i)
#   taking into account constraints:
#       0 <= s <= cap
#       w(i) = withdr_req(i), or = s + inflow if not enough water
#       spill(i) = excess water, over capacity
# (Note the loop will write 1 extra row at end with date_time = <NA>)
for (i in 1:lts) {
  sen.nextrow <- sen.df[i,]
  sen.nextrow <- sen.nextrow %>%
    mutate(available = s + inflow - w,
           spill = case_when(available - cap > 0 ~ available - cap,
                             available - cap <= 0 ~ 0),
           w = case_when(s + inflow >= w ~ w,
                         s + inflow < w ~ s + inflow),
           s = case_when(available > cap ~ cap,
                         available <= cap & available > 0 ~ available,
                         available <= cap & available <= 0 ~ 0)
                         ) %>%
    select(date_time, s, inflow, spill, w)
  # update today's spill and withdrawal and tomorrow's BOP storage
  sen.df[i,4] <- sen.nextrow[1,4]
  sen.df[i,5] <- sen.nextrow[1,5]
  sen.df[i+1,2] <- sen.nextrow[1,2]  
  } # end of for loop