#
start.date <- as.Date("1929-10-01")
end.date <- as.Date("1929-12-31")
x <- "test"
#
potomac.df <- flow.daily.mgd.df %>%
  dplyr:: select(date_time, por_nat, below_por, lfalls_nat) %>%
  dplyr:: filter(date_time <= end.date) %>%
  gather(key = "location", value = "flow_mgd", -date_time)
#
output$potomacFlows <- renderPlot({
  ggplot(potomac.df, aes(x = date_time, y = flow_mgd, group = "location"))
})
#
# output$potomac_flows <- ggplot(sub.df, aes(x = date_time, y = flow,
#                                  color = site,
#                                  linetype = site,
#                                  size = site))