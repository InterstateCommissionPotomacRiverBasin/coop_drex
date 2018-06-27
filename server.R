#
# This is the server logic of a Shiny web application for the 2018 DREX.
# Run the application by clicking 'Run App' above.
# 
# # Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
  #
  #-----------------------------------------------------------------
  # Block temporarily pasted into global.R - for ease of debugging
  #-----------------------------------------------------------------

  #-----------------------------------------------------------------
  # End block temporarily pasted into global.R
  #-----------------------------------------------------------------
  #

  #
  #------------------------------------------------------------------
  # Create the graphs to be displayed by the Shiny app
  #------------------------------------------------------------------
  output$potomacFlows <- renderPlot({
    ggplot(data = potomac.df, aes(x = date_time, y = flow_mgd, group = location)) +
      geom_line(aes(linetype = location, color = location, size = location)) +
      scale_linetype_manual(values = c("dotted", "solid", "solid")) +
      scale_size_manual(values = c(1, 3, 1)) +
      scale_color_manual(values = c("blue", "skyblue1", "blue"))
  })
  output$por_flow <- renderValueBox({
    por_threshold <- 2000
    por_flow <- 1800
    valueBox(
      value = por_flow,
      subtitle = "Flow at Point of Rocks, cfs (Trigger for daily monitoring is 2000 cfs)",
      color = if (por_flow >= por_threshold) "green" else "yellow"
    )
  })
  #
  output$lfaa_alert <- renderValueBox({
    lfaa_alert_threshold <- 800
    lfaa_alert <- 700
    valueBox(
      value = lfaa_alert,
      subtitle = "Little Falls adjusted flow, MGD (Trigger for LFAA Alert stage is 2 x total WMA withdrawals)",
      color = if (lfaa_alert >= lfaa_alert_threshold)
        "green" else "orange"
    )
  })
  #
  output$jrrStorageReleases <- renderPlot({
    ggplot(data = jrr.ops.df, aes(x = date_time)) +
      geom_line(aes(y = stor, color = "Storage")) +
      geom_line(aes(y = rel, color = "Spill")) +
      scale_color_manual(values = c("grey", "black"))
    #    scale_linetype_manual(values = c("solid", "dotted"))
  }) # end renderPlot
  #
  output$senStorageReleases <- renderPlot({
    ggplot(data = sen.ops.df, aes(x = date_time)) +
      geom_line(aes(y = stor, color = "Storage")) +
      geom_line(aes(y = rel, color = "Spill")) +
      scale_color_manual(values = c("grey", "black"))
    #    scale_linetype_manual(values = c("solid", "dotted"))
  }) # end renderPlot
  
  mde_map = "http://mde.maryland.gov/programs/Water/droughtinformation/Currentconditions/PublishingImages/DroughtGraphsStarting2017Apr30/Drought2018-04-30.png"
  output$MDEStatus <- renderText({c('<img src="', mde_map, '">')
  })
  
  vadeq_map = "http://deq1.bse.vt.edu/drought/state/images/maps/imageMapFile152838207923720.png"
  output$VADEQStatus <- renderText({c('<img src="', vadeq_map, '">')
  })

  })

