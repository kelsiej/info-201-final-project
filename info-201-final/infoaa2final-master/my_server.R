
server <- function(input, output) {
  source("data_wrangling.R")
  source("insurance_wrangling.R")
  source("le_by_state.R")
  # In case you need to hide them for some reason

  # Server stuff
  

  ################## 
  ### Question 1 ###
  ##################
  
  # reactive function makes income_by_le's Year equal to the year input in by the selectInput in the ui
  
  
  output$overview <- renderPlotly({
    
    ggplotly(overview_plot)
    
  })
  
  
  results_data1 <- reactive({
    
    results <- income_by_le[income_by_le$Year == input$year, ]
    results
    
    
  })
  
 
  

  output$table1 <- renderDataTable({
    get_result <- results_data1()
    get_result
  })
  
  output$plotly1b <- renderPlotly({
    
    plot1b <- ggplot(data = income_by_le) +
      geom_point(mapping = aes(x = Year, y = median_income,  color = Race)) +
      labs(title = "Median Income Over Time",
           x = "Year",
           y = "Median Income") +
      theme(plot.title = element_text(size = 18)) 
    ggplotly(plot1b, tooltip = c("Year", "Race", "median_income"))
  })
  

  
  output$plotly1c <- renderPlotly({
    plot1c <- ggplot(data = income_by_le) +
      geom_point(mapping = aes(x = Year, y = Avg.Life.Expectancy.Years,  color = Race)) +
      labs(title = "Average Life Expectancy Over Time",
           x = "Year",
           y = "Average Life Expectancy") +
      theme(plot.title = element_text(size = 18)) 
    
    ggplotly(plot1c, tooltip = 
               c("Year", "Race", "Avg.Life.Expectancy.Years")
             )
    
  })
  
  ################## 
  ### Question 2 ###
  ##################
  
  
  output$table2 <- renderDataTable({
    get_result <- results_data2()
    get_result
    
  })
  
  
  output$plot2white <- renderPlotly({
    
    plot2white <- ggplot(data = new_data) +

      geom_polygon(aes(x = long, y = lat, group = group, 
                       fill = cut(new_data$White, seq(65, 90, by = 2.5), include.lowest = TRUE), label = region)) +
      scale_fill_manual(name = "Age Range", values = c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8"), na.value = "#636363") +
      labs(title = "Life Expectancies for Caucasians in 2008") +
      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14,  face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2white + geom_point()
    ggplotly(plot2white, tooltip="region")
  })
  
  output$plot2afa <- renderPlotly({
    
    plot2afa <- ggplot(data = new_data) +
      geom_polygon(aes(x = long, y = lat, group = group, 

                       fill = cut(new_data$African.American, seq(65, 90, by = 2.5), include.lowest = TRUE), label = region)) +
      scale_fill_manual(name = "Age Range", values = c("#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4"), na.value = "#636363") +
      labs(title = "Life Expectancies for Afican Americans in 2008") +
      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14, face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2afa + geom_point()
    ggplotly(plot2afa, tooltip="region")
  })
  
  output$plot2asa <- renderPlotly({
    
    plot2asa <- ggplot(data = new_data) +
      geom_polygon(aes(x = long, y = lat, group = group, 
                       fill = cut(new_data$Asian.American, seq(65, 90, by = 2.5), include.lowest = TRUE))) +
      scale_fill_manual(name = "Age Range", values = c("#225ea8", "#253494", "#081d58"), na.value = "#636363") +
      labs(title = "Life Expectancies for Asian Americans in 2008") +

      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14, face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2asa + geom_point()
    ggplotly(plot2asa, tooltip="region")
  })
  
  
  ################## 
  ### Question 2 ###
  ##################
  
  output$plot2na <- renderPlotly({
    
    plot2na <- ggplot(data = new_data) +
      geom_polygon(aes(x = long, y = lat, group = group, fill =
                         cut(new_data$Native.American, seq(65, 90, by = 2.5), include.lowest = TRUE))) +
      scale_fill_manual(name = "Age Range", values = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#1d91c0"), na.value = "#636363")  + 
      labs(title = "Life Expectancies for Native Americans in 2008") +

      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14, face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2na + geom_point() 
    ggplotly(plot2na, tooltip="region") 
  })
  
  output$plot2lat <- renderPlotly({
    
    plot2lat <- ggplot(data = new_data) +
      geom_polygon(aes(x = long, y = lat, group = group, fill =
                         cut(new_data$Latino, seq(65, 90, by = 2.5), include.lowest = TRUE))) + 
      scale_fill_manual(name = "Age Range", values = c("#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"), na.value = "#636363") +
      labs(title = "Life Expectancies for Latinos in 2008") +
      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14, face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2lat + geom_point()
    ggplotly(plot2lat, tooltip="region")
  })
  output$plot2white <- renderPlotly({
    
    plot2white <- ggplot(data = new_data) +
      geom_polygon(aes(x = long, y = lat, group = group, 
                       fill = cut(new_data$White, seq(65, 90, by = 2.5), include.lowest = TRUE), label = region)) +
      scale_fill_manual(name = "Age Range", values = c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8"), na.value = "#636363") +
      labs(title = "Life Expectancies for Whites in 2008") +
      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14,  face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2white + geom_point()
    ggplotly(plot2white, tooltip="region")
  })
  
  output$plot2afa <- renderPlotly({
    
    plot2afa <- ggplot(data = new_data) +
      geom_polygon(aes(x = long, y = lat, group = group, 
                       fill = cut(new_data$African.American, seq(65, 90, by = 2.5), include.lowest = TRUE), label = region)) +
      scale_fill_manual(name = "Age Range", values = c("#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4"), na.value = "#636363") +
      labs(title = "Life Expectancies for Afican Americans in 2008") +
      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14, face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2afa + geom_point()
    ggplotly(plot2afa, tooltip="region")
  })
  
  output$plot2asa <- renderPlotly({
    
    plot2asa <- ggplot(data = new_data) +
      geom_polygon(aes(x = long, y = lat, group = group, 
                       fill = cut(new_data$Asian.American, seq(65, 90, by = 2.5), include.lowest = TRUE))) +
      scale_fill_manual(name = "Age Range", values = c("#225ea8", "#253494", "#081d58"), na.value = "#636363") +
      labs(title = "Life Expectancies for Asian Americans in 2008") +
      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14, face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2asa + geom_point()
    ggplotly(plot2asa, tooltip="region")
  })
  
  output$plot2na <- renderPlotly({
    
    plot2na <- ggplot(data = new_data) +
      geom_polygon(aes(x = long, y = lat, group = group, fill =
                         cut(new_data$Native.American, seq(65, 90, by = 2.5), include.lowest = TRUE))) +
      scale_fill_manual(name = "Age Range", values = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#1d91c0"), na.value = "#636363")  + 
      labs(title = "Life Expectancies for Native Americans in 2008") +
      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14, face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2na + geom_point() 
    ggplotly(plot2na, tooltip="region") 
  })
  
  output$plot2lat <- renderPlotly({
    
    plot2lat <- ggplot(data = new_data) +
      geom_polygon(aes(x = long, y = lat, group = group, fill =
                         cut(new_data$Latino, seq(65, 90, by = 2.5), include.lowest = TRUE))) + 
      scale_fill_manual(name = "Age Range", values = c("#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"), na.value = "#636363") +
      labs(title = "Life Expectancies for Latinos in 2008") +
      theme_light() + theme(
        plot.title = element_text(color = '#02818a', size = 14, face = 'bold'),
        axis.title.x = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic'),
        axis.title.y = element_text(color = '#bfd3e6', size = 10, face = 'bold.italic') 
      )
    plot2lat + geom_point()
    ggplotly(plot2lat, tooltip="region")
  })
  
  ################## 
  ### Question 3 ###
  ##################
  results_year_slider <- reactive({
    results <- uninsured_data %>%
      filter(Year >= input$insurance_year[1] & Year <= input$insurance_year[2])


    results
  })
  
  output$plot3 <- renderPlot({
    ggplot(results_year_slider(), aes(x = Year, y = Data, color = `Race/Ethnicity`)) +
      geom_point() +
      facet_grid(~`Race/Ethnicity`) +
      ggtitle("Percent of Uninsured by Race") +
      theme(plot.title = element_text(size = 30), legend.position = "none") 
      
  })
  
  ################## 
  ### Question 4 ###
  ##################
  
  #reactive function changes life expectancy range
  results_le_slider <- reactive({
    
    results <- trend %>%
      filter(avg.life.expectancy >= input$avg_le[1] & avg.life.expectancy <= input$avg_le[2])
    
    results
  })
  
  output$plot4 <- renderPlotly({
    
    set.seed(955)
    trend_plot <- ggplot(data = results_le_slider(), aes(x = State, y = avg.life.expectancy, color = State)) +
      geom_point(shape = 11) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(
        title = "Life Expectancy Change From 1987 to 2009",
        x = "States",
        y = "Change in Life Expectancy (in years)")
    trend_plot <- ggplotly(trend_plot)
    trend_plot
    
  })
  
  output$map1q4 <- renderHighchart({
    map_1987 <- hcmap("countries/us/us-all", data = le_state_1987, value = "avg.life.expectancy",
                      joinBy = c("name", "State"), name = "Life Expectancy (in years)",
                      dataLabels = list(enabled = TRUE, format = "{point.name}"),
                      borderColor = "#FAFAFA", borderWidth = 0.1,
                      tooltip = list(valueDecimals = 2, valueSuffix = " years"))

    map_1987
  })
  
  output$map2q4 <- renderHighchart({
    map_2009 <- hcmap("countries/us/us-all", data = le_state_2009, value = "avg.life.expectancy",
                      joinBy = c("name", "State"), name = "Life Expectancy (in years)",
                      dataLabels = list(enabled = TRUE, format = "{point.name}"),
                      borderColor = "#FAFAFA", borderWidth = 0.1,
                      tooltip = list(valueDecimals = 2, valueSuffix = " years"))
    
    map_2009
  })
}
