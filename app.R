library(shiny)
library(bs4Dash)
library(plotly)
library(data.table)
library(dplyr)
library(DT)
library(fresh)
library(tidyverse)
library(shinycssloaders)

theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    #olive = "#013220",
    purple = "#8965CD"
  ),
  bs4dash_status(
    primary = "#013220",
    info = "#013220"
  )
)




# Load the data
iq_data <- fread("es_submission_pbi.csv", na.strings = c("", "NA"))

##info box values
##solar
solar_percentage_use <- iq_data |>
  summarise(number_of_cases = n(),
            number_of_solar = sum(solar == TRUE)) |>
  mutate(solar_percentage_use = number_of_solar / number_of_cases)|>
  mutate(solar_percentage_use = round(solar_percentage_use,2))|>
  pull(solar_percentage_use)

##grid
grid_percentage_use <- iq_data |>
  summarise(number_of_cases = n(),
            number_of_grid = sum(grid == TRUE)) |>
  mutate(grid_percentage_use = number_of_grid / number_of_cases)|>
  mutate(grid_percentage_use = round(grid_percentage_use,2))|>
  pull(grid_percentage_use)

##Generator
diesel_percentage_use <- iq_data |>
  summarise(number_of_cases = n(),
            number_of_diesel = sum(diesel == TRUE)) |>
  mutate(diesel_percentage_use = number_of_diesel / number_of_cases)|>
  mutate(diesel_percentage_use = round(diesel_percentage_use,2))|>
  pull(diesel_percentage_use)

##battery
battery_percentage_use <- iq_data |>
  summarise(number_of_cases = n(),
            number_of_battery = sum(battery_state == "discharge")) |>
  mutate(battery_percentage_use = number_of_battery / number_of_cases)|>
  mutate(battery_percentage_use = round(battery_percentage_use,2))|>
  pull(battery_percentage_use)



##Application
shinyApp(
  ui = dashboardPage(
    freshTheme = theme,
    help = NULL,
    fullscreen = T,
    scrollToTop = TRUE,
    
    title = "OmniGrid-IQ",
    
    header = dashboardHeader(

      
      #status= "olive",
      title = dashboardBrand(title = "OmniGrid-IQ",
                             href = "https://aldridge-xvii.shinyapps.io/OmniGrid-IQ/",
      image =  'https://github.com/Aldridgexvii/Logos/blob/main/OmniGrid-IQ-logo-1c.png?raw=true'                      
                             ),
      rightUi = dropdownMenu(
        badgeStatus = "info",
        type = "notifications"
      )
    ),
    
    sidebar = dashboardSidebar(
      
      # ## filter to sidebar
      # selectInput(
      #   inputId = "site_name",
      #   label = "Site",
      #   choices = unique(iq_data$site_name),
      #   width = "100%"
      # ),
      
      sidebarMenu(
        id="sidebarMenuid",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("bar-chart"))
      )
      
    ),
    
    controlbar = dashboardControlbar(
      id = "controlbar",
      skin = "light",
      pinned = TRUE,
      overlay = FALSE,
      controlbarMenu(
        id = "controlbarMenu",
        type = "pills",
        controlbarItem(
          "Filter",
          column(
            width = 12,
            align = "center",
            selectInput(
              inputId = "site_name",
              label = "Site",
              choices = unique(iq_data$site_name),
              width = "100%"
            )
            
          )
          
          
        )
      )
      
    ),
    
    footer = dashboardFooter(right = "AldridgeXVII 2024"),
    
    body = dashboardBody(

      tabItems(
        ## Home Tab
        tabItem(tabName = "home",
                jumbotron(title = "OmniGrid IQ",
                          status = "info",
                          lead = "Smart Energy Supply Optimization",
                          href = "https://github.com/Aldridgexvii",
                          btnName = "Github"
                          ),

                ),
        
        ## Dashboard
        tabItem(tabName = "dashboard",
                
                ## Info Boxes
                # Note total column width is 12 
                # fonts: https://fontawesome.com/
                fluidRow(
                  column(width = 3, infoBox(
                  width = 12, title = "solar usage (%)", value = solar_percentage_use*100, icon = icon("sun"), color = "olive"
                )
                ),
                
                column(width = 3, infoBox(
                  width = 12, title = "grid usage (%)", value = grid_percentage_use*100, icon = icon("bolt"), color = "olive"
                )
                ),
                
                column(width = 3, infoBox(
                  width = 12, title = "diesel usage (%)", value = diesel_percentage_use*100, icon = icon("oil-well"), color = "olive"
                )
                ),
                
                column(width = 3, infoBox(
                  width = 12, title = "battery usage (%)", value = battery_percentage_use*100, icon = icon("battery"), color = "olive"
                )
                )
                
                ),
                
                ## Sort-able boxes
                fluidRow(
                  ## filter box
                  box(
                    title = "Site Selector", 
                    width = 12, 
                    #status = "olive",
                    collapsible = T, 
                    # ribbon(
                    #   text = "NEW",
                    #   color = "olive"
                    # ),
                    selectInput(
                      inputId = "site_name",
                      label = "Site",
                      choices = unique(iq_data$site_name),
                      width = "100%"
                    )
                  ),
                  box(
                    title = "AI Recommendation", 
                    width = 12, 
                    status = "olive",
                    collapsible = FALSE, 
                    ribbon(
                      text = "GenAI",
                      color = "olive"
                    ),
                    

                    bs4ValueBoxOutput("reco_table", width = 12)%>% withSpinner()

                  ),
                  
                  sortable(
                    width = 6,
                    
                    
                    box(
                      title = "Timeseries of Power Supply and Demand", 
                      width = 12, 
                      status = "olive",
                      collapsible = T, 
                      maximizable = TRUE,
                      # ribbon(
                      #   text = "NEW",
                      #   color = "olive"
                      # ),
                      
                      plotlyOutput("timeseries_plot")%>% withSpinner()
                    )
                    
                  ),
                  
                  sortable(
                    width = 6,
                    
                    box(
                      title = "Power Source Usage", 
                      width = 12, 
                      status = "olive",
                      collapsible = T, 
                      maximizable = TRUE,
                      # ribbon(
                      #   text = "NEW",
                      #   color = "olive"
                      # ),
                      
                      plotlyOutput("donut_plot")%>% withSpinner()#donut_plot
                    )
                    
                  ),
                  
                  
                  sortable(
                    width = 12,
                    
                    box(
                      title = "Energy Use Schedule Heatmap", 
                      width = 12, 
                      status = "olive",
                      collapsible = T, 
                      maximizable = TRUE,
                      # ribbon(
                      #   text = "NEW",
                      #   color = "olive"
                      # ),
                      
                      plotlyOutput("heatmap")%>% withSpinner()#donut_plot
                    )
                    
                  )
                  
                  
                ),
                
                ## Tab box---
                tabBox(
                  title = "Data",
                  width = 12,
                  type = "tabs",
                  status = "olive",
                  solidHeader = TRUE,
                  
                  # tabPanel(
                  #   "Strategy 1",
                  #   DTOutput("data_table")
                  # ),
                  tabPanel(
                    "Strategy",
                    DTOutput("data_table")
                  )
                  
                )
        )
      )
    )
    
  ),
  server = function(input, output) {
    
    
    
    
    # Reactive expression to filter the data based on site_name
    filtered_data <- reactive({
      iq_data %>% 
        filter(site_name == input$site_name)
    })

    
    
    #timeseries plot
    output$timeseries_plot <- renderPlotly({
      df_filtered <- filtered_data() %>%
        group_by(time) %>%
        summarise(max_demand = max(max_total_energy_k_wh),
                  supply = mean(total_energy))
      
      plot_ly(df_filtered, x = ~time, y = ~max_demand, 
              type = 'scatter', mode = 'none', name = 'Demand (kWh)', 
              fill = 'tozeroy', fillcolor = 'rgba(0, 100, 250, 0.5)') %>%
        add_trace(y = ~supply, name = 'Supply (kWh)', 
                  fill = 'tonexty', fillcolor = 'rgba(0, 200, 100, 0.5)') %>%
        layout(title = paste("Forecasts for", input$site_name),
               xaxis = list(title = "Time"),
               yaxis = list(title = "Energy (kWh)"))
    })
    
    
    # Plot 2: Donut Chart
    
    output$donut_plot <- renderPlotly({
      df_option <- filtered_data() %>%
        count(option)
      plot_ly(df_option, labels = ~option, values = ~n, 
              type = 'pie', hole = 0.4, 
              textinfo = 'label+percent',
              marker = list(colors = c('rgba(50, 171, 96, 0.6)', 
                                       'rgba(219, 64, 82, 0.6)', 
                                       'rgba(55, 128, 191, 0.6)', 
                                       'rgba(255, 165, 0, 0.6)'))) %>%
        layout(title = paste("Insights for", input$site_name),
               showlegend = TRUE)
    })

    # Plot 3: Data Table
    output$data_table <- renderDT({
      filtered_data() %>%
        arrange(time) %>%
        select(site_name, option, hour, time, solar, diesel, grid) 
    })
    

    # Plot 4: Recommendation Table
    output$reco_table <- renderbs4ValueBox({
     value =  filtered_data() %>%
        group_by(site_name) |>
        summarise(recommendation = unique(recommendation, na.rm= T))|>
      pull(recommendation)
     subtitle = "..."
     bs4ValueBox(value,subtitle, icon = icon("wand-magic-sparkles"),width = 12,color = "olive", footer = "Risk Analysis", elevation = 3)
    })

    
    # Plot 5: Heatmap Chart
    
    output$heatmap <- renderPlotly({
      
      # Aggregate the data to get the mean max_total_energy_k_wh for each option-time combination
      df_heatmap <- filtered_data() %>%
        group_by(option, time) %>%
        summarise(mean_energy = mean(max_total_energy_k_wh, na.rm = TRUE)) %>%
        arrange(time) %>%
        ungroup()

      # Pivot the data to wide format
      heatmap_data <- df_heatmap %>%
        pivot_wider(names_from = time, values_from = mean_energy, values_fill = list(mean_energy = 0))

      # Extract only numeric data for calculating the median
      numeric_columns <- heatmap_data %>%
        select(-option) %>%
        as.matrix()

      # Calculate the median value of the numeric columns
      median_value <- median(numeric_columns, na.rm = TRUE)

      # Convert the data to a matrix format for Plotly
      heatmap_matrix <- as.matrix(heatmap_data[,-1])
      row_names <- heatmap_data$option
      col_names <- colnames(heatmap_data)[-1]
      
      # Create the heatmap
      plot_ly(
        z = heatmap_matrix,
        x = col_names,
        y = row_names,
        type = "heatmap",

        colorbar = list(title = "Energy")
      ) %>%
        layout(
          title = paste("Heatmap of Total Energy (kWh) for", input$site_name),
          xaxis = list(title = "Time")#,
          #yaxis = list(title = "Option")
        )
      
      
      
    })
    
  }
)
