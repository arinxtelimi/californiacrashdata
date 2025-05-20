# Title: Shiny App 3
# Description: Mapping Crash Data in California
# Author: Arin Telimi
# Date: 12/6/24

# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)    
library(lubridate)    
library(leaflet)      
library(plotly)       

# =======================================================
# Import data
#
# For demo purposes, in this "template" we use storms data
# (but you will have to replace this with the crash data)
#
# Uncomment the lines below in order to import the crash data!!!
# We're assuming the CSV file is in the working directory of your app.
# =======================================================
crashes = read_csv(
  file = "crashes_california_2014_2023.csv", 
  col_types = list(
    col_double(),    #  1) CASE_ID
    col_double(),    #  2) ACCIDENT_YEAR
    col_date(),      #  3) COLLISION_DATE 
    col_double(),    #  4) COLLISION_TIME 
    col_double(),    #  5) HOUR 
    col_integer(),   #  6) DAY_OF_WEEK 
    col_character(), #  7) WEATHER_1 
    col_character(), #  8) WEATHER_2 
    col_character(), #  9) STATE_HWY_IND
    col_character(), # 10) COLLISION_SEVERITY 
    col_integer(),   # 11) NUMBER_KILLED 
    col_integer(),   # 12) NUMBER_INJURED 
    col_integer(),   # 13) PARTY_COUNT 
    col_character(), # 14) PCF_VIOL_CATEGORY 
    col_character(), # 15) TYPE_OF_COLLISION 
    col_character(), # 16) ROAD_SURFACE 
    col_character(), # 17) ROAD_COND_1 
    col_character(), # 18) ROAD_COND_2 
    col_character(), # 19) LIGHTING 
    col_character(), # 20) PEDESTRIAN_ACCIDENT 
    col_character(), # 21) BICYCLE_ACCIDENT 
    col_character(), # 22) MOTORCYCLE_ACCIDENT 
    col_character(), # 23) TRUCK_ACCIDENT 
    col_character(), # 24) NOT_PRIVATE_PROPERTY 
    col_character(), # 25) ALCOHOL_INVOLVED 
    col_character(), # 26) COUNTY 
    col_character(), # 27) CITY 
    col_character(), # 28) PO_NAME
    col_double(),    # 29) ZIP_CODE
    col_double(),    # 30) POINT_X 
    col_double()     # 31) POINT_Y 
  ))

crashes = crashes |>
  mutate(
    MONTH = month(COLLISION_DATE, label = TRUE),
    YEAR = year(COLLISION_DATE),
    DAY_OF_WEEK = factor(DAY_OF_WEEK, 
                         levels = 1:7, 
                         labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
    HOUR_CATEGORY = cut(HOUR, 
                        breaks = c(-1, 5, 11, 17, 23), 
                        labels = c("Night (0-5)", "Morning (6-11)", "Afternoon (12-17)", "Evening (18-23)")),
    casualties = NUMBER_KILLED + NUMBER_INJURED
  )

display_names = list(
  "MONTH" = "Month",
  "DAY_OF_WEEK" = "Day of Week", 
  "HOUR_CATEGORY" = "Time of Day",
  "TYPE_OF_COLLISION" = "Type of Collision",
  "PCF_VIOL_CATEGORY" = "Violation Type",
  "PO_NAME" = "City",
  "COLLISION_SEVERITY" = "Severity"
)

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  titlePanel("California Crash Data Exploratory Analysis"),
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # ---------------------------------------------
      # input widgets of first tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Crash Data Exploration"),
        
        selectInput("po_name", 
                    "City:", 
                    choices = c("All Cities", sort(unique(crashes$PO_NAME))), 
                    selected = "All Cities"),
        
        selectInput("plot1_x", 
                    "First Plot - Crashes By:", 
                    choices = c("Month" = "MONTH", 
                                "Day of Week" = "DAY_OF_WEEK", 
                                "Hours" = "HOUR_CATEGORY"),
                    selected = "Month"),
        
        selectInput("plot2_x", 
                    "Second Plot - Top 10:", 
                    choices = c("Type of Collision" = "TYPE_OF_COLLISION", 
                                "Type of Violation" = "PCF_VIOL_CATEGORY", 
                                "City" = "PO_NAME", 
                                "Day of Week" = "DAY_OF_WEEK"),
                    selected = "Type of Violation"),
        
        selectInput("plot3_x", 
                    "Third Plot - Average Casualties By:", 
                    choices = c("City" = "PO_NAME", 
                                "Type of Collision" = "TYPE_OF_COLLISION"),
                    selected = "Type of Collision"),
        
        sliderInput("year_range", 
                    "Select Year Range:", 
                    min = 2014, 
                    max = 2023, 
                    value = c(2014, 2023)),
        
        checkboxGroupInput("severity_select", "Collision Severity:",
                           choices = unique(crashes$COLLISION_SEVERITY),
                           selected = unique(crashes$COLLISION_SEVERITY))
      ),
      # ---------------------------------------------
      # input widgets of second tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Crash Location Map"),
        
        selectInput("map_city", 
                    "City (Type to search):",
                    choices = unique(crashes$PO_NAME),
                    selected = "Berkeley"),
        
        sliderInput("map_year_range", 
                    "Year Range:", 
                    min = 2014, 
                    max = 2023, 
                    value = c(2014, 2023)),
        
        selectInput("map_metric", 
                    "Map Color By:",
                    choices = c("Collision Severity" = "COLLISION_SEVERITY", 
                                "Violation Category" = "PCF_VIOL_CATEGORY"),
                    selected = "COLLISION_SEVERITY")
      )
    ),
    
    # -------------------------------------------------------
    # Main Panel with 2 tabsets: 
    # tab1: exploratory analysis
    # tab2: map of crashes
    # -------------------------------------------------------
      mainPanel(
      tabsetPanel(
        # first tab (graphic)
        tabPanel(title = "Crash Analysis", 
                 value = 1,
                 plotlyOutput("plot1", height = "300px"),
                 plotlyOutput("plot2", height = "300px"),
                 plotlyOutput("plot3", height = "300px")
        ),
        
        # second tab (map)
        tabPanel(title = "Crash Map", 
                 value = 2,
                 leafletOutput("crash_map", height = 600)
        ),
        # selected tab
        id = "tabselected"
      ) # closes tabsetPanel
    ) # closes mainPanel
    
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)
# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  filtered_crashes = reactive({

    crashes |>
      filter(
        YEAR >= input$year_range[1] & YEAR <= input$year_range[2],
        COLLISION_SEVERITY %in% input$severity_select,
        if (input$po_name != "All Cities") PO_NAME == input$po_name else TRUE
      )
  })
  
  map_filtered_crashes = reactive({

    map_data = crashes |>
      filter(
        YEAR >= input$map_year_range[1] & YEAR <= input$map_year_range[2]
      )
    
    if (input$map_city != "") {
      map_data = map_data |>
        filter(str_to_upper(PO_NAME) == str_to_upper(input$map_city))
    }
    
    return(map_data)
  })
  
  # ------------------------------------------------
  # Output for first TAB (i.e. summary plots)
  # (adapt code to your analysis)
  # ------------------------------------------------
  
  #PLOT 1
  output$plot1 <- renderPlotly({

    
    plot_data = filtered_crashes() |>
      group_by(!!sym(input$plot1_x), 
               COLLISION_SEVERITY = COLLISION_SEVERITY) |>
      summarise(crash_count = n(), .groups = 'drop')
    
    myplot = plot_data |>
      ggplot(aes(x = !!sym(input$plot1_x), 
                 y = crash_count, 
                 fill = COLLISION_SEVERITY,
                 text = paste0(
                   display_names[[input$plot1_x]], ": ", !!sym(input$plot1_x), "<br>",
                   "Severity: ", COLLISION_SEVERITY, "<br>",
                   "Crashes: ", crash_count
                 ))) +
      geom_col(position = "dodge") +
      labs(
        title = paste("Crashes by", display_names[[input$plot1_x]]),
        x = display_names[[input$plot1_x]],
        y = "Number of Crashes"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(myplot, tooltip = "text") |>
      layout(
        legend = list(title = list(text = "Severity")),
        xaxis = list(title = display_names[[input$plot1_x]]),
        yaxis = list(title = "Number of Crashes")
      )
  })
  
  #PLOT 2
  output$plot2 <- renderPlotly({

    plot_data = filtered_crashes() |>
      group_by(!!sym(input$plot2_x)) |>
      summarise(
        crash_count = n(),
        .groups = 'drop'
      ) |>
      arrange(desc(crash_count)) |>
      slice_head(n = 10)
    
    myplot = plot_data |>
      ggplot(aes(x = reorder(!!sym(input$plot2_x), crash_count), 
                 y = crash_count,
                 text = paste0(
                   display_names[[input$plot2_x]], ": ", !!sym(input$plot2_x), "<br>",
                   "Number of Crashes: ", crash_count
                 ))) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(
        title = paste("Top 10", display_names[[input$plot2_x]], "by Crash Count"),
        x = display_names[[input$plot2_x]],
        y = "Number of Crashes"
      ) +
      theme_minimal()
    
    ggplotly(myplot, tooltip = "text") |>
      layout(
        xaxis = list(title = "Number of Crashes"),
        yaxis = list(title = display_names[[input$plot2_x]])
      )
  })
  
  #PLOT 3
  output$plot3 <- renderPlotly({

    plot_data = filtered_crashes() |>
      group_by(!!sym(input$plot3_x)) |>
      summarise(
        total_crashes = n(),
        casualties = sum(casualties),
        avg_casualties_per_crash = casualties / total_crashes
      ) |>
      arrange(desc(total_crashes)) |>
      slice_head(n = 10) 
    
    myplot = plot_data |>
      ggplot(aes(x = reorder(!!sym(input$plot3_x), avg_casualties_per_crash), 
                 y = avg_casualties_per_crash,
                 text = paste0(
                   display_names[[input$plot3_x]], ": ", !!sym(input$plot3_x), "<br>",
                   "Average Casualties: ", avg_casualties_per_crash
                 ))) +
      geom_col(fill = "purple") +
      coord_flip() +
      labs(
        title = paste("Average Casualties per Crash by", display_names[[input$plot3_x]]),
        x = display_names[[input$plot3_x]],
        y = "Avg Casualties per Crash"
      ) +
      theme_minimal()
    
    ggplotly(myplot, tooltip = "text") |>
      layout(
        xaxis = list(title = "Average Casualties per Crash"),
        yaxis = list(title = display_names[[input$plot3_x]])
      )
  })
  
  # -----------------------------------------------
  # Output for second TAB (i.e. map)
  # (adapt code to make your map of crashes)
  # -----------------------------------------------
  output$crash_map <- renderLeaflet({

      #Coloring
      color_pal = colorFactor(
      palette = 'viridis', 
      domain = map_filtered_crashes()[[input$map_metric]]
    )
    
    leaflet(map_filtered_crashes()) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~POINT_X, 
        lat = ~POINT_Y,
        radius = 5,
        color = ~color_pal(get(input$map_metric)),
        popup = ~paste(
          "Date:", COLLISION_DATE, 
          "<br>", display_names[[input$map_metric]], ":", get(input$map_metric),
          "<br>City:", PO_NAME
        ),
        opacity = 0.7
      ) |>
      addLegend(
        position = "bottomright",
        pal = color_pal,
        values = ~get(input$map_metric),
        title = display_names[[input$map_metric]]
      )
  })
} # closes server

# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)