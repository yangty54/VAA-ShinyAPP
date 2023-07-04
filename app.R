# Load required packages
pacman::p_load(shiny, shinydashboard, shinythemes, shinydashboardPlus, shinyWidgets, shinybusy, shinyjs, 
               readxl, plotly, tidyverse, networkD3, visNetwork, kableExtra, lubridate, knitr, tidygraph,
               dplyr, jsonlite, ggplot2, igraph, leaflet, forcats)

# Load the MC2 data from the RDS file
MC2 <- readRDS("MC2_data.rds")

MC2_data <- MC2 %>%
  mutate(fishtype = case_when(
    startsWith(hscode, "301") ~ "live fish",
    startsWith(hscode, "302") ~ "fresh fish",
    startsWith(hscode, "303") ~ "frozen fish",
    startsWith(hscode, "304") ~ "fish meat",
    startsWith(hscode, "305") ~ "processed fish",
    startsWith(hscode, "306") ~ "crustaceans",  #like lobster or shrimps
    startsWith(hscode, "307") ~ "molluscs",  #like Oysters or Abalone
    startsWith(hscode, "308") ~ "aquatic invertebrates", #like Sea cucumbers?
    startsWith(hscode, "309") ~ "seafood flours",  #fish powder, shrimp powder?
    TRUE ~ "not fish"
  ))

MC2_data_p2 <- MC2_data%>%
  group_by(year = year(Arrivaldate), month = month(Arrivaldate), fishtype) %>%
  summarise(no_shipment = n()) %>%
  filter(fishtype != "not fish") %>%
  ungroup()

# Aggregating number of routes into a new dataframe
routes_by_year <- MC2_data %>%
  mutate(yearmonth = format(Arrivaldate, "%b")) %>%
  group_by(year = lubridate::year(Arrivaldate), yearmonth) %>%
  summarise(nroutes = n()) %>%
  arrange(year, factor(yearmonth, levels = month.abb)) %>%
  ungroup()

# Read data for network plot 1
edges <- read_excel("edges.xlsx")
nodes <- read_excel("nodes.xlsx")

# Read data for network plot 2
country_edges <- read_excel("country_edges_2.xlsx")
country_nodes <- read_excel("country_nodes_2.xlsx")


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = span(tagList(icon("fish"), span("Illegal Fishing Analysis", style = "font-size: 16px")))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "tab_home", icon = icon("home")),
      
      menuItem("General", tabName = "tab_summary", icon = icon("anchor"),
               menuSubItem("By Fishtype", tabName = "tab_fishtype", icon = icon("bar-chart")),
               menuSubItem("Shipment/month by fishtype", tabName = "tab_shipment_by_fishtype", icon = icon("line-chart")),
               menuSubItem("Value of Goods in Time", tabName = "tab_value_of_goods", icon = icon("line-chart")),
               menuSubItem("Weight vs Value of Goods", tabName = "tab_weight_vs_value", icon = icon("area-chart"))
      ),
      
      menuItem("Network", tabName = "tab_network", icon = icon("link"),
               menuSubItem("Shipment Network", tabName = "tab_network1", icon = icon("ship")),
               menuSubItem("Country Network", tabName = "tab_network2", icon = icon("globe"))
      ),
      
      menuItem("Anomaly", tabName = "tab_anomaly", icon = icon("chart-line"),
               menuSubItem("# of Traderoutes", tabName = "tab_traderoutes", icon = icon("line-chart")),
               menuSubItem("Value in Time", tabName = "tab_anomaly2", icon = icon("line-chart")),
               menuSubItem("Weight vs Value Ratio", tabName = "tab_anomaly3", icon = icon("arrows-h"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab_home",
        div(
          class = "home-content",
          h2("Welcome to the Illegal Fishing Analysis Dashboard"),
          p(
            class = "dashboard-description",
            "The global issue of illegal, unreported, and unregulated (IUU) fishing poses a significant threat to marine ecosystems and sustainable fishing practices."
            , style = "font-size: 20px"
          ),
          p("Please explore the different tabs to analyze various aspects related to illegal fishing."
            , style = "font-size: 20px"),
          leafletOutput("map"),
          div(
            class = "icon-container",
            icon("info-circle", class = "custom-icon"),
            span(
              style = "font-size: 14px; color: #555555;",
              "For more information, hover over the icon."
            )
          )
        )
      ),
      
      tabItem(
        tabName = "tab_summary",
        h2("Summary"),
        p("This tab provides a summary of the illegal fishing analysis."),
        # Add content specific to the summary tab
      ),
      
      tabItem(
        tabName = "tab_fishtype",
        fluidRow(
          column(
            width = 3,
            accordion(
              id = "parameters_fishtype",
              accordionItem(
                title = span("Filters", style = "font-size: 14px; font-weight: bold"),
                status = "primary",
                collapsed = FALSE,
                selectInput(
                  "fish_type_select",
                  "Select Fish Type:",
                  choices = unique(MC2_data$fishtype),
                  selected = NULL,
                  multiple = TRUE
                )
              )
            )
          ),
          column(width = 8, plotlyOutput("plot1"))
        )
      ),
      
      tabItem(
        tabName = "tab_shipment_by_fishtype",
        fluidRow(
          column(
            width = 3,
            accordion(
              id = "parameters_shipment_by_fishtype",
              accordionItem(
                title = span("Filters", style = "font-size: 14px; font-weight: bold"),
                status = "primary",
                collapsed = FALSE,
                checkboxGroupInput(
                  "fish_type_select_p3",
                  "Select Fish Type:",
                  choices = unique(MC2_data$fishtype),
                  selected = NULL
                )
              )
            )
          )
        ),
        column(width = 8, plotlyOutput("plot2"))
      ),
      
      tabItem(
        tabName = "tab_value_of_goods",
        fluidRow(
          column(
            width = 3,
            accordion(
              id = "parameters_value_of_goods",
              accordionItem(
                title = span("Filters", style = "font-size: 14px; font-weight: bold"),
                status = "primary",
                collapsed = FALSE,
                sliderInput(
                  "time_range",
                  "Select Time Range:",
                  min = min(MC2_data$Arrivaldate),
                  max = max(MC2_data$Arrivaldate),
                  value = c(min(MC2_data$Arrivaldate), max(MC2_data$Arrivaldate)),
                  timeFormat = "%Y-%m",
                  ticks = TRUE
                )
              )
            )
          )
        ),
        column(width = 8, plotlyOutput("plot3"))
      ),
      
      tabItem(
        tabName = "tab_weight_vs_value",
        fluidRow(
          column(
            width = 3,
            accordion(
              id = "parameters_weight_vs_value",
              accordionItem(
                title = span("Filters", style = "font-size: 14px; font-weight: bold"),
                status = "primary",
                collapsed = FALSE,
                selectInput("yearInput", "Select Year", choices = unique(format(MC2_data$Arrivaldate, "%Y")))
              )
            )
          )
        ),
        plotlyOutput("plot4", height = "600px", width = "800px")
      ),
      
      tabItem(
        tabName = "tab_network1",
        fluidRow(
          column(
            width = 3,
            accordion(
              id = "parameters_network1",
              accordionItem(
                title = span("Filters", style = "font-size: 14px; font-weight: bold"),
                status = "primary",
                collapsed = FALSE,
                selectInput("year", "Select Year", choices = unique(edges$Year)),
                radioButtons(
                  "centralityMeasure",
                  "Centrality Measure:",
                  choices = c("betweenness", "degree"),
                  selected = "betweenness"
                ),
                sliderInput(
                  "centralityPercentile",
                  "Centrality Percentile:",
                  min = 0, max = 100, value = c(0, 100), step = 20
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            visNetworkOutput("network", height = "400px")
          )
        )
      ),
      
      tabItem(
        tabName = "tab_network2",
        fluidRow(
          column(width = 4,
                 accordion(
                   id = "parameters_network2",
                   accordionItem(
                     title = span("Filters", style = "font-size: 14px; font-weight: bold"),
                     status = "primary",
                     collapsed = FALSE,
                     sliderInput("time_range_network2", "Select Time Range:",
                                 min = min(country_edges$ArrivalDate),
                                 max = max(country_edges$ArrivalDate),
                                 value = c(min(country_edges$ArrivalDate), max(country_edges$ArrivalDate)),
                                 step = 1,
                                 timeFormat = "%Y-%m",
                                 ticks = TRUE
                     ),
                   )
                 )
          ),
          fluidRow(column(width = 8, visNetworkOutput("networkGraph"))
          )
        )
      ),
      
      tabItem(
        tabName = "tab_traderoutes",
        fluidRow(
          column(
            width = 3,
            accordion(
              id = "parameters_traderoutes",
              accordionItem(
                title = span("Filters", style = "font-size: 14px; font-weight: bold"),
                status = "primary",
                collapsed = FALSE,
                checkboxGroupInput(
                  "selected_years",
                  "Select Years:",
                  choices = unique(routes_by_year$year),
                  selected = unique(routes_by_year$year),
                  inline = TRUE
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            column(width = 8, plotlyOutput("plot6"))
          )
        )
      ),
      
      tabItem(
        tabName = "tab_anomaly2",
        fluidRow(
          column(
            width = 3,
            accordion(
              id = "parameters_anomaly2",
              accordionItem(
                title = span("Filters", style = "font-size: 14px; font-weight: bold"),
                status = "primary",
                collapsed = FALSE,
                sliderInput(
                  "time_range_plot7",
                  "Select Time Range:",
                  min = min(MC2_data$Arrivaldate),
                  max = max(MC2_data$Arrivaldate),
                  value = c(min(MC2_data$Arrivaldate), max(MC2_data$Arrivaldate)),
                  timeFormat = "%Y-%m",
                  ticks = TRUE
                )
              )
            )
          )
        ),
        column(width = 9, plotlyOutput("plot7"))
      ),
      
      tabItem(
        tabName = "tab_anomaly3",
        fluidRow(
          column(
            width = 3,
            accordion(
              id = "parameters_anomaly3",
              accordionItem(
                title = span("Filters", style = "font-size: 14px; font-weight: bold"),
                status = "primary",
                collapsed = FALSE,
                sliderInput(
                  "top_dots_plot8",
                  "Select Number of Top Dots:",
                  min = 1,
                  max = 500,
                  value = 100,
                  step = 1
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotlyOutput("plot8")
          )
        )
      )
    )
  )
)


# Define server
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)  # Set initial view of the map
    # Customize the map with additional layers, markers, etc.
    # For example, you can add markers to highlight specific locations related to illegal fishing
  })
  output$plot1 <- renderPlotly({
    MC2_data_p1 <- MC2_data %>%
      filter(fishtype %in% input$fish_type_select) %>%
      count(fishtype) %>%
      arrange(n) %>%
      mutate(fishtype = factor(fishtype, levels = unique(fishtype)))
    
    plot_ly(
      data = MC2_data_p1,
      x = ~reorder(fishtype, n),
      y = ~n,
      type = "bar",
      marker = list(color = '#808de8'),
      text = ~n,
      hovertemplate = "%{y}",
      textposition = "auto"
    ) %>%
      layout(
        title = list(text = "Distribution of Source Nodes by Fish Type", x = 0.5),
        yaxis = list(title = "No. of Companies"),
        xaxis = list(title = "Fish Type", tickangle = -90),
        showlegend = FALSE
      )
  })
  
  
  # Render Plot 2: Total shipment per month by fish type
  output$plot2 <- renderPlotly({
    # Filter data based on selected fish types
    MC2_data_p2 <- MC2_data %>%
      filter(fishtype %in% input$fish_type_select_p3) %>%
      group_by(year = year(Arrivaldate), month = month(Arrivaldate), fishtype) %>%
      summarise(no_shipment = n()) %>%
      filter(fishtype != "not fish") %>%
      ungroup()
    
    # Create the plot using plot_ly
    plot_ly(
      data = MC2_data_p2,
      x = ~(year - 2028) * 12 + month,
      y = ~no_shipment,
      color = ~fishtype,
      colors = "Dark2",
      type = "scatter",
      mode = "lines+markers",
      group = ~year,
      text = ~paste("Year:", year, "<br>Month:", month, "<br>No. of Shipment:", no_shipment),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Total Shipment per Month by Fish Type, 2028 - 2034",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Num of Shipment"),
        showlegend = TRUE
      )
  })
  
  # Render Plot 3
  output$plot3 <- renderPlotly({
    # Filter the data based on the selected time range
    MC2_data_filtered <- MC2_data %>%
      filter(Arrivaldate >= input$time_range[1] & Arrivaldate <= input$time_range[2])
    
    # Calculate the monthly sum of valueofgoodsusd for the filtered data
    MC2_monthly_sum <- MC2_data_filtered %>%
      mutate(Month = floor_date(Arrivaldate, "month")) %>%
      group_by(Month) %>%
      summarize(total_value = sum(valueofgoodsusd, na.rm = TRUE))
    
    # Generate the time-series plot using Plotly
    MC2_p3 <- plot_ly(MC2_monthly_sum, x = ~Month, y = ~total_value, type = "scatter", mode = "lines") %>%
      layout(
        xaxis = list(
          title = "Month and Year",
          tickformat = "%b<br>%Y",
          tickangle = -45,
          tickfont = list(size = 10)
        ),
        yaxis = list(title = "Sum of Value of Goods in USD"),
        title = "Time-series Plot of Value of Goods"
      )
  })
  

  output$plot4 <- renderPlotly({
    # Filter data based on selected year
    filtered_data <- MC2_data[format(MC2_data$Arrivaldate, "%Y") == input$yearInput, ]
    
    # Generate the plot using the filtered data
    plot4 <- ggplot(filtered_data, aes(x = weightkg, y = valueofgoodsusd)) +
      geom_hex(bins = 50) +
      scale_x_log10() +
      scale_y_log10() +
      theme_minimal() +
      labs(x = "Weight (kg) [Log Scale]", y = "Value of Goods (USD) [Log Scale]",
           title = "Hexbin Plot of Weight vs Value of Goods (Log Scale)")
    
    ggplotly(plot4)
  })
  
  
  #network1
  filtered_edges1 <- reactive({
    edges %>%
      filter(Year == input$year)
  })
  
  filtered_nodes1 <- reactive({
    if (input$centralityMeasure == "betweenness") {
      nodes %>%
        filter(id %in% unique(c(filtered_edges1()$from, filtered_edges1()$to)) &
                 betweenness_centrality >= quantile(betweenness_centrality, input$centralityPercentile / 100))
    } else if (input$centralityMeasure == "degree") {
      nodes %>%
        filter(id %in% unique(c(filtered_edges1()$from, filtered_edges1()$to)) &
                 degree_centrality >= quantile(degree_centrality, input$centralityPercentile / 100))
    }
  })
  
  output$network <- renderVisNetwork({
    visNetwork(
      nodes = filtered_nodes1(),
      edges = filtered_edges1(),
      main = "Shipment Network",
      width = "100%"
    ) %>%
      visEdges(
        arrows = list(
          to = list(enabled = TRUE),
          from = list(enabled = TRUE)
        ),
        color = list(color = "#444444", highlight = "#A6C4FF"),
        smooth = TRUE
      ) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, labelOnly = TRUE, hover = TRUE)
      ) %>%
      visLegend() %>%
      visInteraction(
        hideEdgesOnDrag = TRUE,
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE,
        navigationButtons = TRUE
      ) %>%
      visNodes(
        color = list(
          background = "#69b3a2",
          border = "#428bca"
        ),
        shadow = list(enabled = TRUE, size = 10)
      )
  })
  
  
  #network2
  filtered_edges2 <- reactive({
    country_edges %>%
      filter(ArrivalDate >= input$time_range_network2[1] & ArrivalDate <= input$time_range_network2[2]) %>%
      group_by(from, to) %>%
      summarise(weight = n())
  })
  
  output$networkGraph <- renderVisNetwork({
    visNetwork(
      nodes = country_nodes,
      edges = filtered_edges2(),
      main = "Country Network",
      width = "100%"
    ) %>%
      visIgraphLayout() %>%
      visLegend() %>%
      visNodes(
        color = list(
          background = "#69b3a2",
          border = "#428bca"
        ),
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(
        arrows = list(
          to = list(enabled = TRUE),
          from = list(enabled = TRUE)
        ),
        color = list(color = "#444444", highlight = "#A6C4FF"),
        smooth = TRUE
      ) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, labelOnly = TRUE, hover = TRUE)
      ) %>%
      visInteraction(
        hideEdgesOnDrag = TRUE,
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE,
        navigationButtons = TRUE
      )
  })
  
  
  
  
  
  # Render the anomaly plot
  output$plot6 <- renderPlotly({
    selected_years <- input$selected_years
    filtered_routes <- routes_by_year[routes_by_year$year %in% selected_years, ]
    
    # Specify the order of the month abbreviations
    filtered_routes$yearmonth <- ordered(filtered_routes$yearmonth, levels = month.abb)
    
    # Find the maximum number of routes for the anomaly
    max_routes <- filtered_routes %>% filter(nroutes == max(nroutes))
    
    # Create the plot
    anomaly_plot <- plot_ly(filtered_routes, x = ~yearmonth, y = ~nroutes, color = ~factor(year),
                            colors = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF"), 
                            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Number of Trade Routes by Year",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Number of Trade Routes")
      )
    
    # Add annotation for the anomaly
    if (!is_empty(max_routes)) {
      anomaly_annotation <- list(
        x = max_routes$yearmonth,
        y = max_routes$nroutes,
        text = "Anomaly",
        showarrow = TRUE,
        arrowhead = 1,
        arrowsize = 1,
        arrowwidth = 2,
        arrowcolor = "red"
      )
      
      anomaly_plot <- anomaly_plot %>% layout(annotations = anomaly_annotation)
    }
    
    anomaly_plot
  })
  
  
  output$plot7 <- renderPlotly({
    MC2_data_filtered <- MC2_data %>%
      filter(Arrivaldate >= input$time_range_plot7[1] & Arrivaldate <= input$time_range_plot7[2])
    
    MC2_monthly_sum <- MC2_data_filtered %>%
      mutate(Month = floor_date(Arrivaldate, "month")) %>%
      group_by(Month) %>%
      summarize(total_value = sum(valueofgoodsusd, na.rm = TRUE))
    
    threshold <- 10e9  # Threshold at 10 billion
    
    # Find the maximum total value
    max_value <- MC2_monthly_sum %>% filter(total_value == max(total_value))
    
    plot_ly(MC2_monthly_sum, x = ~Month, y = ~total_value, type = "scatter", mode = "lines") %>%
      add_trace(y = rep(threshold, nrow(MC2_monthly_sum)),
                name = "Threshold",
                mode = "lines",
                line = list(color = "red", dash = "dash")) %>%
      layout(
        xaxis = list(
          title = "Month and Year",
          tickformat = "%b<br>%Y",
          tickangle = -45,
          tickfont = list(size = 10)
        ),
        yaxis = list(title = "Sum of Value of Goods in USD"),
        title = "Time-series Plot of Value of Goods",
        annotations = list(
          list(
            x = max_value$Month,
            y = max_value$total_value,
            text = "Anomaly",
            showarrow = TRUE,
            arrowhead = 1,
            arrowsize = 1.5,
            arrowwidth = 2,
            arrowcolor = "red"
          )
        )
      )
  })
  
  output$plot8 <- renderPlotly({
    # Filter the data to remove rows with weightkg or valueofgoodsusd equal to 0
    MC2_data_filtered <- MC2_data[MC2_data$weightkg != 0 & MC2_data$valueofgoodsusd != 0, ]
    
    # Fit a linear model
    lm_model <- lm(valueofgoodsusd ~ weightkg, data = MC2_data_filtered)
    
    # Calculate the weight-to-value ratio
    MC2_data_filtered$weight_value_ratio <- MC2_data_filtered$valueofgoodsusd / MC2_data_filtered$weightkg
    
    # Rank the top dots with the highest ratio
    top_dots_ratio <- head(MC2_data_filtered[order(-MC2_data_filtered$weight_value_ratio), ], input$top_dots_plot8)
    
    # Create a column to indicate if a dot is an anomaly
    top_dots_ratio <- top_dots_ratio %>%
      mutate(anomaly = ifelse(row_number() <= 10, "Top Abnormal", "The Rest"))
    
    # Plot the scatter plot with the dots and linear model
    MC2_plot8 <- ggplot(top_dots_ratio, aes(x = weightkg, y = valueofgoodsusd)) +
      geom_point(aes(size = weight_value_ratio, alpha = weight_value_ratio, color = anomaly), shape = 16) +
      scale_x_log10() +
      scale_y_log10() +
      theme_minimal() +
      labs(x = "Weight (kg) [Log Scale]", y = "Value of Goods (USD) [Log Scale]",
           title = "Scatter Plot of Weight vs Value of Goods (Log Scale)") +
      scale_alpha_continuous(range = c(0.1, 1)) +
      scale_size_continuous(range = c(1, 10)) +
      geom_smooth(method = "lm", formula = y ~ x, color = "red") +
      scale_color_manual(values = c("blue", "black"),
                         labels = c("Normal", "Anomaly"),
                         guide = guide_legend(title = "Legend")) +
      guides(size = FALSE, alpha = FALSE)
    
    ggplotly(MC2_plot8)
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
