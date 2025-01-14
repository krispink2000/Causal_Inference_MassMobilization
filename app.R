library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(maps)

# Load the dataset
data <- read.csv("/Users/krispinkruger/Krueger/Protets/MassMobilizationProtestData.csv")
data$participants <- as.numeric(data$participants)
data$country[data$country == "UK"] <- "United Kingdom"

# Load world map data and adjust region names
world_map <- map_data("world")
world_map$region[world_map$region == "UK"] <- "United Kingdom"

# Define UI
ui <- fluidPage(
  titlePanel("Protests Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Global Filters"),
      sliderInput("yearRange", "Select Year Range", 
                  min = min(data$year), 
                  max = max(data$year), 
                  value = c(min(data$year), max(data$year)), 
                  step = 1),
      checkboxInput("toggleParticipants", "Include Participants", value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Heatmap", plotlyOutput("heatmapPlot")),
        tabPanel("Protests by Year (Stacked)", plotlyOutput("protestsByYearStacked")),
        tabPanel("Protests by Year (Dodge)", plotlyOutput("protestsByYear")),
        tabPanel("Top Countries", plotlyOutput("topCountriesPlot")),
        tabPanel("Bottom Countries", plotlyOutput("bottomCountriesPlot"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  # Reactive filtered dataset based on user input
  filtered_data <- reactive({
    data %>% filter(year >= input$yearRange[1], year <= input$yearRange[2])
  })
  
  # Heatmap plot
  output$heatmapPlot <- renderPlotly({
    filtered_country_data <- filtered_data() %>%
      group_by(country) %>%
      summarise(Total_Protests = n(), .groups = "drop")
    
    # Merge with map data
    merged_world_data <- world_map %>%
      left_join(filtered_country_data, by = c("region" = "country"))
    
    # Ensure Total_Protests is numeric and replace NA with 0
    merged_world_data$Total_Protests[is.na(merged_world_data$Total_Protests)] <- 0
    
    ggplot_obj <- ggplot(merged_world_data, aes(x = long, y = lat, group = group, fill = Total_Protests)) +
      geom_polygon(color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "red", na.value = "gray90") +
      labs(title = "World Heatmap of Protests", fill = "Number of Protests") +
      theme_void() +
      theme(legend.position = "bottom")
    
    ggplotly(ggplot_obj)
  })
  
  # Stacked bar chart for Protests by Year
  output$protestsByYearStacked <- renderPlotly({
    stacked_data <- filtered_data() %>%
      group_by(year, region) %>%
      summarise(Total_Protests = n(), .groups = "drop")
    
    ggplot_obj <- ggplot(stacked_data, aes(x = year, y = Total_Protests, fill = region)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = "Protests by Year (Stacked by Region)",
        x = "Year",
        y = "Number of Protests",
        fill = "Region"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(ggplot_obj)
  })
  
  # Protests by Year (dodge-style bar chart)
  output$protestsByYear <- renderPlotly({
    yearly_data <- filtered_data() %>%
      group_by(year, region) %>%
      summarise(
        Total_Protests = n(),
        Total_Participants = sum(participants, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot_obj <- ggplot(yearly_data, aes(x = as.factor(year), y = Total_Protests, fill = region)) +
      geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
      labs(
        title = "Protests by Year and Region",
        x = "Year",
        y = "Number of Protests",
        fill = "Region"
      ) +
      theme_minimal()
    
    if (input$toggleParticipants) {
      ggplot_obj <- ggplot_obj +
        geom_line(aes(y = Total_Participants / 10000, group = region, color = region), size = 1.2) +
        geom_point(aes(y = Total_Participants / 10000, color = region), size = 3) +
        scale_y_continuous(
          sec.axis = sec_axis(~ . * 10000, name = "Participants (in tens of thousands)")
        )
    }
    
    ggplotly(ggplot_obj)
  })
  
  # Top countries plot
  output$topCountriesPlot <- renderPlotly({
    filtered_country_data <- filtered_data() %>%
      group_by(country, region) %>%
      summarise(
        Total_Protests = n(),
        Total_Participants = sum(participants, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Protests)) %>%
      head(15)
    
    plot_ly(
      data = filtered_country_data,
      x = ~Total_Protests,
      y = ~reorder(country, Total_Protests),
      type = "bar",
      orientation = "h",
      text = ~paste(
        "Country:", country,
        "<br>Region:", region,
        "<br>Total Protests:", Total_Protests,
        if (input$toggleParticipants) paste("<br>Participants:", Total_Participants) else ""
      ),
      hoverinfo = "text",
      marker = list(color = "blue")
    ) %>%
      layout(
        title = "Top 10 Countries with Most Protests",
        xaxis = list(title = "Number of Protests"),
        yaxis = list(title = "Country", tickfont = list(size = 12)),
        margin = list(l = 150)
      )
  })
  
  # Bottom countries plot
  output$bottomCountriesPlot <- renderPlotly({
    filtered_country_data <- filtered_data() %>%
      group_by(country, region) %>%
      summarise(
        Total_Protests = n(),
        Total_Participants = sum(participants, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Total_Protests) %>%
      head(15)
    
    plot_ly(
      data = filtered_country_data,
      x = ~Total_Protests,
      y = ~reorder(country, Total_Protests),
      type = "bar",
      orientation = "h",
      text = ~paste(
        "Country:", country,
        "<br>Region:", region,
        "<br>Total Protests:", Total_Protests,
        if (input$toggleParticipants) paste("<br>Participants:", Total_Participants) else ""
      ),
      hoverinfo = "text",
      marker = list(color = "orange")
    ) %>%
      layout(
        title = "Top 10 Countries with Least Protests",
        xaxis = list(title = "Number of Protests"),
        yaxis = list(title = "Country", tickfont = list(size = 12)),
        margin = list(l = 150)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)