
library(shiny)
library(tidyverse)

# Read the dataset
places <- read_csv("data/places_county_2025.csv", show_col_types = FALSE)

# Keep only the needed columns
places_small <- places %>%
  select(
    Year,
    StateAbbr,
    StateDesc,
    LocationName,
    Category,
    Measure,
    Data_Value_Type,
    Data_Value,
    Low_Confidence_Limit,
    High_Confidence_Limit,
    Geolocation
  )

# Choose the best 12 measures
chosen_measures <- c(
  "Coronary heart disease among adults",
  "Stroke among adults",
  "Diagnosed diabetes among adults",
  "High blood pressure among adults",
  "Obesity among adults",
  "No leisure-time physical activity among adults",
  "Short sleep duration among adults",
  "High cholesterol among adults who have ever been screened",
  "Food insecurity in the past 12 months among adults",
  "Housing insecurity in the past 12 months among adults",
  "Lack of reliable transportation in the past 12 months among adults",
  "Lack of social and emotional support among adults"
)

# Keep only chosen measures and age-adjusted prevalence
places_best <- places_small %>%
  filter(
    Measure %in% chosen_measures,
    Data_Value_Type == "Age-adjusted prevalence"
  )

ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  titlePanel("County Health Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Filters"),
      selectInput(
        inputId = "year_filter",
        label = "Select Year",
        choices = 2023,
        selected = 2023
      ),
      selectInput(
        inputId = "state_filter",
        label = "Select State",
        choices = c("All States", sort(unique(places_best$StateDesc))),
        selected = "All States"
      ),
      p("This dashboard shows county-level age-adjusted public health measures."),
      
      conditionalPanel(
        condition = "input.main_tabs == 'obesity'",
        tags$hr(),
        h4("Top 10 obesity ranking"),
        tags$div(
          style = "font-size: 12px;",
          tableOutput("obesity_table")
        )
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'diabetes'",
        tags$hr(),
        h4("Top 10 diabetes ranking"),
        tags$div(
          style = "font-size: 12px;",
          tableOutput("diabetes_table")
        )
      )
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          title = "Obesity",
          value = "obesity",
          plotOutput("obesity_plot", height = "650px")
        ),
        
        tabPanel(
          title = "Diabetes",
          value = "diabetes",
          plotOutput("diabetes_plot", height = "650px")
        ),
        
        tabPanel(
          title = "Relationship",
          value = "relationship",
          plotOutput("relationship_plot", height = "650px")
        ),
        
        tabPanel(
          title = "About",
          value = "about",
          h3("About this dashboard"),
          p("This dashboard uses county-level CDC PLACES data to explore age-adjusted public health measures."),
          tags$ul(
            tags$li("Focus measures: obesity, diagnosed diabetes, and their county-level relationship."),
            tags$li("Year currently supported in this dashboard: 2023."),
            tags$li("Age-adjusted prevalence allows fairer comparison across places with different age structures."),
            tags$li("The relationship chart shows association, not proof of causation.")
          ),
          h4("How to use"),
          tags$ul(
            tags$li("Use the State filter to view all states or a single state."),
            tags$li("Click the tabs to switch between charts."),
            tags$li("Interpret higher values as higher estimated burden.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$obesity_plot <- renderPlot({
    
    obesity_year <- places_best %>%
      filter(
        Year == input$year_filter,
        Measure == "Obesity among adults"
      )
    
    if (input$state_filter != "All States") {
      obesity_year <- obesity_year %>%
        filter(StateDesc == input$state_filter)
    }
    
    obesity_year <- obesity_year %>%
      mutate(County_State = paste0(LocationName, ", ", StateAbbr)) %>%
      arrange(desc(Data_Value)) %>%
      slice_head(n = 10)
    
    ggplot(obesity_year, aes(x = reorder(County_State, Data_Value), y = Data_Value)) +
      geom_col(fill = "steelblue") +
      geom_text(
        aes(label = sprintf("%.1f", Data_Value)),
        hjust = -0.1,
        size = 4
      ) +
      coord_flip() +
      labs(
        title = ifelse(
          input$state_filter == "All States",
          paste0("Top 10 Counties for Age-Adjusted Adult Obesity Prevalence (", input$year_filter, ")"),
          paste0("Top 10 Counties for Age-Adjusted Adult Obesity Prevalence in ", input$state_filter, " (", input$year_filter, ")")
        ),
        subtitle = "Age-adjusted county-level estimates from the CDC PLACES dataset",
        x = "County, State",
        y = "Age-adjusted obesity prevalence (%)",
        caption = paste0("Source: CDC PLACES county-level dataset, ", input$year_filter)
      ) +
      ylim(0, max(obesity_year$Data_Value) + 3) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 11),
        plot.caption = element_text(size = 10)
      )
  })
  
  output$obesity_table <- renderTable({
    
    obesity_year <- places_best %>%
      filter(
        Year == input$year_filter,
        Measure == "Obesity among adults"
      )
    
    if (input$state_filter != "All States") {
      obesity_year <- obesity_year %>%
        filter(StateDesc == input$state_filter)
    }
    
    obesity_year %>%
      mutate(`County, State` = paste0(LocationName, ", ", StateAbbr)) %>%
      arrange(desc(Data_Value)) %>%
      slice_head(n = 10) %>%
      transmute(
        Rank = row_number(),
        `County, State`,
        `Obesity (%)` = round(Data_Value, 1)
      )
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$diabetes_plot <- renderPlot({
    
    diabetes_year <- places_best %>%
      filter(
        Year == input$year_filter,
        Measure == "Diagnosed diabetes among adults"
      )
    
    if (input$state_filter != "All States") {
      diabetes_year <- diabetes_year %>%
        filter(StateDesc == input$state_filter)
    }
    
    diabetes_year <- diabetes_year %>%
      mutate(County_State = paste0(LocationName, ", ", StateAbbr)) %>%
      arrange(desc(Data_Value)) %>%
      slice_head(n = 10)
    
    ggplot(diabetes_year, aes(x = reorder(County_State, Data_Value), y = Data_Value)) +
      geom_col(fill = "darkorange") +
      geom_text(
        aes(label = sprintf("%.1f", Data_Value)),
        hjust = -0.1,
        size = 4
      ) +
      coord_flip() +
      labs(
        title = ifelse(
          input$state_filter == "All States",
          paste0("Top 10 Counties for Age-Adjusted Diagnosed Diabetes Prevalence (", input$year_filter, ")"),
          paste0("Top 10 Counties for Age-Adjusted Diagnosed Diabetes Prevalence in ", input$state_filter, " (", input$year_filter, ")")
        ),
        subtitle = "Age-adjusted county-level estimates from the CDC PLACES dataset",
        x = "County, State",
        y = "Age-adjusted diagnosed diabetes prevalence (%)",
        caption = paste0("Source: CDC PLACES county-level dataset, ", input$year_filter)
      ) +
      ylim(0, max(diabetes_year$Data_Value) + 3) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 11),
        plot.caption = element_text(size = 10)
      )
  })
  
  output$diabetes_table <- renderTable({
    
    diabetes_year <- places_best %>%
      filter(
        Year == input$year_filter,
        Measure == "Diagnosed diabetes among adults"
      )
    
    if (input$state_filter != "All States") {
      diabetes_year <- diabetes_year %>%
        filter(StateDesc == input$state_filter)
    }
    
    diabetes_year %>%
      mutate(`County, State` = paste0(LocationName, ", ", StateAbbr)) %>%
      arrange(desc(Data_Value)) %>%
      slice_head(n = 10) %>%
      transmute(
        Rank = row_number(),
        `County, State`,
        `Diabetes (%)` = round(Data_Value, 1)
      )
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$relationship_plot <- renderPlot({
    
    obesity_diabetes_year <- places_best %>%
      filter(
        Year == input$year_filter,
        Measure %in% c("Obesity among adults", "Diagnosed diabetes among adults")
      )
    
    if (input$state_filter != "All States") {
      obesity_diabetes_year <- obesity_diabetes_year %>%
        filter(StateDesc == input$state_filter)
    }
    
    obesity_diabetes_year <- obesity_diabetes_year %>%
      group_by(StateAbbr, StateDesc, LocationName, Measure) %>%
      summarise(Data_Value = mean(Data_Value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = Measure,
        values_from = Data_Value
      ) %>%
      drop_na()
    
    correlation_value <- cor(
      obesity_diabetes_year$`Obesity among adults`,
      obesity_diabetes_year$`Diagnosed diabetes among adults`,
      use = "complete.obs"
    )
    
    ggplot(
      obesity_diabetes_year,
      aes(
        x = `Obesity among adults`,
        y = `Diagnosed diabetes among adults`
      )
    ) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
      labs(
        title = ifelse(
          input$state_filter == "All States",
          paste0("Relationship Between Age-Adjusted Obesity and Diagnosed Diabetes (", input$year_filter, ")"),
          paste0("Relationship Between Age-Adjusted Obesity and Diagnosed Diabetes in ", input$state_filter, " (", input$year_filter, ")")
        ),
        subtitle = paste0(
          "Each point represents one county from the CDC PLACES dataset (r = ",
          round(correlation_value, 3),
          ")"
        ),
        x = "Age-adjusted obesity prevalence (%)",
        y = "Age-adjusted diagnosed diabetes prevalence (%)",
        caption = paste0("Source: CDC PLACES county-level dataset, ", input$year_filter)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 11),
        plot.caption = element_text(size = 10)
      )
  })
}

shinyApp(ui = ui, server = server)










