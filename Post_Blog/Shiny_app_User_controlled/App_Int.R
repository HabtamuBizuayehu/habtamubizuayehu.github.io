
#-------------------------- Shiny app by Habtamu Bizuayehu-------------------------------------------------------------------------
#...................................................................................................................................


# clear environment
# rm(list=ls())
# 
# # clear console
# cat("\014")



# The script sets up a Shiny web application with interactive UI elements, tables, plots, and enhanced theming options.

#...................................................................................................

# Define required packages
# Please not you may not required all these packages unless you want to some data manipulation and cleaning 
# packages <- c("shiny", "DT", "plotly", "shinythemes", "shinyWidgets", 
#               "shinydashboard", "colourpicker", "readr", "readxl", 
#               "purrr", "stringr", "lubridate", "tidyr", "haven", "tidyverse", "dplyr", "rsconnect")
# 
# # Install any missing packages
# missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]
# if (length(missing_packages) > 0) {
#   install.packages(missing_packages)
# }

# Load required libraries with descriptions
library(shiny) # Core package for building interactive web applications in R
library(DT) # Provides interactive tables using DataTables in Shiny applications
library(plotly) # Enables the creation of interactive and web-based plots in R
library(shinythemes) # Provides pre-built themes for customizing the appearance of Shiny applications
library(shinyWidgets) # Adds additional UI widgets and controls to enhance Shiny applications
library(shinydashboard) # Allows the creation of dashboard-style layouts for Shiny applications
library(colourpicker) # Adds a color picker input to the UI, enabling users to select colors
library(readr) # Provides functions for reading and writing tabular data efficiently
library(readxl) # A package for reading Excel files into R, handling both .xls and .xlsx formats
library(purrr) # Provides functional programming tools for iteration and manipulation of lists and vectors
library(stringr) # Simplifies string manipulation in R, offering consistent and easy-to-use functions
library(lubridate) # Simplifies the manipulation of dates and times in R
library(tidyr) # Helps in reshaping and tidying up data for analysis and visualization
library(haven) # Facilitates reading and writing data in formats used by other statistical software, such as SPSS, SAS, and Stata
library(tidyverse) # A collection of R packages for data manipulation, visualization, and analysis, includes ggplot2, dplyr, tidyr, readr, and more
library(dplyr) # Provides tools for data manipulation and transformation, especially for working with data frames
library(rsconnect) # to deploy the app

# shinyWidgetsGallery() # opens a pre-built app that allows users to explore available widgets and view implementation code.


# # URL to the raw Excel file on GitHub
# url <- "https://raw.githubusercontent.com/HabtamuBizuayehu/Data-sources/main/Breast_cancer_screening.xls"
# 
# # Download the file to a temporary location
# temp_file <- tempfile(fileext = ".xls")
# download.file(url, temp_file, mode = "wb")

# Set working directory (only for local development)
setwd("C:/Users/User/Desktop/Materials_ Course and proposals/VIP collection for Syntax code or project titles/R codes/Shiny app_for Breast Cancer Screening/")

# Read the Excel file - you need to put the filename in quotes
breast_cancer_data <- read_excel("Breast_cancer_screening.xls")

# Check the first few rows of the data
head(breast_cancer_data)

table (breast_cancer_data$year)

# Modify the 'year' variable based on the provided values 
breast_cancer_data <- breast_cancer_data %>%
  mutate(year = case_when(
    year == 201617 ~ 2017,
    year == 201718 ~ 2018,
    year == 201819 ~ 2019,
    year == 201920 ~ 2020,
    TRUE ~ year # Retain original year for any other values
  ))

# Check the updated 'year' variable
table(breast_cancer_data$year)
colnames(breast_cancer_data)

# # Drop all columns that start with "agen" and "sa3coden"
breast_cancer_data <- breast_cancer_data %>%
  select(-sa3coden, -starts_with("agen"))


# rearranging the data to provide the planned outconme for shiny app
breast_cancer_long <- breast_cancer_data %>%
  pivot_longer(
    cols = starts_with("part") | starts_with("pop") | starts_with("rate") | starts_with("expct"),
    names_to = c("measure", "age_group"),
    names_pattern = "(part|pop|rate|expct)([0-9]{2}_[0-9]{2})", 
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>%
  select(sa3code, sa3name, state, year, age_group, part, pop, rate, expct)

# Check data
head(breast_cancer_long)

breast_cancer_long <- breast_cancer_long %>%
  mutate(
    rate = round(rate, 1),  # Round rate to one decimal place
    rate_10_group = ntile(rate, 10)  # Divide into 10 quantile-based groups
  )

# exclude missing values for state

table(breast_cancer_long$state)

breast_cancer_long <- breast_cancer_long %>%
  filter(!(state %in% c("Unknown", "Other")))

# Check the transformed data
table(breast_cancer_long$rate_10_group)

# Filrer the data ie removing unknown and other states 


# ------------------- Advanced shiny Application features ---------------------

# lets include more functions such as title, customised themes, colour selection, help and download data
#  Also adding User controlled outputs: to Stop - delay - trigger ie reactive when the user clicks on "show result input"
# State presented in Drop down option rather than Radio buttons


# Define custom CSS
ui <- fluidPage(
  h1("Breast Cancer Screening Uptake"),
  tags$style(HTML("
    #download_data {
      background: orange;
      font-size: 20px;
    }
  ")),  # Apply CSS
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("rate_10_group", "Rate Group", 
                  min = min(breast_cancer_long$rate_10_group, na.rm = TRUE), 
                  max = max(breast_cancer_long$rate_10_group, na.rm = TRUE), 
                  value = c(5, 7)),
      
      selectInput("state", "Select State",  # Change from radioButtons to selectInput
                  choices = unique(breast_cancer_long$state), 
                  selected = "WA"),
      
      selectInput("age_group", "Women's Age Group", 
                  choices = unique(breast_cancer_long$age_group), 
                  multiple = TRUE,
                  selected = unique(breast_cancer_long$age_group)[1]),
      
      colourInput("color", "Point color", value = "blue"),  # Add color input
      
      actionButton("show_help", "Help"),
      downloadButton("download_data", "Download Data")
    ),
    
    mainPanel(
      plotlyOutput("plot"),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # Show Help Modal
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Help",
      "This data was compiled from the Australian Institute of Health and Welfare."
    ))
  })
  
  # Filtered Data (no need for eventReactive, just reactive)
  filtered_data <- reactive({
    req(input$rate_10_group, input$state, input$age_group)
    
    breast_cancer_long %>%
      filter(rate_10_group >= input$rate_10_group[1] & 
               rate_10_group <= input$rate_10_group[2],
             state == input$state,
             age_group %in% input$age_group) %>%
      group_by(year, state, age_group) %>%
      summarise(year = as.integer(year),  # Ensure year is an integer
                rate = round(mean(rate, na.rm = TRUE), 1), # round mean value to 1 digit
                .groups = "drop") 
  })
  
  # Render Table
  output$table <- renderDT({
    datatable(filtered_data(), 
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',  # Define position of buttons (Buttons above the table)
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Buttons for export
                class = "display nowrap compact"
              ),
              extensions = 'Buttons',  # Enable buttons extension
              style = "bootstrap", 
              class = "table table-striped table-bordered") %>%
      formatStyle(columns = names(filtered_data()), color = input$color)
  })
  
  # Download Data
  output$download_data <- downloadHandler(
    filename = "Breast_cancer_screening_data.csv",
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Render Plot
  output$plot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = year, y = rate, color = age_group, group = interaction(state, age_group))) +
      geom_line(size = 1) +  # Remove input$color from here
      geom_point(size = 3) +  # Remove input$color from here
      labs(title = "Breast Cancer Screening Uptake", 
           x = "Year", 
           y = "Screening Rate",
           color = "Age Group") +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui, server)


### 8.1 **UI**
# The UI defines the layout and user interface components of the Shiny app.
# It includes the app title, sidebar for inputs, and main panel for outputs.
ui <- fluidPage(

  # Title of the app
  h1("Breast Cancer Screening Uptake"),

  # Custom CSS for the download button to improve visibility
  tags$style(HTML("
    #download_data {
      background: orange;
      font-size: 20px;
    }
  ")),

  # Use sidebar layout with sidebarPanel for inputs and mainPanel for outputs
  sidebarLayout(
    sidebarPanel(
      # Slider for selecting a quantile-based rate group range
      sliderInput("rate_10_group", "Rate Group", 
                  min = min(breast_cancer_long$rate_10_group, na.rm = TRUE), 
                  max = max(breast_cancer_long$rate_10_group, na.rm = TRUE), 
                  value = c(5, 7)),

      # Dropdown to select the state (replaces radioButtons for better scalability)
      selectInput("state", "Select State",  
                  choices = unique(breast_cancer_long$state), 
                  selected = "WA"),

      # Multiple selection for age groups
      selectInput("age_group", "Women's Age Group", 
                  choices = unique(breast_cancer_long$age_group), 
                  multiple = TRUE,
                  selected = unique(breast_cancer_long$age_group)[1]),

      # Color picker to allow customization of table font color
      colourInput("color", "Point color", value = "blue"),

      # Button to show a help popup
      actionButton("show_help", "Help"),

      # Button to download the currently filtered dataset
      downloadButton("download_data", "Download Data")
    ),

    mainPanel(
      # Output area for the interactive plot
      plotlyOutput("plot"),
      # Output area for the interactive data table
      DTOutput("table")
    )
  )
)


### 8.2 **Server**
# The server logic contains reactivity, rendering, and event handling
server <- function(input, output, session) {

  # Display a modal with help information when the help button is clicked
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Help",
      "This data was compiled from the Australian Institute of Health and Welfare."
    ))
  })

  # Reactive expression that filters the dataset based on user input
  filtered_data <- reactive({
    req(input$rate_10_group, input$state, input$age_group)  # Ensure inputs are available

    # Filter and aggregate the dataset by year, state, and age group
    breast_cancer_long %>%
      filter(rate_10_group >= input$rate_10_group[1] & 
               rate_10_group <= input$rate_10_group[2],
             state == input$state,
             age_group %in% input$age_group) %>%
      group_by(year, state, age_group) %>%
      summarise(
        year = as.integer(year),
        rate = round(mean(rate, na.rm = TRUE), 1),
        .groups = "drop"
      )
  })

  # Render the filtered dataset as an interactive datatable
  output$table <- renderDT({
    datatable(filtered_data(), 
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',  # Button layout
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                class = "display nowrap compact"
              ),
              extensions = 'Buttons',
              style = "bootstrap", 
              class = "table table-striped table-bordered") %>%
      formatStyle(columns = names(filtered_data()), color = input$color)
  })

  # Allow user to download the filtered data as a CSV file
  output$download_data <- downloadHandler(
    filename = "Breast_cancer_screening_data.csv",
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  # Render an interactive line plot showing screening trends
  output$plot <- renderPlotly({
    df <- filtered_data()

    p <- ggplot(df, aes(x = year, y = rate, color = age_group, group = interaction(state, age_group))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(title = "Breast Cancer Screening Uptake", 
           x = "Year", 
           y = "Screening Rate",
           color = "Age Group") +
      theme_minimal()

    ggplotly(p)  # Convert static ggplot to interactive plotly object
  })
}


### 8.3 **App Launch**
# Run the application using ui and server defined above
shinyApp(ui, server)



