# ---- LIBRARIES ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(usmap)

# ---- MOCK DATA ----
set.seed(123)
states <- state.name
segments <- c("Consumer", "Corporate", "Home Office")
categories <- c("Technology", "Office Supplies", "Furniture")
sub_categories <- c("Phones", "Chairs", "Binders", "Storage", "Copiers")
manufacturers <- c("Canon", "Global", "Hon", "GBC", "Other")
customers <- paste("Customer", 1:10)

df <- data.frame(
  State = sample(states, 500, replace = TRUE),
  Segment = sample(segments, 500, replace = TRUE),
  Category = sample(categories, 500, replace = TRUE),
  Sub_Category = sample(sub_categories, 500, replace = TRUE),
  Manufacturer = sample(manufacturers, 500, replace = TRUE),
  Customer = sample(customers, 500, replace = TRUE),
  Sales = runif(500, 100, 1000),
  Profit = runif(500, 10, 200),
  Order_ID = sample(1000:2000, 500, replace = TRUE),
  Date = sample(seq(as.Date("2022-01-01"), as.Date("2022-12-30"), by = "day"), 500, replace = TRUE)
)

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(
    title = "Overview Dashboard",
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      dateRangeInput("date_range", "Date Range:",
                     start = min(df$Date), end = max(df$Date)),
      selectInput("segment", "Segment:", choices = c("All", unique(df$Segment))),
      selectInput("category", "Category:", choices = c("All", unique(df$Category)))
    )
  ),
  dashboardBody(
    # Custom CSS
    tags$head(tags$style(HTML('
      .content-wrapper {
        background: linear-gradient(to bottom right, pink, lightblue);
      }
      .main-header .logo {
        background-color: #f7c6d9 !important;
        color: #000 !important;
        font-weight: bold;
      }
      .main-header .navbar {
        background-color: #f7c6d9 !important;
      }
      .main-sidebar {
        background-color: #ffe6f0 !important;
      }
      .main-title {
        font-size: 36px;
        font-weight: bold;
        text-align: center;
        margin-top: 20px;
      }
      .subtitle {
        font-size: 20px;
        text-align: center;
        margin-bottom: 30px;
        color: #444;
      }
      .sidebar .form-group > label {
        color: black !important;
        font-weight: bold;
      }
    '))),
    
    div(class = "main-title", "SALES PERFOMANCE ACROSS THE US"),
    div(class = "subtitle", "by Blondie Ndebele and Zemen Ghechew"),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("customersBox"),
                valueBoxOutput("ordersBox"),
                valueBoxOutput("salesBox"),
                valueBoxOutput("profitBox")
              ),
              fluidRow(
                box(title = "Sales by State", width = 6, plotOutput("stateMap")),
                box(title = "Sales by Segment", width = 6, plotlyOutput("segmentBar"))
              ),
              fluidRow(
                box(title = "Sales by Category", width = 4, plotlyOutput("categoryBar")),
                box(title = "Sales by Manufacturer", width = 4, plotlyOutput("manufacturerBar")),
                box(title = "Sales by Customer", width = 4, plotlyOutput("customerBar"))
              )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- df %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    if (input$segment != "All") {
      data <- data %>% filter(Segment == input$segment)
    }
    
    if (input$category != "All") {
      data <- data %>% filter(Category == input$category)
    }
    
    data
  })
  
  # KPI Boxes
  output$customersBox <- renderValueBox({
    data <- filtered_data()
    valueBox(length(unique(data$Customer)), "Customers", icon = icon("users"), color = "orange")
  })
  
  output$ordersBox <- renderValueBox({
    data <- filtered_data()
    valueBox(length(unique(data$Order_ID)), "Orders", icon = icon("shopping-cart"), color = "purple")
  })
  
  output$salesBox <- renderValueBox({
    data <- filtered_data()
    valueBox(sprintf("$%.1fK", sum(data$Sales)/1000), "Total Sales", icon = icon("dollar-sign"), color = "blue")
  })
  
  output$profitBox <- renderValueBox({
    data <- filtered_data()
    valueBox(sprintf("$%.1fK", sum(data$Profit)/1000), "Total Profit", icon = icon("chart-line"), color = "green")
  })
  
  # Map
  output$stateMap <- renderPlot({
    data <- filtered_data()
    valid_states <- state.name
    state_data <- data %>%
      filter(State %in% valid_states) %>%
      group_by(State) %>%
      summarise(Sales = sum(Sales)) %>%
      rename(state = State)
    
    plot_usmap(data = state_data, values = "Sales") +
      scale_fill_continuous(name = "Sales ($)", label = scales::comma) +
      theme(legend.position = "right")
  })
  
  # Segment Plot
  output$segmentBar <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Segment) %>%
      summarise(Sales = sum(Sales))
    
    plot_ly(data, x = ~Segment, y = ~Sales, type = "bar",
            color = ~Segment, colors = "Set1") %>%
      layout(title = "Sales by Segment")
  })
  
  # Category Plot
  output$categoryBar <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Category) %>%
      summarise(Sales = sum(Sales))
    
    plot_ly(data, x = ~Category, y = ~Sales, type = "bar",
            color = ~Category, colors = "Set2") %>%
      layout(title = "Sales by Category")
  })
  
  # Manufacturer Plot
  output$manufacturerBar <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Manufacturer) %>%
      summarise(Sales = sum(Sales)) %>%
      arrange(desc(Sales)) %>%
      head(10)
    
    plot_ly(data, x = ~Sales, y = ~reorder(Manufacturer, Sales),
            type = "bar", orientation = "h",
            color = ~Manufacturer, colors = "Dark2") %>%
      layout(title = "Top Manufacturers", yaxis = list(title = ""))
  })
  
  # Customer Plot
  output$customerBar <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Customer) %>%
      summarise(Sales = sum(Sales)) %>%
      arrange(desc(Sales)) %>%
      head(10)
    
    plot_ly(data, x = ~Sales, y = ~reorder(Customer, Sales),
            type = "bar", orientation = "h",
            color = ~Customer, colors = "Set3") %>%
      layout(title = "Top Customers", yaxis = list(title = ""))
  })
}


shinyApp(ui, server)
