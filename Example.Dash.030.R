# Load library
library("shinydashboard")
library("ggplot2")

# Run dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Example Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Search", tabName = "search", icon = icon("search")), 
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")), 
      menuItem("Sheet", tabName = "sheet", icon = icon("table"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # Search tab content
      tabItem(
        tabName = "search",
        # Boxes need to be put in a row (or column)
        fluidRow(
          column(3,selectInput("man",
                               "Manufacturer:",
                               c("All",
                                 unique(as.character(mpg$manufacturer))))
          ), 
          column(3,selectInput("man",
                               "Manufacturer:",
                               c("All",
                                 unique(as.character(mpg$manufacturer))))
          ), 
          column(3,selectInput("trans",
                               "Transmission:",
                               c("All",
                                 unique(as.character(mpg$trans))))
          ), 
          column(3,selectInput("cyl",
                               "Cylinders:",
                               c("All",
                                 unique(as.character(mpg$cyl))))
          )
        ),
        # Create a new row for the table.
        DT::dataTableOutput("table")
      ), 
      
      # Summary tab content
      tabItem(
        tabName = "summary",
        h2("Summary tab content")
      ), 
      
      # Sheet tab content
      tabItem(
        tabName = "sheet",
        h2("Sheet tab content")
      )
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- mpg
    if (input$man != "All") {
      data <- data[data$man == input$man,]
    }
    if (input$man != "All") {
      data <- data[data$man == input$man,]
    }
    if (input$trans != "All") {
      data <- data[data$cyl == input$cyl,]
    }
    if (input$cyl != "All") {
      data <- data[data$trans == input$trans,]
    }
    data
  }))
}

shinyApp(ui, server)