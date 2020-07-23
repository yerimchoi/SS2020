# Load library
library("shinydashboard")

# Load data
data <- read.csv("Data.csv", stringsAsFactors = TRUE, fileEncoding = 'euc-kr')

# Run dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Example Dashboard"),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem("Search", tabName = "search", icon = icon("search")), 
    menuItem("Summary", tabName = "summary", icon = icon("dashboard")), 
    menuItem("Upload", tabName = "upload", icon = icon("upload"))
  )),
  ## Body content
  dashboardBody(tabItems(
    # Search tab content
    tabItem(
      tabName = "search",
      # Select boxes need to be put in a row
      fluidRow(box(
        column(2, selectInput("major",
                              "대분류:",
                              c("All", unique(as.character(data$대분류))))
        ), 
        column(2, selectInput("name",
                              "품명:",
                              c("All", unique(as.character(data$품명))))
        ), 
        column(2, selectInput("std",
                              "규격:",
                              c("All", unique(as.character(data$규격))))
        ), 
        column(2, selectInput("year",
                              " 연도:",
                              c("All", unique(as.character(data$연도))))
        ), 
        column(2, selectInput("cite",
                              "현장:",
                              c("All", unique(as.character(data$현장))))
        ), 
        column(2, selectInput("build",
                              "시공:",
                              c("All", unique(as.character(data$시공))))
        ), width = NULL
      )),
      # Create a new row for the table.
      DT::dataTableOutput("table")
    ), 
    
    # Summary tab content
    tabItem(
      tabName = "summary",
      h2("Summary tab content")
    ), 
    
    # Upload tab content
    tabItem(
      tabName = "upload", 
      fluidRow( 
        # Input: Select a file
        box(fileInput("sheet", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
        )
      ), 
      DT::dataTableOutput("sheet")
    )
  )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- read.csv("Data.csv", stringsAsFactors = TRUE, fileEncoding = 'euc-kr')
    if (input$major != "All") {
      data <- data[data$대분류 == input$major,]
    }
    if (input$name != "All") {
      data <- data[data$품명 == input$name,]
    }
    if (input$std != "All") {
      data <- data[data$규격 == input$std,]
    }
    if (input$year != "All") {
      data <- data[data$연도 == input$year,]
    }
    if (input$cite != "All") {
      data <- data[data$현장 == input$cite,]
    }
    if (input$build != "All") {
      data <- data[data$시공 == input$build,]
    }
    data
  }))
  output$sheet <- DT::renderDataTable(DT::datatable({
    req(input$sheet)
    tryCatch(
      {
        sheet <- read.csv(input$sheet$datapath, stringsAsFactors = TRUE, fileEncoding = 'euc-kr')
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    sheet
  }, options = list(searching = FALSE)))
}

shinyApp(ui, server)