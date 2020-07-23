# Load library
library("shinydashboard")
library("DT")

# Run dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Duct Dashboard"),
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
                              c("All"))
        ), 
        column(2, selectInput("name",
                              "품명:",
                              c("All"))
        ), 
        column(2, selectInput("std",
                              "규격:",
                              c("All"))
        ), 
        column(2, selectInput("year",
                              " 연도:",
                              c("All"))
        ), 
        column(2, selectInput("site",
                              "현장:",
                              c("All"))
        ), 
        column(2, selectInput("build",
                              "시공:",
                              c("All"))
        ), width = NULL
      )),
      # Create a new row for the table.
      dataTableOutput("table")
    ), 
    
    # Summary tab content
    tabItem(
      tabName = "summary",
      dataTableOutput("stat")
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
      dataTableOutput("sheet")
    )
  ))
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  observe({
    # Filter data based on selections
    data <- read.csv("Data.csv", fileEncoding = "euc-kr")
    table <- data
    if (input$major != "All") {
      table <- table[table$대분류 == input$major, ]
    }
    if (input$name != "All") {
      table <- table[table$품명 == input$name, ]
    }
    if (input$std != "All") {
      table <- table[table$규격 == input$std, ]
    }
    if (input$year != "All") {
      table <- table[table$연도 == input$year, ]
    }
    if (input$site != "All") {
      table <- table[table$현장 == input$site, ]
    }
    if (input$build != "All") {
      table <- table[table$시공 == input$build, ]
    }
    
    # Render table
    output$table <- renderDataTable(datatable(table, options = list(lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")))))
    
    # Update select box contents
    list.select <- sapply(1:6, function(temp) list(data[, c(1:3, 9:11)]))
    if (input$major != "All") {
      for (i in c(2:6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$대분류 == input$major, ]
    }
    if (input$name != "All") {
      for (i in c(1, 3:6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$품명 == input$name, ]
    }
    if (input$std != "All") {
      for (i in c(1:2, 4:6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$규격 == input$std, ]
    }
    if (input$year != "All") {
      for (i in c(1:3, 5:6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$연도 == input$year, ]
    }
    if (input$site != "All") {
      for (i in c(1:4, 6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$현장 == input$site, ]
    }
    if (input$build != "All") {
      for (i in c(1:5)) list.select[[i]] <- list.select[[i]][list.select[[i]]$시공 == input$build, ]
    }
    updateSelectInput(session, 
                      inputId = "major", 
                      choices = c("All", sort(unique(as.character(list.select[[1]]$대분류)))), 
                      selected = input$major)
    updateSelectInput(session, 
                      inputId = "name", 
                      choices = c("All", sort(unique(as.character(list.select[[2]]$품명)))), 
                      selected = input$name)
    updateSelectInput(session, 
                      inputId = "std", 
                      choices = c("All", sort(unique(as.character(list.select[[3]]$규격)))), 
                      selected = input$std)
    updateSelectInput(session, 
                      inputId = "year", 
                      choices = c("All", sort(unique(as.character(list.select[[4]]$연도)))), 
                      selected = input$year)
    updateSelectInput(session, 
                      inputId = "site", 
                      choices = c("All", sort(unique(as.character(list.select[[5]]$현장)))), 
                      selected = input$site)
    updateSelectInput(session, 
                      inputId = "build", 
                      choices = c("All", sort(unique(as.character(list.select[[6]]$시공)))), 
                      selected = input$build)
    
    # Read uploaded sheet
    req(input$sheet)
    tryCatch(
      {
        sheet <- read.csv(input$sheet$datapath, fileEncoding = "euc-kr")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    # Filter data based on sheet
    stat <- cbind(sheet[, c(1:3, 6)], data.frame(최소값 = NA, 평균값 = NA, 중간값 = NA, 최대값 = NA))
    for (i in 1:nrow(stat)) {
      temp <- data[data$대분류 == stat[i, ]$대분류 & data$규격 == stat[i, ]$규격, ]$자재비
      stat[i, 5:8] <- round(c(min(temp), mean(temp), median(temp), max(temp)))
    }
    
    # Render stat
    output$stat <- renderDataTable(datatable(stat, options = list(lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")), pageLength = 25)))
    
    # Render uploaded sheet
    output$sheet <- renderDataTable(datatable(sheet, options = list(lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")), pageLength = 25, searching = FALSE)))
  })
}

shinyApp(ui, server)