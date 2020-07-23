# Load library
library("shinydashboard")
library("DT")
library("xlsx")

# Run dashboard
ui <- dashboardPage(
  dashboardHeader(title = "덕트 구매지원 시스템"),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem("기계약 검색", tabName = "search", icon = icon("search")), 
    menuItem("입찰 분석", tabName = "analytics_bid", icon = icon("chart-bar")), 
    menuItem("기계약 분석", tabName = "analytics_con", icon = icon("dashboard")), 
    menuItem("품샘 정보", tabName = "labor", icon = icon("file-pdf")), 
    menuItem("물가 정보", tabName = "price", icon = icon("file-invoice"))
  )), 
  ## Body content
  dashboardBody(tabItems(
    # search tab content
    tabItem(
      tabName = "search",
      # Select boxes need to be put in a row
      fluidRow(box(
        column(2, selectInput("major", "대분류:", c("전체"))), 
        column(2, selectInput("name", "품명:", c("전체"))), 
        column(2, selectInput("std", "규격:", c("전체"))), 
        column(2, selectInput("year", " 연도:", c("전체"))), 
        column(2, selectInput("site", "현장:", c("전체"))), 
        column(2, selectInput("coop", "협력사:", c("전체"))), 
        width = NULL
      )),
      # Create a new row for the table.
      DT::dataTableOutput("table"), 
      # Download buttons for table
      
    ), 
    
    # analytics_bid tab content
    tabItem(
      tabName = "analytics_bid",
      DT::dataTableOutput("stat")
    ), 
    
    # analytics_con tab content
    tabItem(
      tabName = "analytics_con", 
      fluidRow( 
        # Input: Select a file
        box(fileInput("sheet", "Choose Excel File", multiple = FALSE,
                      accept = ".xlsx"
        ))
      ), 
      DT::dataTableOutput("sheet")
    ), 
    
    # labor tab content
    tabItem(
      tabName = "labor",
      h1("품셈 정보 탭 내용")
    ), 
    
    # price tab content
    tabItem(
      tabName = "price",
      h1("물가 정보 탭 내용")
    )
  ))
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  observe({
    # Filter data based on selections
    data <- read.csv("Data.csv", stringsAsFactors = F, fileEncoding = "euc-kr")
    table <- data
    if (input$major != "전체") {
      table <- table[table$대분류 == input$major, ]
    }
    if (input$name != "전체") {
      table <- table[table$품명 == input$name, ]
    }
    if (input$std != "전체") {
      table <- table[table$규격 == input$std, ]
    }
    if (input$year != "전체") {
      table <- table[table$연도 == input$year, ]
    }
    if (input$site != "전체") {
      table <- table[table$현장 == input$site, ]
    }
    if (input$coop != "전체") {
      table <- table[table$협력사 == input$coop, ]
    }
    
    # Render table
    output$table <- DT::renderDataTable(datatable(table, extensions = "FixedHeader", 
                                                  options = list(fixedHeader = T, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체"))))
                                        %>% formatCurrency(c("자재비", "노무비", "경비"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))
    
    # Update select box contents
    list.select <- sapply(1:6, function(temp) list(data[, c(1:3, 9:11)]))
    if (input$major != "전체") {
      for (i in c(2:6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$대분류 == input$major, ]
    }
    if (input$name != "전체") {
      for (i in c(1, 3:6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$품명 == input$name, ]
    }
    if (input$std != "전체") {
      for (i in c(1:2, 4:6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$규격 == input$std, ]
    }
    if (input$year != "전체") {
      for (i in c(1:3, 5:6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$연도 == input$year, ]
    }
    if (input$site != "전체") {
      for (i in c(1:4, 6)) list.select[[i]] <- list.select[[i]][list.select[[i]]$현장 == input$site, ]
    }
    if (input$coop != "전체") {
      for (i in c(1:5)) list.select[[i]] <- list.select[[i]][list.select[[i]]$협력사 == input$coop, ]
    }
    updateSelectInput(session, inputId = "major", choices = c("전체", sort(unique(as.character(list.select[[1]]$대분류)))), selected = input$major)
    updateSelectInput(session, inputId = "name", choices = c("전체", sort(unique(as.character(list.select[[2]]$품명)))), selected = input$name)
    updateSelectInput(session, inputId = "std", choices = c("전체", sort(unique(as.character(list.select[[3]]$규격)))), selected = input$std)
    updateSelectInput(session, inputId = "year", choices = c("전체", sort(unique(as.character(list.select[[4]]$연도)))), selected = input$year)
    updateSelectInput(session, inputId = "site", choices = c("전체", sort(unique(as.character(list.select[[5]]$현장)))), selected = input$site)
    updateSelectInput(session, inputId = "coop", choices = c("전체", sort(unique(as.character(list.select[[6]]$협력사)))), selected = input$coop)
    
    # Read uploaded sheet
    req(input$sheet)
    tryCatch(
      {
        sheet <- read.xlsx2(input$sheet$datapath, sheetIndex = 1, stringsAsFactors = F)
      },
      error = function(e) {
        # return a safe error if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    # Filter data based on sheet
    stat <- cbind(sheet[, c(1:3, 6)], data.frame(최소값 = NA, 평균값 = NA, 중간값 = NA, 최대값 = NA))
    for (i in 1:nrow(stat)) {
      temp <- as.numeric(data[data$대분류 == stat[i, ]$대분류 & data$규격 == stat[i, ]$규격, ]$자재비)
      stat[i, 5:8] <- round(c(min(temp), mean(temp), median(temp), max(temp)))
    }
    
    # Render stat
    output$stat <- DT::renderDataTable(datatable(stat, extensions = "FixedHeader", 
                                                 options = list(fixedHeader = T, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체")), pageLength = 25))
                                       %>% formatCurrency(c("자재비.단가", "최소값", "평균값", "중간값", "최대값"), 
                                                          currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))
    
    # Render uploaded sheet
    output$sheet <- DT::renderDataTable(datatable(sheet, extensions = "FixedHeader", 
                                                  options = list(fixedHeader = T, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체")), pageLength = 25, searching = FALSE))
                                        %>% formatCurrency(c("자재비.단가", "자재비.금액", "노무비.단가", "노무비.금액", "경비.단가", "경비.금액", "합계.단가", "합계.금액"), 
                                                           currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))
  })
}

shinyApp(ui, server)