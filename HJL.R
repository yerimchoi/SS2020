library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Example"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Category:",
                  choices = c("Firm A", "Firm B", "Firm C")),
      plotOutput(outputId =
                   "plot1"),
      plotOutput(outputId =
                   "plot2")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "A" = rock,
           "B" = pressure[,c(1:2)],
           "C" = cars[,c(1:2)])
  })
  
  # Show the first "n" observations ----\
  output$view <- renderTable({
    head(read.csv("Dashboard.csv", header = T), n = 25)
  })
  output$plot1 <-  renderPlot({
    par(family = "Gullim", pty ="s")
    plot(seq(-3, 3, length=200), dnorm(seq(-3, 3, length=200), mean=0, sd=1), type='l', xaxt = "n", yaxt = "n", ann = F)
    title(xlab = "Summary", cex.lab = 2)
    lines(data.frame(x = c(2, 2), y = c(0, dnorm(2, mean=0, sd=1))), col = 3)
    lines(data.frame(x = c(1.5, 1.5), y = c(0, dnorm(1.5, mean=0, sd=1))), col = 2)
    lines(data.frame(x = c(-0.5, -0.5), y = c(0, dnorm(-0.5, mean=0, sd=1))), col = 4)
    legend("topright", legend = c("A", "B", "C"), fil = 2:4, col = 1:3, cex = 0.6)
  })
  output$plot2 <-  renderPlot({
    par(family = "Gullim", pty ="s")
    plot(seq(-4.5, 4, length=200), dnorm(seq(-4.5, 4, length=200), mean=0, sd=1), type='l', xlim = c(-8, 5), ann = F, xaxt = "n", yaxt = "n", col = 1)
    title(xlab = "Details", cex.lab = 2)
    lines(seq(-8, 5, length=200), dnorm(seq(-8, 5, length=200), mean=1, sd=2),  col="blue")
    lines(seq(-8, 3, length=200), dnorm(seq(-8, 3, length=200), mean=-4, sd=1.5), col="limegreen")
    lines(data.frame(x = c(1, 1), y = c(0, dnorm(1, mean=0, sd=1))), col = "Red")
    lines(data.frame(x = c(-1, -1), y = c(0, dnorm(-1, mean=1, sd=2))), col = "Red")
    lines(data.frame(x = c(-5, -5), y = c(0, dnorm(-5, mean=-4, sd=1.5))), col = "Red")
    
    
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
