library(shiny)
library(plotly)
library(dplyr)

############## preparing the data #######################

topActors <- read.csv("data/topActors.csv", stringsAsFactors = FALSE) 
topCategories <- read.csv("data/topCategories.csv", stringsAsFactors = FALSE) 
topMovies <- read.csv("data/topMovies.csv", stringsAsFactors = FALSE) 
topCustomers <- read.csv("data/topCustomers.csv", stringsAsFactors = FALSE) 

############ Shiny App #################

ui <- fluidPage(
  titlePanel('DVD Rental Summary'),
  sidebarLayout(
    sidebarPanel(
      br(),
      br(),
      helpText("Top 10"),
      selectInput("TopTen",
                  label = "choose item",
                  choices = list("The Most Rented Movies",
                                 "The Most Active Customer",
                                 "The Most Active Actor",
                                 "The Most Favorite Category"),
                  selected = "The Most Active Customer"),
      br(),
      br(),
      br()
    ),
    mainPanel(
      plotlyOutput("bar"),
      br(),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  
  dataInput <- reactive({
    switch(input$TopTen,
           "The Most Rented Movies" = topMovies,
           "The Most Active Customer" = topCustomers,
           "The Most Active Actor" = topActors,
           "The Most Favorite Category" = topCategories)
  })
  
  output$bar <- renderPlotly({
    data <- dataInput()[1:10, ]
    nc <- ncol(data)
    data$Name <- factor(
      data$Name, 
      levels = unique(data$Name)[order(data[, nc], decreasing = TRUE)])
    y <- list(
      title = colnames(data)[nc]
    )
    x <- list(
      title = "", tickfont = list(size = 8)
    )
    plot_ly(data, y = ~data[, ncol(data)], 
            x = ~Name,
            type = 'bar') %>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$table <- renderTable({
    head(dataInput(), 10)
  })
}

shinyApp(ui, server)
