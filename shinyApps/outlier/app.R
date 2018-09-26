#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Outliers in OLS"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("outlier",
                     "Value:",
                     min = 0,
                     max = 3000,
                     value = 500)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      x <- runif(100, 100, 1000)
      x[1] <- input$outlier
      y <- 50 + 10*x + rnorm(100,0, 20)
      df <- data.frame('Advertising' = x, "Sales" = y)
      ggplot(df)+
        geom_smooth(aes(x = Advertising, y = Sales), method = 'lm')+
        ylim(c(0, 50000))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

