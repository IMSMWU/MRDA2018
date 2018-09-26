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
library(knitr)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Central Limit Theorem"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("samp",
                  "Sample Size:",
                  min = 0,
                  max = 200,
                  value = 0),
      sliderInput("numsamps",
                  "Number of Samples:",
                  min = 1,
                  max = 5000,
                  value = 1),
      checkboxInput('rand', label = 'Random Sample', value = TRUE),
      withMathJax(),
      tableOutput('mtable')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("samPlot"),
      plotOutput("distMeans"),
      plotOutput("distSD")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  population <- runif(1000, 0, 100)
  df <- data.frame('pop' = population, y = 1:1000, Sampled = FALSE)
  seed <- runif(1, 0, 9999)
  output$samPlot <- renderPlot({
    set.seed(seed)
    if (input$rand){
      smp <- sample(1:1000, size = input$samp, replace = FALSE)
    }else{
      smp <- sample(which(df$pop %in% sort(df$pop)[500:1000]), size = input$samp, replace = FALSE)
    }
    df$Sampled[smp] <- TRUE
    
    ggplot(df, aes(x = pop, y = y, color = Sampled)) +
      geom_jitter() + 
      theme_bw() +
      labs(x = 'Population Values', y = '') +
      scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = 'top') + 
      ggtitle("First Sample")
  })
  
  output$mtable <- renderTable({
    set.seed(seed)
    if (input$rand){
      smp <- sample(1:1000, size = input$samp, replace = FALSE)
    }else{
      smp <- sample(which(df$pop %in% sort(df$pop)[500:1000]), size = input$samp, replace = FALSE)
    }
    df$Sampled[smp] <- TRUE
    tab <- matrix(c(mean(df$pop), mean(df$pop[df$Sampled]),
                    sd(df$pop), sd(df$pop[df$Sampled])), nrow = 2)
    rownames(tab) <- c("Population", "First Sample")
    colnames(tab) <- c("Mean", "SD")
    tab 
  }, include.rownames = TRUE)
  
   output$distMeans <- renderPlot({
    set.seed(seed)
    if (input$rand){
      smp <- sample(1:1000, size = input$samp, replace = FALSE)
    }else{
      smp <- sample(which(df$pop %in% sort(df$pop)[500:1000]), size = input$samp, replace = FALSE)
    }
    df$Sampled[smp] <- TRUE
    m1 <- mean(df$pop[df$Sampled])
    means <- data.frame(mean = c(m1, rep(NA, input$numsamps-1)))
    if (input$numsamps>1){
      if(input$rand == FALSE){
        df2 <- df[df$pop > median(df$pop),]
      }else{
        df2 <- df
      }
    for (i in 2:input$numsamps){
      x <- sample(df2$pop, input$samp, replace = FALSE)
      means$mean[i] <- mean(x)
    }
    }
    p <- ggplot(means, aes(x = mean)) + 
            geom_histogram(show.legend = FALSE,aes(alpha = 0, y = ..count../sum(..count..)), fill = 'white', color = 'black', bins = 80, position = 'identity') +
            theme_bw() +
            ggtitle("Is the sample mean a good estimate of the population mean?") +
            labs(x = 'Mean Value', y = '') +
            scale_x_continuous(limits = c(0,100), breaks = seq(0,100, 25)) +
            #scale_y_continuous(limits = c(0,0.51))+
            geom_vline(data = df, xintercept = mean(df$pop), aes(color = 'Population Mean'))+
      annotate("text", x = mean(df$pop) + 15.5, y = 0.12, label = paste("Population Mean: ", round(mean(df$pop), 2))) +
      annotate("text", x = mean(df$pop) + 15, y = 0.07, label = paste("Mean of Means:", round(mean(means$mean), 2))) +
      annotate("text", x = mean(df$pop) + 14, y = 0.02, label = paste("SD of Means:", round(sd(means$mean), 2))) 
    suppressMessages(suppressWarnings(print(p)))
  })
 
   output$distSD <- renderPlot({
     set.seed(seed)
     if (input$rand){
       smp <- sample(1:1000, size = input$samp, replace = FALSE)
     }else{
       smp <- sample(which(df$pop %in% sort(df$pop)[500:1000]), size = input$samp, replace = FALSE)
     }
     df$Sampled[smp] <- TRUE
     sd1 <- sd(df$pop[df$Sampled])
     SDs <- data.frame(sd = c(sd1, rep(NA, input$numsamps-1)))
     if (input$numsamps>1){
       if(input$rand == FALSE){
         df2 <- df[df$pop > median(df$pop),]
       }else{
         df2 <- df
       }
       for (i in 2:input$numsamps){
         x <- sample(df2$pop, input$samp, replace = FALSE)
         SDs$sd[i] <- sd(x)
       }
     }
     p <- ggplot(SDs, aes(sd)) + 
       geom_histogram(show.legend = FALSE, aes(alpha = 0, y = ..count../sum(..count..)), fill = 'white', color = 'black', bins=100) +
        theme_bw() +
        ggtitle("Is the sample SD a good estimate of the population SD?") +
        labs(x = 'SD Value', y = '') +
        scale_x_continuous(limits = c(0,100), breaks = seq(0,100, 25)) +
       # scale_y_continuous(limits = c(0,0.51))+
        geom_vline(data = df, xintercept = sd(df$pop), aes(color = 'Population SD'))+
        annotate("text", x = sd(df$pop) + 10.5, y = 0.12, label = paste("Population SD:", round(sd(df$pop), 2))) +
        annotate("text", x = sd(df$pop) + 10.5, y = 0.07, label = paste("Mean sample SDs:", round(mean(SDs$sd), 2))) +
        annotate("text", x = sd(df$pop) + 10.5, y = 0.02, label = paste("SD of sample SDs:", round(sd(SDs$sd), 2))) 
     suppressMessages(suppressWarnings(print(p)))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

