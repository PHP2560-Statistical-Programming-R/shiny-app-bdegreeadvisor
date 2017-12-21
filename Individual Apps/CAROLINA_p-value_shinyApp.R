library(shiny)

ui <- fluidPage(
  # Title for the website
  titlePanel("P-value Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      # Enter z-value
      numericInput("num", label = h4("Enter a z-value"), 
                   value = 0, min = -100000, max = 100000, step = 0.01),
      
      # Radio button for type of test
      radioButtons("test", label = h4("Select the type of test"),
                   choices = c("Left-tailed test", "Right-tailed test", "Two-sided test")),
      
      # Submit button
      actionButton("submit", label = h5("Get p-value"))
    ),
    
    mainPanel(
      
      # Display a character string with the calculated p-value
      textOutput("pvalue"), 
      
      # Add a horizontal line to separate outputs
      tags$hr(),
      
      # Plot of calculated p-value
      plotOutput("stdNplot")
      
    )
  )
)



server <- function(input, output) {
  
  # Return the requested p-value depending on the type of test ----
  # Used eventReactive() for both z and typeTest because they depend on
  # input$submit (the action button), so that the output is only
  # updated when the user clicks the button
  
  z <- eventReactive(input$submit, {input$num})

  typeTest <- eventReactive(input$submit, {
    switch(input$test,
           "Left-tailed test" = 1,
           "Right-tailed test" = 2,
           "Two-sided test" = 3)
  }, ignoreNULL = FALSE)
  
  # Display result based on type of test
  output$pvalue <- renderText({
    if (typeTest()==1) {
      result <- round(pnorm(z(), lower.tail=TRUE), 4)
    } else if (typeTest()==2) {
      result <- round(pnorm(z(), lower.tail=FALSE), 4)
    } else if (typeTest()==3) {
      result <- round(2*pnorm(abs(z()), lower.tail=FALSE), 4)}
    
    # Return a character string with the calculated p-value depending on value of z
    print(paste0("The p-value for ", z(), " is ", result))
    
  })
  
  output$stdNplot <- renderPlot({
    areaTest <- function(x) {
      density <- dnorm(x)
      # Have NA values outside the area of interest depending on type of test
      if (typeTest()==1) { #lower tailed
        density[x > z()] <- NA
      } else if (typeTest()==2) { #upper tailed
        density[x < z()] <- NA
      } else { #two tailed
        density[x > -abs(z()) & x < abs(z())] <- NA
      }
      return(density) 
    }
    
    
    library(ggplot2) 
    plot <-  ggplot(data =  data.frame(x = c(-3-(abs(z()/2)), 3+(abs(z()/2)))), aes(x)) +
      # Standard normal density
      stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
      # Remove scales and breaks
      scale_y_continuous(breaks = NULL) +
      #scale_x_continuous(breaks = NULL) +
      # Color area of interest
      stat_function(fun = areaTest, geom = "area", fill = "blue") +
      # Add plot title and axes titles
      labs(x = "\n z", y = "f(z) \n", title = "Standard Normal Distribution \n") +
      theme(plot.title = element_text(hjust = 0.5, face="bold"), 
            axis.title.x = element_text(face="bold", size = 12),
            axis.title.y = element_text(face="bold", size = 12)) 
    plot
    
  })
  
}

shinyApp(ui=ui, server=server)  

