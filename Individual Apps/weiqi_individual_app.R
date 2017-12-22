#
# This is a "Z score and P-value Converting Application"
# By using this app, users can type in or select interested Z-score  
# and select interested area under normal distribution
# The returned results are the corresponding p-value and the graph of the probability 
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("P Value From Z Score"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # Add two inputs
        # First input: Z-score.
        # Could select the number or type in the interested number
        numericInput("input_z", "Z-Score", 1.645, step = 0.1),
        # Three types of shaded area under normal distribution
        selectInput("input_test","Interested Area", h3("Select Test Type"),
                    choices = c("Right Side", "Left Side","Two-tailed"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # return the p-value in the mainpanel
        h5("P-value:"),
        verbatimTextOutput("output_p"),
        # return the graph
        plotOutput("output_plot")
      )
   )
)

# Define server logic required to return text and draw a diagram
server <- function(input, output) {
  
  # Return p-value based on the z-score
  p_value<- reactive({
    if(input$input_test =="Right Side"){
      pnorm(input$input_z, mean = 0, sd = 1, lower.tail = FALSE) 
    }else if(input$input_test == "Left Side"){
      pnorm(input$input_z, mean = 0, sd = 1, lower.tail = TRUE)
    } else{
      2 * pnorm(input$input_z, mean = 0, sd = 1, lower.tail = FALSE)
    }
  })
 
  # Display the output of p-value and name it as output_p
  output$output_p <- renderPrint({p_value()}, width=20)
  
  graph <- reactive({
    # Use "polygon" plot and normal distribution and the interested area
      # For "right side"
      if(input$input_test =="Right Side"){
      x=seq(-4,4,length=200)     
      y = dnorm(x)
      plot(x,y,type="l", lwd=2, col="blue")
      x=seq(input$input_z,4,length=200)
      y = dnorm(x)
      polygon(c(input$input_z,x,4),c(0,y,0),col="skyblue")
      #for "left side"
    } else if(input$input_test =="Left"){
      x=seq(-4,4,length=200)     
      y = dnorm(x)
      plot(x,y,type="l", lwd=2, col="blue")
      x=seq(-4,input$input_z,length=200)
      y = dnorm(x)
      polygon(c(-4,x,input$input_z),c(0,y,0),col="skyblue")
      # for two-sided
    } else if(input$input_test =="Two-tailed"){
      x=seq(-4,4,length=200)
      y=dnorm(x)
      plot(x,y,type="l", lwd=2, col="blue")
      x=seq(-4,-(input$input_z),length=200)
      y=dnorm(x)
      polygon(c(-4,x,-(input$input_z)),c(0,y,0),col="skyblue")
      x=seq((input$input_z),4,length=200)
      y=dnorm(x)
      polygon(c(input$input_z,x,4),c(0,y,0),col="skyblue")
      
      
      
    }
  })
  
  # Display the graph and name it as output_plot
  output$output_plot <- renderPlot(
    graph())
  
}

# Run the application 
shinyApp(ui = ui, server = server)

