library(shiny)
library(shinythemes)

ui <- fluidPage(
  #theme = shinytheme("lumen"),
  titlePanel("Z-value to P-value"),
  sidebarLayout(
    sidebarPanel(
      numericInput("z_input", "z-value:", 1.645, step = .005),
      radioButtons("type", "Type:", 
                   c("Right-Tailed",
                     "Left-Tailed", 
                     "Two-Tailed"), 
                   selected = "Right-Tailed")
    ),
    mainPanel(
      h3("p-value:"),
      verbatimTextOutput("pval"),
      plotOutput("norm")
    )
  )
)

server <- function(input, output) {
  p_val <- reactive({
    if(input$type == "Right-Tailed"){
      pnorm(input$z_input, lower.tail = FALSE)
    } else if(input$type == "Left-Tailed"){
      pnorm(input$z_input, lower.tail = TRUE)
    } else {
      2 * pnorm(input$z_input, lower.tail = FALSE)
    }
  })
  output$pval <- renderPrint({p_val()}, width = 20)
  poly <- reactive({
    if(input$type == "Right-Tailed"){
      polygon(c(input$z_input, seq(input$z_input, 5, 0.01), 5),
              c(0, dnorm(seq(input$z_input, 5, 0.01)), 0),
              col = "firebrick1",
              border = NA)
    } else if(input$type == "Left-Tailed"){
      polygon(c(-5, seq(-5, input$z_input, 0.01), input$z_input),
              c(0, dnorm(seq(-5, input$z_input, 0.01)), 0),
              col = "firebrick1",
              border = NA)
    } else {
      polygon(c(abs(input$z_input), seq(abs(input$z_input), 5, 0.01), 5),
              c(0, dnorm(seq(abs(input$z_input), 5, 0.01)), 0),
              col = "firebrick1",
              border = NA)
      polygon(c(-5, seq(-5, -abs(input$z_input), 0.01), -abs(input$z_input)),
              c(0, dnorm(seq(-5, -abs(input$z_input), 0.01)), 0),
              col = "firebrick1",
              border = NA)
    }
  })
  output$norm <- renderPlot({
    curve(dnorm(x, 0, 1), xlim = c(-5, 5), main = "Normal Distribution", xlab = "", ylab = "Density")
    poly()
  })
}

shinyApp(ui, server)