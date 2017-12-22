library(shiny)

ui<-fluidPage(
  # Specify the input and output names here
  numericInput(inputId="mean",label="Mean",value=0),
  numericInput(inputId="sd",label="Standard Deviation",value=1),
  sliderInput(inputId="z",label="Z score",value=0,min=-4.0,max=4.0,step=0.01),
  plotOutput("line"),
  verbatimTextOutput("pvalue")
)
# inputs are things the user can toggle/manipulate
server<-function(input,output){
  
    
    output$line<-renderPlot({
      title<- "P Value"
      x<-(seq(-3.5,3.5,length=100)*input$sd) +(input$mean)
      y<-dnorm(x,input$mean,input$sd)
      data<-data.frame(x,y)
      pval=1-pnorm(input$z,input$mean,input$sd)
      pval<-paste0("p=",pval)
      plot(x,y,type="l",main=title)
      text(3,3,"hungry")
      polygon(c( x[x>=input$z], input$z ),  c(y[x>=input$z],0 ), col="gold3")
      
    
  })
    output$pvalue<-renderPrint({
      1-pnorm(input$z,input$mean,input$sd)

    })
}

shinyApp(ui=ui,server=server)
