library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "pressure" = pressure,
           "cars" = cars)
  })

  
  # Generate a summary of the dataset
  output$plot_fit <- renderPlot({
    dataset <- datasetInput()

      x= dataset[,1]
      y= dataset[,2]

      fit = lm(y~poly(x, input$degree, raw=TRUE))
        
      plot(x, y,pch = 19)
      
      xx <- seq(min(x),max(x), length=100)
      lines(xx, predict(fit, data.frame(x=xx)), col="red")
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$degree)
  })
})


# 
# pres = pressure$pressure
# temp = pressure$temperatureroc
# 
# lm.out3 = lm(pres ~ temp + I(temp^2) + I(temp^3))
# 
# plot(pres~temp)
# curve(lm.out3[[1]][1] + lm.out3[[1]][2]*x + lm.out3[[1]][3]*x^2 + lm.out3[[1]][4]*x^3,add=T)
