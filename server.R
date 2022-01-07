source('prediction.R')
library(shiny)


shinyServer(function(input, output) {
    
    observeEvent( input$submit,{
        
        inputTxt=input$text
        
        prediction=f.predFunt(inputTxt,5)
         output$table=renderDataTable(data.table(prediction))
       
    })
    
    
})
