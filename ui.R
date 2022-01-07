library(shiny)
library(shinythemes)

shinyUI(fluidPage(
    
         sidebarLayout(
         sidebarPanel(
             textInput("text", label=('Enter text here'), value=''),
                 actionButton("submit", "Submit")
         ),
     mainPanel(
   
                  dataTableOutput('table'),
                 # verbatimTextOutput('table'),
                 textOutput('list')
        
         )
    )
)
)