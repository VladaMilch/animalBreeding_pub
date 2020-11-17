library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Animal Breeding"),
  dashboardSidebar(
    
    numericInput(
      inputId = "genotypes_N",
      label = "Required number of animals",
      value = 10,
      min = 1,
      max = NA,
      step = 1,
      width = NULL
    ),
    
    sliderInput(inputId = "confidence_p", 
                label = "Required confidence level:",
                min = 0, max = 1,
                value = 0.9, step = 0.05),
    
    selectInput(inputId = "method", 
                label = "Please select a method", 
                choices = c("NONE",
                            "festing", 
                            "poisson",
                            "binomial*", 
                            "empirical*")),
    #uiOutput("conditional_inputs"),
    
    
    conditionalPanel(
      condition = "input.method == 'festing'",
      verticalLayout( 
        # textAreaInput(
        #   inputId = "comment", 
        #   label = "please add the required parameters", 
        #   placeholder = "write a positive whole number here"),
        
        sliderInput("effective_fertility_p", 
                    "Effective fertility:",
                    min = 0, max = 1,
                    value = 0.7, step = 0.05),
        
        sliderInput(inputId = "litter_mean",
                    label = "Average litter size:",
                    min = 1, 
                    max = 20,
                    value = 7),
        
        sliderInput(inputId = "litter_sd",
                    label = "Litter SD",
                    min = 0.5, 
                    max = 10,
                    value = 2.5, step = 0.5)
      )
    ),
    
    conditionalPanel(
      condition = "input.method == 'poisson'",
      verticalLayout( 
        # textAreaInput(
        #   inputId = "comment", 
        #   label = "please add the required parameters", 
        #   placeholder = "write a positive whole number here"),
        
        sliderInput("effective_fertility_p", 
                    "Effective fertility:",
                    min = 0, max = 1,
                    value = 0.7, step = 0.05),
        
        sliderInput(inputId = "litter_mean",
                    label = "Average litter size:",
                    min = 1, 
                    max = 20,
                    value = 7)
      ) 
    )
    
  ),
  dashboardBody(
    #uiOutput(
     # titlePanel("Animal Breeding"),
    #),
    uiOutput("selection_text"),
    uiOutput("comment_text")
  )
)

server <- function(input, output) {
  
  output$selection_text <- renderUI({
    paste("The selected method is: ", input$method)
  })
  
  output$conditional_inputs <- renderUI({
    req(input$method == "festing")
    
    verticalLayout( 
      # textAreaInput(
      #   inputId = "comment", 
      #   label = "please add the required parameters", 
      #   placeholder = "write a positive whole number here"),
      
      sliderInput("effective_fertility_p", 
                "Effective fertility:",
                min = 0, max = 1,
                value = 0.7, step = 0.05),
    
      sliderInput(inputId = "litter_mean",
                label = "Average litter size:",
                min = 1, 
                max = 20,
                value = 7),
    
      sliderInput(inputId = "litter_sd",
                label = "Litter SD",
                min = 0.1, 
                max = 20,
                value = 2.5)
    )
  })
  
  output$conditional_inputs <- renderUI({
    req(input$method == "poisson")
    
    verticalLayout( 
      # textAreaInput(
      #   inputId = "comment", 
      #   label = "please add the required parameters", 
      #   placeholder = "write a positive whole number here"),
      
      sliderInput("effective_fertility_p", 
                  "Effective fertility:",
                  min = 0, max = 1,
                  value = 0.7, step = 0.05),
      
      sliderInput(inputId = "litter_mean",
                  label = "Average litter size:",
                  min = 1, 
                  max = 20,
                  value = 7)
      ) 
    })
  
  output$comment_text <- renderText({
    input$comment
  })
}

shinyApp(ui = ui, server = server)