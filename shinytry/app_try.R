#install.packages("shiny")
#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(animalBreeding)

ui <- dashboardPage(
  
  readRDS()
  dashboardHeader(title = "Animal Breeding"),
  dashboardSidebar(
    
    numericInput(
      inputId = "n_needed",
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
                label = "Please select the mouse strain", 
                choices = c("NONE",
                            "manual")),
    #uiOutput("conditional_inputs"),
    
    
    conditionalPanel(
      condition = "input.method == 'manual'",
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
    #uiOutput("selection_text"),
    #uiOutput("needed_breedings"),
    #uiOutput("problem_statement_text"),
    fluidRow(
      box(title = "Histogram", status = "primary", plotOutput("plot2", height = 250)),
      
      box(
        title = "Inputs", status = "warning",
        "Box content here", br(), "More box content",
        sliderInput("slider", "Slider input:", 1, 100, 50),
        textInput("text", "Text input:")
      )
    )
    
  )
)

server <- function(input, output) {
  
  output$conditional_inputs <- renderUI({
    req(input$method == "festing")
    
    verticalLayout( 
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
  
  output$needed_breedings <- renderUI({
    req(input$method == "poisson" || input$method == "festing")
    
      needed_breedings <- 
        calculate_needed_breedings(
          n_needed = input$n_needed,
          confidence_p = input$confidence_p, 
          method = input$method, 
          effective_fertility_p = input$effective_fertility_p, 
          litter_mean = input$litter_mean, 
          binomial_p = input$binomial_p,
          offsprings_n_sample = input$offsprings_n_sample
      )
      paste("To achieve no less than ", 
            input$n_needed, 
            "offsprings in total, with the success probability ",
            input$confidence_p*100, 
            "%, \n one will require at least ", needed_breedings, " breedings.")
    
    
  })
  
  output$selection_text <- renderUI({
    paste("The selected method is: ", input$method)
  })
  output$problem_statement_text <- renderUI({
    req(input$method == "poisson" || input$method == "festing")
    
    paste("The following parameters have been used: ",
          "Average litter size  = ",
          input$litter_mean, 
          ", Effective fertility = ", 
          input$effective_fertility_p, 
          ", Method = ", input$method, ".", sep = "")
  })
    
}

shinyApp(ui = ui, server = server)