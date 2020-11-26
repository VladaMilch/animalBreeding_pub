#install.packages("shiny")
#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(devtools)
devtools::install("./animalBreeding/")
library(animalBreeding)
load("./data/strain_stats.Rdata")


ui <- dashboardPage(

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
    
    sliderInput("effective_fertility_p", 
                "Effective fertility:",
                min = 0, max = 1,
                value = 0.7, step = 0.05),
    
    selectInput(inputId = "method", 
      label = "Please select the mouse strain", 
      choices = c("NONE", "manual", c(strain_stats$strain))),
    #uiOutput("conditional_inputs"),
    
    
    
    conditionalPanel(
      condition = "input.method == 'manual'",
      verticalLayout( 
        # textAreaInput(
        #   inputId = "comment", 
        #   label = "please add the required parameters", 
        #   placeholder = "write a positive whole number here"),
        sliderInput(inputId = "litter_mean",
                    label = "Average litter size:",
                    min = 1, 
                    max = 20,
                    value = 7)
      ) 
    )
    
  ),
  dashboardBody(
    
    conditionalPanel(
      condition = "input.n_needed == 0 | input.effective_fertility_p==1 | input.method=='NONE'",
      verticalLayout( 
          helpText("Please specify the required number of pups, effective fertility and the mouse strain in the left pannel. ")
    )),
      # fluidRow(
      #   column(12,
      #     radioButtons(inputId="choice", 
      #                label="How many genotypes do you wish to get from a single breeding setup?", 
      #                choices=c("Single genotype")))#,
      #   #column(5,
      #   #  helpText("Help text here..."))
      #   ), 
      fluidRow(
        column(11,
              uiOutput("needed_breedings")
              )
      ), 
      plotOutput(outputId = "confidence_plot")
      )
)

server <- function(input, output) {
  
  # output$selected_var <- renderText({ 
  #   paste("You have selected", input$var)
  # })

  output$conditional_inputs <- renderUI({
    req(input$method == "manual")
    
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
    #req(input$choice == "Single genotype")
    req(input$method != "NONE")
    req(input$n_needed > 0)
    
    
    if(input$method != "manual" & input$method != "NONE"){
      litt_mean <- strain_stats$litter_mean[which(strain_stats$strain==input$method)]
    }else if(input$method == "manual"){
      litt_mean = input$litter_mean
    }
    
    
      needed_breedings <- 
        calculate_needed_breedings(
          n_needed = input$n_needed,
          confidence_p = input$confidence_p, 
          method = "poisson", 
          effective_fertility_p = input$effective_fertility_p, 
          litter_mean = litt_mean, 
          binomial_p = input$binomial_p,
          offsprings_n_sample = input$offsprings_n_sample
      )
      HTML(
        paste("To achieve no less than ","<b>", 
            input$n_needed, "</b>",
            "offsprings (of a single genotype) in total, with the success probability ", "<b>",
            input$confidence_p*100, "%</b>",
            ", \n one will require at least ", "<b>",needed_breedings, "</b>"," breedings.")
        )
    
    
  })

  
  output$confidence_plot <- renderPlot({
      #req(input$choice == "Single genotype")
    req(input$method != "NONE")
    req(input$n_needed > 0)
    
      
      if(input$method != "manual" & input$method != "NONE"){
        litt_mean <- strain_stats$litter_mean[which(strain_stats$strain==input$method)]
      }else if(input$method == "manual"){
        litt_mean = input$litter_mean
      }
      
      needed_breedings <- 
        calculate_needed_breedings(
          n_needed = input$n_needed,
          confidence_p = input$confidence_p, 
          method = "poisson", 
          effective_fertility_p = input$effective_fertility_p, 
          litter_mean = litt_mean, 
          binomial_p = input$binomial_p,
          offsprings_n_sample = input$offsprings_n_sample
        )
      # 
      # #input$confidence_p
      # confidence_grid <- seq(0.5, 0.95, 0.05)
      # 
      # bree_of_confidence <- sapply(confidence_grid,FUN = function(x){
      #     calculate_needed_breedings(
      #     n_needed = input$n_needed,
      #     confidence_p = x, 
      #     method = "poisson", 
      #     effective_fertility_p = input$effective_fertility_p, 
      #     litter_mean = litt_mean, 
      #     binomial_p = input$binomial_p,
      #     offsprings_n_sample = input$offsprings_n_sample
      #     )
      #   })
      
      
      effective_fertility_p = input$effective_fertility_p
      freqs_r <- dpois(x = seq(1,round(4*litt_mean), 1), lambda = litt_mean)
      freqs <- freqs_r/sum(freqs_r)
      supp1 = as.numeric(c(0, seq(1,round(4*litt_mean))))
      prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
      stopifnot(sum(prob1)==1)
      
      #search_interval <- seq(1,10)
      doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
      ff <- function(k){
        doofN_req <- distr::convpow(doof1, N=k)
        #@q(1-confidence_p)
        true_confidence <- 1-doofN_req@p(input$n_needed)   
      }
      
      interval_around_reqbre <- c(
        (needed_breedings-2):(needed_breedings+2)
        )
      true_confs <- sapply(
        interval_around_reqbre, 
        ff)
      
      plot(interval_around_reqbre, true_confs, 
           xlab = "number of breedings", 
           ylab = "confidence level", pch =16)
      points(x=needed_breedings,y=true_confs[3], col = "blue", pch = 16)
      text(x=c(needed_breedings),
           y=c(true_confs[3]),
           labels = c("required number of breedings"), pos = 1, 
           col="blue")
      
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
