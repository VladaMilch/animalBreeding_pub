#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    selectInput("method", "Method", c("festing", "binomial", "empirical", "poisson")),
    conditionalPanel( condition = "output.nrows",
                      checkboxInput("headonly", "Only use first 1000 rows")),
    # Application title
    titlePanel("Animal Breeding"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "litter_mean",
                        label = "Average litter size:",
                        min = 1, 
                        max = 20,
                        value = 7),
            
            sliderInput(inputId = "confidence_p", 
                        label = "Required confidence level",
                        min = 0, max = 1,
                        value = 0.9, step = 0.05),
            
            # Input: Specification of range within an interval ----
            sliderInput("effective_fertility_p", 
                        "Effective fertility:",
                        min = 0, max = 100,
                        value = c(20,50)),
            
            # Input: Custom currency format for with basic animation ----
            sliderInput("format", "Custom Format:",
                        min = 0, max = 10000,
                        value = 0, step = 2500,
                        pre = "$", sep = ",",
                        animate = TRUE),
            
            # Input: Animation with custom interval (in ms) ----
            # to control speed, plus looping
            sliderInput("animation", "Looping Animation:",
                        min = 1, max = 2000,
                        value = 1, step = 10,
                        animate =
                            animationOptions(interval = 300, loop = TRUE))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        plot(
            x = c(0:20), y = dpois(x = c(0:20), lambda = input$litter_mean))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
