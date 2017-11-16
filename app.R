## shiny app is contained in unique app.R file

## load library
library(shiny)
library(ggplot2)

## Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("hypergeomagic"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of cards ----
      numericInput(inputId = "pop",
                  label = "deck size:",
                  step=1,
                  value = 0),
      
      numericInput(inputId = "tot_hits",
                  label = "# of hits in deck:",
                  step=1,
                  value = 0),
      
      numericInput(inputId = "min_hits",
                  label = "# of desired hits:",
                  step=1,
                  value = 1)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: plot ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)


## Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Plot of the probability mass of drawing at least one card
  # of the wanted type given 7, 8, ..., 17 looks and having #hits and size as input
  
  # This expression that generates a plot is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    myhyper <- function(min_hits, tot_hits, pop, looks){
      probabilities <- dhyper(min_hits, tot_hits, pop-tot_hits, looks)-phyper(min_hits, tot_hits, pop-tot_hits, looks)+1
      return(probabilities)
    }
    
    looks <- 7:17
    probabilities <- myhyper(input$min_hits, input$tot_hits, input$pop, looks)
    
    plot(x=looks, y=probabilities)
    
  })
  
}

shinyApp(ui = ui, server = server)



