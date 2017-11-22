## shiny app is contained in unique app.R file

## load library
library(shiny)

## Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("hypergeomagic"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: numeric inputs for the number of cards ----
      numericInput(inputId = "pop",
                  label = "deck size:",
                  step=1,
                  value = 40),
      
      numericInput(inputId = "tot_hits",
                  label = "# of hits in deck:",
                  step=1,
                  value = 17),
      
      numericInput(inputId = "min_hits",
                  label = "# of desired hits:",
                  step=1,
                  value = 3)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: plot ----
      tableOutput("table")
      
    )
  )
)


## Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Table of the probability mass of drawing at least min_hits card and exactly min_hits cards
  # of the wanted type given 7, 8, ..., 17 looks and having #hits and size as input
  
  # The expression that generates a plot is wrapped in a call
  # to renderTable which takes same inputs as xtable to generate html code for the table
  output$table <- renderTable({
    
    myhyper <- function(min_hits, tot_hits, pop, looks){
      exactly <- round(dhyper(min_hits, tot_hits, pop-tot_hits, looks), 3)
      at_least <- round(dhyper(min_hits, tot_hits, pop-tot_hits, looks)
                        -phyper(min_hits, tot_hits, pop-tot_hits, looks)+1,
                        3)
      return(list(exactly, at_least))
    }
    
    looks <- 7:17
    probabilities <- myhyper(input$min_hits, input$tot_hits, input$pop, looks)
    
    matrix(c(as.character(looks-6), unlist(probabilities[1]), unlist(probabilities[2])),
           ncol=3, dimnames=list(c(),
                                 c("turn",
                                   paste("at least ", input$min_hits),
                                   paste("exactly", input$min_hits))))
    
  })
  
}

shinyApp(ui = ui, server = server)



