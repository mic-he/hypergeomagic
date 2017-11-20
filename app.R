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
  
  # Table of the probability mass of drawing at least min_hits card
  # of the wanted type given 7, 8, ..., 17 looks and having #hits and size as input
  
  # The expression that generates a plot is wrapped in a call
  # to renderTable which takes same inputs as xtable to generate html code for the table
  output$table <- renderTable({
    
    myhyper <- function(min_hits, tot_hits, pop, looks){
      probabilities <- dhyper(min_hits, tot_hits, pop-tot_hits, looks)-phyper(min_hits, tot_hits, pop-tot_hits, looks)+1
      return(probabilities)
    }
    
    looks <- 7:17
    probabilities <- round(myhyper(input$min_hits, input$tot_hits, input$pop, looks),3)
    
    matrix(c(as.character(looks-7),probabilities), ncol=2, dimnames=list(c(),c("turn","probability")))
    
  })
  
}

shinyApp(ui = ui, server = server)



