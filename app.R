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
      numericInput(inputId = "pop", # population, or total number of cards in the deck
                  label = "deck size",
                  step=1,
                  value = 40),
      
      numericInput(inputId = "tot_hits", # successes, or total number of cards of the wanted kind in the deck
                  label = "# of hits in deck",
                  step=1,
                  value = 17),
      
      numericInput(inputId = "min_hits", # hits, or minimum number of cards of wanted kind
                  label = "# of desired hits",
                  step=1,
                  value = 3),
      
     # Input: slider for the number of draws ----
      sliderInput(inputId = "max_turns", # looks, or number of cards drawn from the deck
                  label = "# of turns",
                  min = 1,
                  max = 10,
                  value = 1),
     
     # Input: radio button for play or draw ---
     radioButtons(inputId = "on_the_draw", # if on the draw, player will look at one more card
                  label= "play or draw?",
                  choices = c("play" = FALSE,
                    "draw" = TRUE),
                  inline = TRUE
                  )

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: table ----
      tableOutput("table")
      
    )
  )
)


## Define server logic required to put together the table ----
server <- function(input, output) {
  
  # Table of the probability mass of drawing at least min_hits card and exactly min_hits cards
  # of the wanted type in turn 1, 2, ... (i.e. with 7, 8, .. looks) and having tot_hits and pop as input
  
  # The expression that generates table (matrix) is wrapped in a call
  # to renderTable which takes same kinds of input as xtable and return html code for the table
  output$table <- renderTable({
    
    myhyper <- function(min_hits, tot_hits, pop, looks){ # custom function to perform hypergeom. calculations
      
      exactly <- round(dhyper(min_hits, tot_hits, pop-tot_hits, looks), 3) # hypergeom. probability
      
      at_least <- round(dhyper(min_hits, tot_hits, pop-tot_hits, looks) 
                        -phyper(min_hits, tot_hits, pop-tot_hits, looks)+1, 3) # cumulative probability
      
      return(list(exactly, at_least))
    }
    
    # the rage of cards drawn depends on user's input about how many turns
    looks <- if (input$max_turns==1) {7} # turn 1 -> 7 drawn cards
             else {7:(6+input$max_turns)} # turn 1, 2, ..., n -> 7, 8, ..., 6+n drawn cards 
    turns <- as.character(looks-6)
    
    if (input$on_the_draw) {looks <- looks+1} # when on the draw, player looks at one more card
    
    probabilities <- myhyper(input$min_hits, input$tot_hits, input$pop, looks) # compute probabilities given inputs
    
    # return matrix
    matrix(c(turns, as.character(looks), unlist(probabilities[1]), unlist(probabilities[2])), # data
           ncol=4, dimnames=list(c(),
                                 c("turn",
                                   "cards",
                                   paste("exactly ", input$min_hits),
                                   paste("at least", input$min_hits)))) # names
    
  })
  
}

shinyApp(ui = ui, server = server)



