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
                  value = 3),
     
     # Input: radio button for play or draw ---
     radioButtons(inputId = "on_the_draw", # if on the draw, player will look at one more card
                  label= "play or draw?",
                  choices = c("play" = FALSE,
                    "draw" = TRUE),
                  inline = TRUE
                  )

     , width = 2),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: table and plot
      column(4,tableOutput("table")),
      column(6,plotOutput("plot"))
      
    )
  )
)


## Define server logic required to put together the table ----
server <- function(input, output) {
  
  myhyper <- function(min_hits, tot_hits, pop, looks){ # custom function to perform hypergeom. calculations
    exactly <- round(dhyper(min_hits, tot_hits, pop-tot_hits, looks), 3) # hypergeom. probability
    at_least <- round(dhyper(min_hits, tot_hits, pop-tot_hits, looks) 
                      -phyper(min_hits, tot_hits, pop-tot_hits, looks)+1, 3) # cumulative probability
    return(list(exactly, at_least))
  }
  
  # Table of the probability mass of drawing at least min_hits card and exactly min_hits cards
  # of the wanted type in turn 1, 2, ... (i.e. with 7, 8, .. looks) and having tot_hits and pop as input
  
  # The expression that generates table (matrix) is wrapped in a call
  # to renderTable which takes same kinds of input as xtable and return html code for the table
  output$table <- renderTable({
    
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
                                   paste("exactly", input$min_hits),
                                   paste("at least", input$min_hits)))) # names
    
  })
  
  # Something similar is required to output a plot as well
  output$plot <- renderPlot({
    
    # the rage of cards drawn depends on user's input about how many turns
    looks <- if (input$max_turns==1) {7} # turn 1 -> 7 drawn cards
    else {7:(6+input$max_turns)} # turn 1, 2, ..., n -> 7, 8, ..., 6+n drawn cards 
    turns <- looks-6
    if (input$on_the_draw) {looks <- looks+1} # when on the draw, player looks at one more card
    probabilities <- myhyper(input$min_hits, input$tot_hits, input$pop, looks) # compute probabilities given inputs
    
    # create data frame from the matrix, to use with ggplot
    df <- as.data.frame(matrix(c(turns, unlist(probabilities[1]), unlist(probabilities[2])), # data
           ncol=3, dimnames=list(c(),
                                 c("turn",
                                   "exactly",
                                   "at_least"))))
    
    # expression to output the plot
    ggplot(data=df,aes(x=turn))+ # basic settings: data and values for x-axis
      geom_point(aes(y=exactly, colour="blue", shape="blue"), size=4)+ # points for hyperg. prob. 
      geom_line(aes(y=exactly, colour="blue"))+ # ...and lines connecting them
      geom_point(aes(y=at_least, colour="red", shape="red"), size=4)+ # points for cumul. prob. 
      geom_line(aes(y=at_least, colour="red"))+ # ...and lines connecting them
      ylab("probability")+ # label for y-axis
      scale_x_continuous(limits = c(1,input$max_turns), breaks=seq(1,input$max_turns,1))+ # display all values on x-axis
      scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.1))+ # display all values on y-axis
      # legend
      scale_colour_manual(name="", 
                          values=c("red" = "red", "blue"="blue"),
                          labels=c("red"=paste("at least", input$min_hits), "blue"=paste("exactly", input$min_hits)))+
      scale_shape_manual(name="",
                         values = c("red"=15, "blue"=16),
                         labels=c("red"=paste("at least", input$min_hits), "blue"=paste("exactly", input$min_hits)))+
      # theme and visual adjustments
      theme_bw()+theme(panel.grid.major = element_blank(), # remove grids
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(), # and borders
                       axis.line = element_line(colour = "black"), # display axis
                       text=element_text(size=22), # size of text
                       legend.position="top" # legend positioning
                       )
    
  })
  
}

shinyApp(ui = ui, server = server)



