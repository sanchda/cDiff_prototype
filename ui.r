library(shiny)

# Define UI, based loosely on one of Shiny's templates
shinyUI(fluidPage(
  
  ### SIDE PANEL ###
  fluidRow(
  column(3,
    titlePanel("Prototype Cdiff graph viewer"),
    
    # Simulation configuration
    tabsetPanel(
      tabPanel("Patients",
        uiOutput("choose_pats")
      ),
      tabPanel("Viz Parameters",
        numericInput("arrowSize", "arrow size",
          min = 0.0,
          max = 5.0,
          value = 0.1),
      
        numericInput("vertexSize", "vertex size",
          min = 0.0,
          max = 5.0,
          value = 0.1),
      
        numericInput("labelSize", "label size",
          min = 0.0,
          max = 5.0,
          value = 0.1)
      )
    )
  ),

  ### MAIN PANEL
  column(9,
    tabsetPanel(id="MAIN_PANEL",
      tabPanel("Graph Theory",
               "Some quick graphs.",
       plotOutput("adjGraph")
      )
    )
  )
)
))