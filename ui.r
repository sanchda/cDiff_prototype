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
        uiOutput("choose_pats"),
        sliderInput("nDisplay", "Neighborhood order",
          min = 1,
          max = 5,
          value = 2,
          step = 1),

        sliderInput("nDays", "Maxium Days",
          min = 1,
          max = 10,
          value = 2,
          step = 1)
      ),

      tabPanel("Viz Parameters",


        sliderInput("arrowSize", "arrow size",
          min = 0.0,
          max = 5.0,
          value = 1.0,
          step = 0.1),

        sliderInput("vertexSize", "vertex size",
          min = 10,
          max = 50.0,
          value = 10,
          step=0.1),

        sliderInput("labelSize", "label size",
          min = 0.0,
          max = 10.0,
          value = 1.0,
          step=0.1),

        sliderInput("weightCoeff", "edge weight",
          min = 0.05,
          max = 2,
          value = 0.1,
          step=0.05),

        sliderInput("margUp", "top margin",
          min = -1,
          max = 1,
          value = 0,
          step=0.05),

        sliderInput("margLeft", "left margin",
          min = -1,
          max = 1,
          value = 0,
          step=0.05),

        sliderInput("margBot", "bottom margin",
          min = -1,
          max = 1,
          value = 0,
          step=0.05),

        sliderInput("MargRight", "right margin",
          min = -1,
          max = 1,
          value = 0,
          step=0.05)

      )
    )
  ),

  ### MAIN PANEL
  column(9,
    tabsetPanel(id="MAIN_PANEL",
      tabPanel("Graph Theory",
               "Some quick graphs.",
       plotOutput(outputId = "adjGraph",  width = "100%")
      )
    )
  )
)
))