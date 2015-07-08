library(shiny)

# Define UI, based loosely on one of Shiny's templates
shinyUI(fluidPage(

  ### Title ###
  titlePanel("Prototype C. diff patient viewer"),

  ### TOP PANEL ###
  tabsetPanel(
  tabPanel("Environment Configuration",
           uiOutput("allPatsUI")
  ),

  tabPanel("Analysis",

    ### SIDE PANEL ###
    fluidRow(
    column(3,

      # Simulation configuration
      tabsetPanel(
        tabPanel("Connectivity",
          sliderInput("nDisplay", "Neighborhood order",
            min = 1,
            max = 10,
            value = 2,
            step = 1),

          sliderInput("nDays", "Maxium Days",
            min = 1,
            max = 30,
            value = 2,
            step = 1),

          uiOutput("targetInfectedDate")
        ),

        tabPanel("Target",
           uiOutput("targetPatientsUI")
        ),

        tabPanel("Viz",

          sliderInput("heightPixels", "Plot size (height)",
            min = 50,
            max = 2000,
            value = 400,
            step = 25),

          sliderInput("widthPixels", "Plot size (width)",
            min = 50,
            max = 1000,
            value = 500,
            step = 25),

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
           "Graph theory provides a convenient abstraction for structuring the relationship between individual patients.  The graphs below have patients as their nodes.  An arrow is drawn from patient A to patient B if patient A was observed to be positive for C. Diff and stayed in a given room prior to patient B.",
           "In order to improve interpretability, three context-modifying options are given.",
           "The patient selector on the left allows you to choose which patients MUST be included in the graph",
           "The degree slider modifies whether to include only the patients who shared rooms with a target patient",
           "The days slider affects the maximum number of days after which a potential interaction is not counted.",
         uiOutput("graphDisplayOutput")
        ),
        tabPanel("Patient Summary",
           uiOutput("patDisplayOutput")
        ),

        tabPanel("Adjacency Matrix",
          fluidRow(
          column(6,
           uiOutput("ordering_choices"),
           uiOutput("comm_choices")
          ),
          column(6,
            checkboxInput("alpha_weight", "Set alpha by edge weight", FALSE)
          )),
          annotate <- conditionalPanel(
            condition = "output.annotate_vars",
            checkboxInput("ann_var", "Annotate plot by node attribute sorting", FALSE)
          ),
          uiOutput("adjDisplayOutput")
        )
      )
    )
  )
  ))
))