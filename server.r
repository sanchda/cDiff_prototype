# Load dependencies
library(igraph);
library(ggplot2);
library(shiny);
library(dplyr);
shiny:::setAutoflush(TRUE)
options(shiny.suppressMissingContextError=TRUE)

## Import data
source("global.R");
newDfs = getDataFromCSV("2015 CDIFF JUNE BED trace.csv");
patDf = newDfs[["patDf"]];
microDf = newDfs[["microDf"]];

patients = sort(as.character(unique(patDf$patient)));
rooms    = sort(as.character(unique(patDf$place)));


## Create pseudo-adjacency matrix
patientMatrix = getMatrix(patDf, microDf, unique(patDf$patient), 100);

# Finalize matrix
patientMatrix[ patientMatrix==Inf ] = 0;

# Define server logic
shinyServer(function(input, output) {
  #### Overview of server logic ####
  #
  # The graphics-facing data structure is an adjacency matrix defined in multiple steps.
  # Some reference steps are handled above.  From them:
  #   1.  The user selects a room filter, which is engaged first.  This is expensive as it is done before the pseudo-adjaceny matrix is created
  #   2.  A patient filter, which is engaged second
  #   3.  Finally, there are options to convert the pseudo-adjacency matrix to the adjacency matrix in different ways

  # Initialize appropriate variables
  patMat = reactive({

    patList  = input$patList;
    roomList = input$roomList;
    thisDf   = patDf[ patDf$patient %in% patList, ];
    thisDf   = thisDf[ thisDf$place %in% roomList, ];

    patMatBuild = getMatrix(thisDf, microDf, patList, input$mDays);

    patMatBuild[ patMatBuild==Inf ] = 0;
    patMatBuild[ row(patMatBuild) == col(patMatBuild) ] = 0;
    return( patMatBuild );
  })

  # Choose the overall list of patients and rooms
  output$allPatsUI = renderUI({
    # Create checkboxes; have all ticked by default
    return(
      fluidRow(
        column(6,   checkboxGroupInput("patList", "Included patients:", choices=patients, selected=patients, inline=FALSE) ),
        column(6,   checkboxGroupInput("roomList", "Included rooms:", choices=rooms, selected=rooms, inline=FALSE) )
      )
    )
  });

  # Choose the target patient
  output$targetPatientsUI = renderUI({
    # Create checkboxes; have all ticked by default
    checkboxGroupInput("targetPatients", "Target patients:", choices=input$patList, selected=input$patList[1], inline=FALSE)
  });

  # Populate a list of infection dates for the target patient
  output$targetInfectedDate = renderUI({


  });

  # Was the patient infected?  When?
  output$patMicroDates = renderText({
    thisMicro = microDf[ microDf$place %in% input$roomList ,];
    thisMicro = microDf[ microDf$patient %in% input$targetPatients, ];
    sprintf("Day(s) of positive micro results: %s", paste(thisMicro$date, collapse=", "))
  })

  # Generate a selection menu for ordering choices
  output$ordering_choices <- renderUI({
    base <- c(
      "name",
      "community"
    )

    dataset_names <- names(node_list())
    var_choices <- dataset_names[grep("comm", dataset_names, invert = TRUE)] %>% union(base)

    return(selectInput(
      "arr_var",
      "Sort patients by",
      choices = var_choices,
      selected = "community"
    ))
  })

  # Generate a selection menu for community detection choices
  output$comm_choices <- renderUI({

    dataset_names <- names(node_list())
    comm_choices <- dataset_names[grep("comm", dataset_names)]

    return(selectInput(
      "comm_var",
      "Community Algorithm",
      choices = comm_choices,
      selected = "optimal_comm"
    ))
  })

  annotate_vars <- reactive({

    dataset_names <- names(node_list())

    base <- c(
      "name",
      "degree",
      "closeness",
      "betweenness",
      "eigen",
      "walktrap_comm",
      "edge_comm",
      "optimal_comm",
      "spinglass_comm",
      "fastgreedy_comm"
    )

    return(setdiff(dataset_names, base))

  })


  ### Helper function for subsetting
  getPatAdjList = function(adjList, n) {
    # Exploits the fact that adjacency matrix powers correspond to existence of paths

    thisAdj = patMat();

    # thisAdj is weighted by number of days.  Use that to filter,
    # then renormalized to pure connectivity.
    thisAdj[ thisAdj > input$nDays ] = 0;
    thisAdj[ thisAdj > 0 ] = 1;
    diag(thisAdj) = 1;
    A = thisAdj;

    neighbors = list();

    # Find the correct rows
    # Sum up the columns of those rows
    # Eliminate rows whose sum is trivial (1)
    # Find the corresponding patient names
    if( length( adjList ) < 2 ) {
      matchedPats = colnames(A)[         A[ rownames(A) %in% adjList, ]  > 0 ];
    } else {
      matchedPats = colnames(A)[ colSums(A[ rownames(A) %in% adjList, ]) > 0 ];
    }

    if( length( matchedPats ) > 0 ) {
      neighbors[[1]] = data.frame(patients=matchedPats, distance=1);
    }

    # If a given row has a nonzero element in a given column
    # Then there is a connection between the two nodes
    if( n > 1 ) {
      for( i in 2:n ) {
        A = A %*% thisAdj;
        if( length( adjList ) < 2 ) {
          matchedPats = colnames(A)[         A[ rownames(A) %in% adjList,]  > 0 ];
        } else {
          matchedPats = colnames(A)[ colSums(A[ rownames(A) %in% adjList,]) > 0 ];
        }

        if( length( matchedPats ) > 0 ) {
          neighbors[[i]] = data.frame(patients=matchedPats, distance=i);
        }
      }
    }

    # Populate adjList entities for consistency and reusability
    neighbors[[n+1]] = data.frame(patients=adjList, distance=0);

    # adjOutput$patients are the names of the patients
    # adjOutput$distances   are the connectivity classes
    adjOutput = do.call(rbind, neighbors);

    # Keep only the entry with the lowest distance (might be multiple paths)
    adjOutput = arrange( adjOutput, patients, distance );
    adjOutput = adjOutput[ !duplicated( adjOutput$patients ), ];

    return( adjOutput );
  }

  adjMat = reactive({

    # Grab the list of patients for which there is an nDisplay-path from a target patient
    patAdjDf = getPatAdjList( input$targetPatients, input$nDisplay );
    thesePats = patAdjDf$patients;

    # Grab the distance-weighted adjacency matrix, patMat()
    thisAdj = patMat();

    # If day threshold is exceeded, drop it
    thisAdj[ thisAdj > input$nDays ] = 0;

    # Include only rows and columns which are a target patient or a connected patient
    # (target patients implicitly returned from getPatAdjList)
    thisAdj = thisAdj[ (rownames(thisAdj) %in% thesePats), (colnames(thisAdj) %in% thesePats) ];

    # Weight according to inverse of days since
    thisAdj[ thisAdj > 0 ] = 1/thisAdj[ thisAdj > 0];

    # The filtering above can strip out the matrix structure if the result is a scalar
    # Recast and rename.  Thanks R.
    if( class( thisAdj ) == "numeric" ) {
      thisAdj = as.matrix( thisAdj );
      colnames( thisAdj ) = thesePats;
      rownames( thisAdj ) = thesePats;
    }

    # Remove self-connections
    thisAdj[ row(thisAdj) == col(thisAdj) ] = 0;
    return( thisAdj );
  });

  ### Graph visualization
  output$adjGraph = renderPlot({
    thisAdj = adjMat();
    patAdjDf = getPatAdjList( input$targetPatients, input$nDisplay );

    patientGraph =  graph.adjacency(thisAdj, mode="directed", weighted=TRUE);

    # Create a color ramp function between blue and red
    red2blue = colorRampPalette(c('red','bisque'))
    theseColors = red2blue( max(patAdjDf$distance) + 1 );
    graphColors = theseColors[ patAdjDf$distance + 1 ];
    patAdjDf$colors = graphColors;

#    thisLayout = layout.kamada.kawai( patientGraph );

    # Set graph color attribute
    V(patientGraph)$color = patAdjDf$color[ match( V(patientGraph)$name, patAdjDf$patient ) ];

    x = plot.igraph(patientGraph,
                    vertex.label=V(patientGraph)$name,
#                    layout=thisLayout,
                    vertex.label.color="black",
                    edge.color="black",
                    edge.width=E(patientGraph)$weight*input$weightCoeff,
                    edge.arrow.size=input$arrowSize,
                    vertex.size = input$vertexSize,
                    vertex.label.cex = input$labelSize,
                    margin=c(input$margTop, input$magLeft, input$margBot, input$margRight)
    );

    return(x);
  })

  ### Patient summary
  output$patSummary = renderPlot({
    if( !input$patSummaryToggle ) {
      patAdjDf = getPatAdjList( input$targetPatients, input$nDisplay );
      patList  = patAdjDf$patient;
    } else {
      patList  = input$targetPatients;
    }

    roomList = input$roomList;
    thisDf   = patDf[ patDf$patient %in% patList, ];
    thisDf   = thisDf[ thisDf$place %in% roomList, ];
#    thisDf$date = max(thisDf$date) - thisDf$date + 1;
    thisDf$nDate = thisDf$date + 1;

    # If the overlap toggle is set, remove single-occurence rooms
    if( input$patOverlapToggle ) {
      # Make a list of the unique name-room combinations
      occStrings = unique(sprintf("%s___%s", thisDf$place, thisDf$patient));
      occStrings = gsub("___.+","",occStrings);
      occTable   = table(occStrings);
      occStrings = rownames(occTable)[occTable > 1];
      thisDf = thisDf[ thisDf$place %in% occStrings, ];
    }

    ggplot(thisDf, aes(colour=patient)) +
      geom_segment(aes(x=date, xend=nDate, y=place, yend=place), size=3) +
      xlab("Dates") +
      scale_x_continuous(breaks=seq(0, 30, 1))  # Ticks from 0-10, every .25
  })

  ### Room summary

  ### Adjacency matrix
  graph = reactive({
    thisAdj = patMat();
    thisAdj[ thisAdj > input$nDaysAdj ] = 0;
    thisAdj[ thisAdj > 0 ] = 1/thisAdj[ thisAdj > 0];

    thisAdj = thisAdj[ apply( thisAdj, 1, sum) > 0, apply( thisAdj, 1, sum) > 0 ];

    thisAdj[ row(thisAdj) == col(thisAdj) ] = 0;


    patientGraph =  graph.adjacency(thisAdj, mode="directed", weighted=TRUE)

    ### Shamelessly stolen
    ### http://matthewlincoln.net/2014/12/20/adjacency-matrix-plots-with-r-and-ggplot2.html
    # Calculate various network properties, adding them as attributes
    # to each node/vertex
#    V(patientGraph)$multilevel_comm <- membership(multilevel.community(patientGraph, weights = E(patientGraph)$weight));
    V(patientGraph)$degree <- degree(patientGraph);
    V(patientGraph)$closeness <- centralization.closeness(patientGraph)$res;
    V(patientGraph)$betweenness <- centralization.betweenness(patientGraph)$res;
    V(patientGraph)$eigen <- centralization.evcent(patientGraph)$vector;
    V(patientGraph)$optimal_comm <- membership(optimal.community(patientGraph));
    V(patientGraph)$walktrap_comm <- membership(walktrap.community(patientGraph));
    V(patientGraph)$edge_comm <- membership(edge.betweenness.community(patientGraph));

    return( patientGraph );
  })

  node_list = reactive({
    get.data.frame(graph(), what = "vertices")
  })

  edge_list = reactive({
    get.data.frame(graph(), what = "edges")
  })

  # List non-calculated node attributes
  annotate_vars <- reactive({

    dataset_names <- names(node_list())

    base <- c(
      "name",
      "degree",
      "closeness",
      "betweenness",
      "eigen",
      "walktrap_comm",
      "edge_comm",
      "optimal_comm",
      "spinglass_comm",
      "fastgreedy_comm",
      "multilevel_comm"
    )

    return(setdiff(dataset_names, base))

  })

  annotatable <- reactive({
    return(input$arr_var %in% annotate_vars())
  })

  output$annotate_vars <- reactive({annotatable()})
  outputOptions(output, "annotate_vars", suspendWhenHidden = FALSE)


  # Returns a character vector of the vertices ordered based on given variables
  ordering <- reactive({
    if(input$arr_var == "community") {
      return((node_list() %>% arrange_(input$comm_var))$name)
    } else {
      return((node_list() %>% arrange_(input$arr_var))$name)
    }
  })

  # Determine a community for each edge. If two nodes belong to the
  # same community, label the edge with that community. If not,
  # the edge community value is 'NA'
  coloring <- reactive({
    colored_edges <- edge_list() %>%
      inner_join(node_list() %>% select_("name", "community" = input$comm_var), by = c("from" = "name")) %>%
      inner_join(node_list() %>% select_("name", "community" = input$comm_var), by = c("to" = "name")) %>%
      mutate(group = ifelse(community.x == community.y, community.x, NA) %>% factor())
    return(colored_edges)
  })

  # Sort the edge list based on the given arrangement variable
  plot_data <- reactive({
    name_order <- ordering()
    sorted_data <- coloring() %>% mutate(
      to = factor(to, levels = name_order),
      from = factor(from, levels = name_order))
    return(sorted_data)
  })

  output$adjDisplay = renderPlot({
    # Define the graph
    patientGraph = graph();

    if(input$alpha_weight) {
      p <- ggplot(plot_data(), aes(x = from, y = to, fill = group, alpha = weight))
    } else {
      p <- ggplot(plot_data(), aes(x = from, y = to, fill = group))
    }

    p <- p + geom_raster() +
      theme_bw() +
      # Because we need the x and y axis to display every node,
      # not just the nodes that have connections to each other,
      # make sure that ggplot does not drop unused factor levels
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme(
        # Rotate the x-axis lables so they are legible
        axis.text.x = element_text(angle = 270, hjust = 0, size = 12),
        axis.text.y = element_text(size = 12),
        # Force the plot into a square aspect ratio
        aspect.ratio = 1,
        # Hide the legend (optional)
        legend.position = "bottom")

    # Annotate the plot based on preexisting node attributes
    if(annotatable() & input$ann_var) {

      # Determine the "first" and "last" members of a node group
      ordered_anns <- node_list() %>%
        group_by_(input$arr_var) %>%
        summarize(min = first(name), max = last(name)) %>%
        filter(min != max)

      ann_groups <- ordered_anns[[input$arr_var]]

      # For each node grouping, add an annotation layer
      for(val in ann_groups[!is.na(ann_groups)]) {

        # Retrieve the min and max value for the given group value
        ann_min <- ordered_anns[ordered_anns[, input$arr_var] == val, ][["min"]]
        ann_max <- ordered_anns[ordered_anns[, input$arr_var] == val, ][["max"]]

        p <- p + annotate(
          "rect",
          xmin = ann_min,
          xmax = ann_max,
          ymin = ann_min,
          ymax = ann_max,
          alpha = .1) +
          annotate(
            "text",
            label = val,
            x = ann_min,
            y = ann_max,
            hjust = 0
          )
      }
    }

    return(p)
  })


  ### Build output options
  hPx = reactive({
    sprintf("%dpx",input$heightPixels)
  });

  wPx = reactive({
    sprintf("%dpx",input$widthPixels)
  });

  output$adjDisplayOutput <- renderUI({
    plotOutput(outputId = "adjDisplay", width=wPx(), height=hPx())
  });

  output$patDisplayOutput <- renderUI({
    plotOutput(outputId = "patSummary", width=wPx(), height=hPx())
  });

  output$graphDisplayOutput <- renderUI({
    plotOutput(outputId = "adjGraph", width=wPx(), height=hPx())
  });



})