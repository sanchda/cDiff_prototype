# Load dependencies
library(igraph);
library(ggplot2);
library(shiny);
library(dplyr);
shiny:::setAutoflush(TRUE)
options(shiny.suppressMissingContextError=TRUE)

# Read .csv file, alter rownames to be the first column and throw out that column.
# NOTE that this .csv is currently manually preprocessed to fix header and bottom row.
# ASSUMPTION:  data is formatted with dates as column names and rooms as row names
#              elements consist of strings, which may look like e.g.:
#                - PATIENT
#                - PATIENT micro
#                - PATIENT1 , PATIENT2
#                - PATIENT1 micro , PATIENT2
roster=read.csv("cdiff_apr_2015.csv",quote="\"", stringsAsFactors=FALSE);

# Clear out blank rooms (formatting artifact)
roster = roster[ apply(roster, 1, function(x) sum( (x != "") + 0)) > 1,];
rownames(roster) = roster[,1];
roster = roster[,!(colnames(roster) %in% colnames(roster)[1])];

# Define a list of patients, clean it up, list which patients have received micro
patients = unique(unlist(apply(roster, 1, unique)));
patients = patients[ !(patients %in% "") ];
patients = unlist(strsplit(patients, ","));
patients = gsub("^\\s+","",patients);
patients = gsub("\\s+$","",patients);

hasMicro = unique(gsub("\\smicro$","",patients[ grep("^.+\\smicro$",patients) ]));

patients = sort( unique(gsub("\\smicro$","",patients)) );

rooms    = sort(unique( rownames(roster) ) );


# unzip the input df into a 3-column structure, describing each "patient", room, and date.
# Null out the empty-string-patients
# Iterate through the remainder and use that to populate desired data frame
dateMat = matrix( colnames(roster), nrow = nrow(roster), ncol = ncol(roster), byrow=TRUE);
placeMat= matrix( rownames(roster), nrow = ncol(roster), ncol = nrow(roster), byrow=TRUE);
patMat  = matrix( unlist(roster),   nrow = nrow(roster), ncol = ncol(roster) );

patDf = data.frame( patient=as.vector(patMat), date=as.vector(dateMat), place=as.vector(t(placeMat)), stringsAsFactors = FALSE);
patDf = patDf[ patDf$patient != "", ];

# Dereference dates into days since first date
# ASSUMPTION:  dates don't skip a day!
# TODO:  parse actual dates?
# Don't iterate through individual occurrences of a lookup, R is vectorized!
# Iterate through classes!
dateNames = sort( unique( patDf$date ) );
dateDays  = length(dateNames):1;
for( i in 1:length(dateNames) ) {
  patDf$date[ patDf$date == dateNames[i] ] = dateDays[i];
}

# Clean up patient names to drop "micro;" it isn't used anymore
patDf$patient = gsub(" micro", "", patDf$patient);

# Some rooms have multiple patients; split them apart
# Create a buffer dataframe of same type as original
replaceIdx  = grep(",", patDf$patient);
workingDf = patDf[  replaceIdx, ];
bufferDf  = patDf[ 0, ];
patDf     = patDf[ !((1:nrow(patDf)) %in% replaceIdx),];  # Throw out the ones we just grabbed
# Is there a better way?

# Addicted to the apply idiom, so create a function to thread over the rows.
# Should be faster than just iterating through rows with for
uncombinePatientNames = function(thisRow) {
  thesePats  = unlist(strsplit(thisRow[1], ","));
  thesePats  = gsub("^\\s+","",thesePats);
  thesePats  = gsub("\\s+$","",thesePats);
  return( data.frame(patient=thesePats, date=thisRow[2], place=thisRow[3], stringsAsFactors = FALSE) );
}

# Result is a list of data frames, stitch them together with a do.call to rbind
answerList = apply( workingDf, 1, uncombinePatientNames );
bufferDf = do.call(rbind, answerList);
patDf = rbind(patDf, bufferDf);
rownames(patDf) = NULL;

# Convert to factor for faster comparison, we're done doing string manipulation
patDf$patient = as.factor( patDf$patient );
patDf$place   = as.factor( patDf$place   );
patDf$date    = as.numeric( patDf$date );

## Create pseudo-adjacency matrix
# numbers indicate days
patientMatrix = matrix(Inf, ncol=length(patients), nrow=length(patients) );
rownames( patientMatrix ) = patients;
colnames( patientMatrix ) = patients;

roomMatrix = matrix(0, ncol=length(rooms), nrow=length(rooms) );
rownames( roomMatrix ) = rooms;
colnames( roomMatrix ) = rooms;

# Ugh...  This needs to be cleaned up.  The patient x room space isn't as big
# as the rowxrow space, but still.
repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}

repmatVec = function(Y,m,n){
  X = as.matrix(Y);
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}

dayFunc = function(X) {
  Y = X[ X > -1 ];
  if( length(Y) == 0) {
    return( NA );
  } else {
    return( min(Y) );
  }
}

for( thisPatient in patients) {
  hotRooms = unique( patDf$place[ patDf$patient == thisPatient]);
  hotRows  = patDf[ patDf$place %in% hotRooms, ];

  for( thisRoom in hotRooms ) {
    patDates    = hotRows[ hotRows$patient == thisPatient & hotRows$place == thisRoom, 2];
    roomSubset  = hotRows[ hotRows$patient != thisPatient & hotRows$place == thisRoom,];
    roomDates   = roomSubset$date;

    # If either of the dates are zero, then bail out
    if( length( patDates ) == 0 | length( roomDates ) == 0 ) {
      next;
    }

    # Construct an array of the possible date differences.  This is probably overkill,
    # but affords flexibility if I want to create rules later
    # You only care about the row index of hits.  The row tells you which patient it was.
    # Positive values indicate that thisPatient was in the given room after.
    timeMat = repmatVec(t(patDates), length(roomDates), 1) - repmatVec(roomDates, 1, length(patDates));

    # NA if patient was not in a given room before thisPatient, number of days otherwise
    patWeights = apply( timeMat, 1, dayFunc);
    updateDf = data.frame(days=patWeights, patient=roomSubset$patient );

    # Update the pseudo-adjacency matrix
    for( thatPatient in updateDf$patient ) {

      # Do this the lazy way; reference updateDf, which may have multiple entries for the same patient.
      # so choose the min (updateDf ensured to be non-negative).
      thisDays = min( updateDf[ updateDf$patient == thatPatient, 1 ] );

      if( ! is.na( thisDays ) ) {
        thatDays = patientMatrix[ colnames( patientMatrix) == thisPatient, colnames( patientMatrix) == thatPatient];

        if( thisDays < thatDays ) {
          patientMatrix[ colnames( patientMatrix) == thisPatient, colnames( patientMatrix) == thatPatient] = thisDays;
        }
      }
    }

  }
}

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

    patMatBuild = matrix(Inf, ncol=length(patList), nrow=length(patList) );
    rownames( patMatBuild ) = patList;
    colnames( patMatBuild ) = patList;

    roomMatrix = matrix(0, ncol=length(rooms), nrow=length(rooms) );
    rownames( roomMatrix ) = rooms;
    colnames( roomMatrix ) = rooms;

    for( thisPatient in unique( thisDf$patient ) ) {
      hotRooms = unique( thisDf$place[ thisDf$patient == thisPatient] );
      hotRows  = thisDf[ thisDf$place %in% hotRooms, ];

      for( thisRoom in hotRooms ) {
        patDates    = hotRows[ hotRows$patient == thisPatient & hotRows$place == thisRoom, 2];
        roomSubset  = hotRows[ hotRows$patient != thisPatient & hotRows$place == thisRoom,  ];
        roomDates   = roomSubset$date;

        # If either of the dates are zero, then bail out
        if( length( patDates ) == 0 | length( roomDates ) == 0 ) {
          next;
        }

        # Construct an array of the possible date differences.  This is probably overkill,
        # but affords flexibility if I want to create rules later
        # You only care about the row index of hits.  The row tells you which patient it was.
        # Positive values indicate that thisPatient was in the given room after.
        timeMat = repmatVec(t(patDates), length(roomDates), 1) - repmatVec(roomDates, 1, length(patDates));

        # NA if patient was not in a given room before thisPatient, number of days otherwise
        patWeights = apply( timeMat, 1, dayFunc);
        updateDf = data.frame(days=patWeights, patient=roomSubset$patient );

        # Update the pseudo-adjacency matrix
        for( thatPatient in updateDf$patient ) {

          # Do this the lazy way; reference updateDf, which may have multiple entries for the same patient.
          # so choose the min (updateDf ensured to be non-negative).
          thisDays = min( updateDf[ updateDf$patient == thatPatient, 1 ] );

          if( ! is.na( thisDays ) ) {
            thatDays = patMatBuild[ colnames( patMatBuild) == thisPatient, colnames( patMatBuild) == thatPatient];

            if( thisDays < thatDays ) {
              patMatBuild[ rownames( patMatBuild) == thisPatient, colnames( patMatBuild) == thatPatient] = thisDays;
            }
          }
        }

      }
    }

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
      "Arrange by",
      choices = var_choices,
      selected = "name"
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
      selected = "walktrap_comm"
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
    red2blue = colorRampPalette(c('red','blue'))
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
    patList  = input$targetPatients;
    roomList = input$roomList;
    thisDf   = patDf[ patDf$patient %in% patList, ];
    thisDf   = thisDf[ thisDf$place %in% roomList, ];
    thisDf$date = max(thisDf$date) - thisDf$date + 1;
    thisDf$nDate = thisDf$date + 1;

    ggplot(thisDf, aes(colour=patient)) +
      geom_segment(aes(x=date, xend=nDate, y=place, yend=place), size=3) +
      xlab("Dates") +
      scale_x_continuous(breaks=seq(0, 30, 1))  # Ticks from 0-10, every .25
  })

  ### Room summary

  ### Adjacency matrix
  graph = reactive({
    thisAdj = patMat();
    thisAdj[ thisAdj > input$nDays ] = 0;
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