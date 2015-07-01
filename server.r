# Load dependencies
library(igraph);
library(shiny);

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

patients = unique(gsub("\\smicro$","",patients))

rooms    = unique( rownames(roster) );


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

patAdjList = function(adjList, n) {
  if( n < 1 ) {
    return( adjList );
  } else {
    thesePats = patientMatrix[ patients %in% adjList, ];
    if( is.null( nrow( thesePats ) ) ) {
      theseHits = thesePats > 0;
    } else {
      theseHits = apply(thesePats,2,sum) > 0;
    }
    return( patAdjList( c(patients[ theseHits ], adjList ), n-1 ) );
  }
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



  # Define patient list
  output$choose_pats = renderUI({

    # Create checkboxes; have all ticked by default
    selectInput("targetPatients", "Target patient:", choices=patients, selected=patients[1])

  });

  ### Graph visualization
  output$adjGraph = renderPlot({
    thesePats = patAdjList( input$targetPatients, input$nDisplay );
    thisAdj = patientMatrix[ patients %in% thesePats, patients %in% thesePats ];
    thisAdj[ thisAdj > input$nDays ] = 0;
    thisAdj[ thisAdj > 0 ] = 1/thisAdj[ thisAdj > 0];

    thisAdj = thisAdj[ apply( thisAdj, 1, sum) > 0, apply( thisAdj, 1, sum) > 0 ];

    patientGraph =  graph.adjacency(thisAdj, mode="directed", weighted=TRUE);
#    x = plot.igraph(patientGraph, edge.arrow.size=input$arrowSize, vertex.size = input$vertexSize, vertex.label.cex = input$labelSize);

    thisLayout = layout.kamada.kawai( patientGraph );

    x = plot.igraph(patientGraph,
                    vertex.label=V(patientGraph)$name,
                    layout=thisLayout,
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


})