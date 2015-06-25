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
  print( thisRow );
  thesePats  = unlist(strsplit(thisRow[1], ","));
  thesePats  = gsub("^\\s+","",thesePats);
  thesePats  = gsub("\\s+$","",thesePats);
  return( data.frame(patient=thesePats, date=thisRow[2], place=thisRow[3], stringsAsFactors = FALSE) );
}

# Result is a list of data frames, stitch them together with a do.call to rbind
answerList = apply( workingDf, 1, uncombinePatientNames );
bufferDf = do.call(rbind, thisAnswer);
patDf = rbind(patDf, bufferDf);
rownames(patDf) = NULL;

# Convert to factor for faster comparison, we're done doing string manipulation
patDf$patient = as.factor( patDf$patient );
patDf$place   = as.factor( patDf$place   );

## Create adjacency matrix
patientMatrix = matrix(0, ncol=length(patients), nrow=length(patients) );
rownames( patientMatrix ) = patients;
colnames( patientMatrix ) = patients;

roomMatrix = matrix(0, ncol=length(rooms), nrow=length(rooms) );
rownames( roomMatrix ) = rooms;
colnames( roomMatrix ) = rooms;

for( thisPatient in patients) {
  hotRooms = unique( patDf$place[ patDf$patient == thisPatient]);
  hotRows  = patDf[ patDf$place %in% hotRooms, ];



  hotRows  = hotRows[ hotRows$patient != thisPatient, ];

}

# Define server logic
shinyServer(function(input, output) {

  # Define patient list
  output$choose_pats = renderUI({

    # Create checkboxes; have all ticked by default
    checkboxGroupInput("thesePats", "Select patients:", patients, patients)
  });

  ### Graph visualization
  output$adjGraph = renderPlot({
    thisAdj = patientMatrix[ patients %in% input$thesePats, patients %in% input$thesePats ];
    patientGraph <- graph.adjacency(thisAdj, mode="directed", weighted=TRUE);
    x = plot.igraph(patientGraph, edge.arrow.size=input$arrowSize, vertex.size = input$vertexSize, vertex.label.cex = input$labelSize);

    return(x);
  })

})