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


## Create a data frame which is indexed by patient and room, whose elements are vectors of dates.
# First, unzip the dataframe into a 3-column matrix, describing each "patient", room, and date.
# Null out the empty-string-patients
# Iterate through the remainder and use that to populate desired data frame
dateMat = matrix( colnames(roster), nrow = nrow(roster), ncol = ncol(roster) );
placeMat= matrix( rownames(roster), nrow = ncol(roster), ncol = nrow(roster) );
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

# Addicted to the apply idiom...
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

# Convert to factor for easier comparison.
patDf$patient = as.factor( patDf$patient );
patDf$place   = as.factor( patDf$place   );




patMat = as.matrix( roster );
colnames( patMat ) = colnames( roster );
rownames( patMat ) = rownames( roster );

# Process patient lookup leading to adjacency matrix
# Pick a patient.  Subset table by rows containing that patient.
# Let a first-order interaction be:
#     - patient1 was the patient immediately before patient2 in a room
#     - patient1 was the patient immediately after  patient2 in a room
#     - patient1 was in the room at the same time as patient2

getPatientProx = function(thisInputRow, thisPatient) {
  # This will get rid of the occurrence of individual patients in combinations.
#  repl1 = sprintf(" , %s (micro)*", thisPatient);
#  repl2 = sprintf("%s (micro)*, ", thisPatient);
#  if( is.null( nrow(thisInputRow) ) ) {
#    thisRow = as.data.frame(apply( thisInputRow, 1, function(x) gsub(repl2, "", gsub(repl1, "", x)) ), stringsAsFactors =FALSE);
#    print( thisInputRow );
#  } else {
#    thisRow = as.data.frame(apply( thisInputRow, c(1,2), function(x) gsub(repl2, "", gsub(repl1, "", x)) ), stringsAsFactors =FALSE);
#  }
thisRow = thisInputRow;
  # Standard column index; consumes overhead, but whatever
  colIdx = 1:length(thisRow);

  # Identifies positions in which there is any patient
  allIdx = colIdx[ (thisRow != "") ];

  # Identifies positions in which there is a non-target, non-blank patient
  patIdx = colIdx[ !grepl("^\\s*$", thisRow) & (!grepl(thisPatient, thisRow) | grepl(",", thisRow) ) ];

  # Identifies positions in which there is the target patient
  trgIdx = colIdx[ grepl(thisPatient, thisRow) ];

  # Identifies when the target and another guy were in the same room at the same time
#  sIdx = allIdx[ patIdx %in% trgIdx ];
#  if( length( sIdx ) == 0 ) {
#    sPats = character(0);
#  } else {
#    sPats = unlist( thisRow[ sIdx ] );

#    sPats = unlist(strsplit(sPats, ","));
#    sPats = gsub("^\\s+","",sPats);
#    sPats = gsub("\\s+$","",sPats);
#    sPats = unique(gsub("\\smicro$","",sPats));
#  }

  # Find the left-patients
  dayIdx = length(thisRow):1;
  dayIdx = dayIdx[ allIdx ];
#  lDays  = dayIdx[ c( tail(allIdx %in% trgIdx, -1), FALSE) ] - dayIdx[ allIdx %in% trgIdx ];
  lIdx = allIdx[ c( tail(allIdx %in% trgIdx, -1), FALSE) ];
  if( length( lIdx ) == 0 ) {
    lPats = character(0);
    lDays = numeric(0);
  } else {
    lPats = unlist( thisRow[ lIdx ] );
#    lDays = unlist( lDays );

    lPats = unlist(strsplit(lPats, ","));
    lPats = gsub("^\\s+","",lPats);
    lPats = gsub("\\s+$","",lPats);
    lPats = unique(gsub("\\smicro$","",lPats));

    if( length(lPats) != length(lDays)) {
      print("NOPE.");
    }

    # Expand lDays according to multi-patient days
#    str_count(XX, ",");
  }

  # Find the right-patients
  # Not needed, since every r-patient has this patient as an l-patient
#  rIdx = allIdx[ c(FALSE, head(allIdx %in% trgIdx, -1) ) ];
#  if( length( rIdx ) == 0 ) {
#    rPats = character(0);
#  } else {
#    rPats = unlist( thisRow[ rIdx ] );
#
#    rPats = unlist(strsplit(rPats, ","));
#    rPats = gsub("^\\s+","",rPats);
#    rPats = gsub("\\s+$","",rPats);
#    rPats = unique(gsub("\\smicro$","",rPats));
#  }

  outList = list();
#  outList[["samePatients"]]  = sPats;
#  outList[["rightPatients"]] = rPats;
  outList[["leftPatients"]]  = lPats;
#  outList[["leftDays"]] = lDays;

  return(outList);
}

patList = list();
for( thisPatient in patients) {
  hotRoster = roster[ apply(roster, 1, function(x) sum( grep(thisPatient, x) ) > 1), ];
  patList[[thisPatient]] = apply( hotRoster, 1, function(x) getPatientProx(x, thisPatient) );
}

# I give up...
adjVals = unlist( patList );
adjKeys = attr( adjVals, "names");
attr( adjVals, "names" ) = NULL;

patientMatrix = matrix(0, ncol=length(patients), nrow=length(patients) );
rownames( patientMatrix ) = patients;
colnames( patientMatrix ) = patients;

for( thisPatient in patients) {
  lNameExpr = sprintf("%s.+leftPatient", thisPatient);
  rNameExpr = sprintf("%s.+rightPatient", thisPatient);
  lIdx  = grep( lNameExpr, adjKeys );
  lPats = unique( adjVals[ lIdx ] );
  lPats = lPats[ !( lPats %in% thisPatient )];

  patientMatrix[ patients %in% thisPatient, patients %in% lPats ] = -1.0;

  rIdx  = grep( rNameExpr, adjKeys );
  rPats = unique( adjVals[ rIdx ] );
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