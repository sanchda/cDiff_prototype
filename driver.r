# Load dependencies
library(igraph);

# Read .csv file, alter rownames to be the first column and throw out that column.
# NOTE that this .csv is currently manually preprocessed to fix header and bottom row.
roster=read.csv("cdiff_apr_2015.csv",quote="\"", stringsAsFactors=FALSE);

# Clear out blank rooms
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


# Process patient lookup leading to adjacency matrix
# Pick a patient.  Subset table by rows containing that patient.
# Let a first-order interaction be:
#     - patient1 was the patient immediately before patient2 in a room
#     - patient1 was the patient immediately after  patient2 in a room
#     - patient1 was in the room at the same time as patient2

getPatientProx = function(thisRow, thisPatient) {
  # Standard column index; consumes overhead, but whatever
  colIdx = 1:length(thisRow);
  
  # Identifies positions in which there is any patient
  allIdx = colIdx[ (thisRow != "") ];
  
  # Identifies positions in which there is a non-target, non-blank patient
  patIdx = colIdx[ !grepl("^\\s*$", thisRow) & (!grepl(thisPatient, thisRow) | grepl(",", thisRow) ) ];
  
  # Identifies positions in which there is the target patient
  trgIdx = colIdx[ grepl(thisPatient, thisRow) ];
  
  # Identifies when the target and another guy were in the same room at the same time
  sIdx = allIdx[ patIdx %in% trgIdx ];
  if( length( sIdx ) == 0 ) {
    sPats = character(0);
  } else {
    sPats = unlist( thisRow[ sIdx ] );
    
    sPats = unlist(strsplit(sPats, ","));
    sPats = gsub("^\\s+","",sPats);
    sPats = gsub("\\s+$","",sPats);
    sPats = unique(gsub("\\smicro$","",sPats));
  }
  
  # Find the left-patients
  lIdx = allIdx[ c( tail(allIdx %in% trgIdx, -1), FALSE) ];
  if( length( lIdx ) == 0 ) {
    lPats = character(0);
  } else {
    lPats = unlist( thisRow[ lIdx ] );
  
    lPats = unlist(strsplit(lPats, ","));
    lPats = gsub("^\\s+","",lPats);
    lPats = gsub("\\s+$","",lPats);
    lPats = unique(gsub("\\smicro$","",lPats));
  }
  
  # Find the right-patients
  rIdx = allIdx[ c(FALSE, head(allIdx %in% trgIdx, -1) ) ];
  if( length( rIdx ) == 0 ) {
    rPats = character(0);
  } else {
    rPats = unlist( thisRow[ rIdx ] );
    
    rPats = unlist(strsplit(rPats, ","));
    rPats = gsub("^\\s+","",rPats);
    rPats = gsub("\\s+$","",rPats);
    rPats = unique(gsub("\\smicro$","",rPats));
  }
  
  outList = list();
  outList[["samePatients"]]  = sPats;
  outList[["rightPatients"]] = rPats;
  outList[["leftPatients"]]  = lPats;
  
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
  
  patientMatrix[ patients %in% thisPatient, patients %in% lPats ] = 1.0;
  
  rIdx  = grep( rNameExpr, adjKeys );
  rPats = unique( adjVals[ rIdx ] );
}


# Visualize
patientGraph <- graph.adjacency(patientMatrix, mode="directed", weighted=TRUE);
plot.igraph(patientGraph, edge.arrow.size=0.5, vertex.size = 0.1, vertex.label.cex = 0.5);


plot.igraph(patientGraph, edge.arrow.size=0.5, vertex.size = 0.1, vertex.label.cex = 0.01)


