

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
