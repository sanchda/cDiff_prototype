# Generic data importer
getDataFromCSV = function(thisFileName) {
  # Read .csv file, alter rownames to be the first column and throw out that column.
  # NOTE that this .csv is currently manually preprocessed to fix header and bottom row.
  # ASSUMPTION:  data is formatted with dates as column names and rooms as row names
  #              elements consist of strings, which may look like e.g.:
  #                - PATIENT
  #                - PATIENT micro
  #                - PATIENT1 , PATIENT2
  #                - PATIENT1 micro , PATIENT2
  roster=read.csv(thisFileName,quote="\"", stringsAsFactors=FALSE);
  roster[1,] = gsub("/","_",roster[1,]); # Reformat dates, which came in on first actual data field
  colnames(roster) = roster[1,];         # Rename cols
  roster = roster[ 2:nrow(roster),];     # Get rid of unnecessary date lines

  roster[,1] = iconv(roster[,1] , "latin1", "ASCII", sub="");
  roster = roster[ !( roster[,1] %in% ""),]; # Get rid of rows with blank room names

  topNames = c("Emma", "Olivia", "Noah", "Sophia", "Liam", "Mason", "Isabella", "Jacob", "William", "Ethan", "Ava", "Michael", "Alexander", "James", "Daniel", "Elijah", "Benjamin", "Logan", "Mia", "Aiden", "Jayden", "Matthew", "Emily", "Jackson", "Lucas", "David", "Joseph", "Abigail", "Anthony", "Andrew", "Samuel", "Gabriel", "Joshua", "John", "Carter", "Luke", "Dylan", "Christopher", "Madison", "Charlotte", "Isaac", "Harper", "Sofia", "Avery", "Elizabeth", "Oliver", "Henry", "Sebastian", "Caleb", "Owen");

  # Clear out blank rooms (formatting artifact)
  roster = roster[ apply(roster, 1, function(x) sum( (x != "") + 0)) > 1,];
  rownames(roster) = roster[,1];
  roster = roster[,!(colnames(roster) %in% colnames(roster)[1])];


  # unzip the input df into a 3-column structure, describing each "patient", room, and date.
  # Null out the empty-string-patients
  # Iterate through the remainder and use that to populate desired data frame
  dateMat = matrix( colnames(roster), nrow = nrow(roster), ncol = ncol(roster), byrow=TRUE);
  placeMat= matrix( rownames(roster), nrow = nrow(roster), ncol = ncol(roster), byrow=FALSE);
  patMat  = matrix( unlist(roster),   nrow = nrow(roster), ncol = ncol(roster) );

  patDf = data.frame( patient=as.vector(patMat), date=as.vector(dateMat), place=as.vector(placeMat), stringsAsFactors = FALSE);
  patDf = patDf[ patDf$patient != "", ];
  rownames(patDf) = NULL;

  # Dereference dates into days of the month
  patDf$date = gsub(".+_(.+)_.+","\\1",patDf$date);

  # Some rooms have multiple patients; split them apart
  # Create a buffer dataframe of same type as original
  patCommaIdx  = grep(",", patDf$patient);
  patCommaDf   = patDf[  patCommaIdx, ];
  patBufferDf  = patDf[ 0, ];
  patDf        = patDf[ -patCommaIdx,];  # Throw out the ones we just grabbed
  # Is there a better way?


  # Addicted to the apply idiom, so create a function to thread over the rows.
  # Should be faster than just iterating through rows with for
  uncombinePatientNames = function(thisRow) {
    thesePats  = unlist(strsplit(thisRow[1], ","));
    return( data.frame(patient=thesePats, date=thisRow[2], place=thisRow[3], stringsAsFactors = FALSE) );
  }

  # Result is a list of data frames, stitch them together with a do.call to rbind
  answerList  = apply( patCommaDf, 1, uncombinePatientNames );
  patBufferDf = do.call(rbind, answerList);
  patDf       = rbind(patDf, patBufferDf);
  rownames(patDf) = NULL;

  # Get rid of leading and trailing whitespace
  patDf$patient = gsub("^\\s","",patDf$patient);
  patDf$patient = gsub("\\s$","",patDf$patient);
  rownames(patDf) = NULL;

  # Subset the DF by presence of micro
  microDf = unique( patDf[ grep(" micro", patDf$patient), c("patient","date")] );
  rownames(microDf) = NULL;

  # Clean up patient names to drop "micro;" it isn't used anymore
  patDf$patient   = gsub(" micro", "", patDf$patient);
  microDf$patient = gsub(" micro", "", microDf$patient);



  # Convert to factor for faster comparison, we're done doing string manipulation
  patDf$patient = as.factor( patDf$patient );
  patDf$place   = as.factor( patDf$place   );
  patDf$date    = as.numeric( patDf$date );

  microDf$patient = as.factor( microDf$patient );
  microDf$date = as.numeric( microDf$date );

  # Return a list
  thisOutput = list();
  thisOutput[["patDf"]]   = patDf;
  thisOutput[["microDf"]] = microDf;

  return( thisOutput );
}




# Ugh...  This needs to be cleaned up.  The patient * room space isn't as big
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

getMatrix = function(patDf, microDf, patients, incTime) {

  # Set up output container
  patientMatrix = matrix(Inf, ncol=length(patients), nrow=length(patients) );
  rownames( patientMatrix ) = patients;
  colnames( patientMatrix ) = patients;


  for( thisPatient in patients) {
    # Find the subset of rooms inhabited by a given patient
    hotRooms = unique( patDf$place[ patDf$patient == thisPatient]);
    hotRows  = patDf[ patDf$place %in% hotRooms, ];

    # Figure out if/when patient received positive micro
    if( any( microDf$patient %in% thisPatient ) ) {
      # Received positive micro, so take the first day and subtract innoculation
      minDay = min( microDf[ microDf$patient %in% thisPatient, "date" ] );
      minDay = minDay - incTime;
      if( minDay < 1 ) {
        minDay = 1;
      }
      hotRows = hotRows[ hotRows$date >= minDay, ];

    } else {
      # Did not receive positive micro during the time period, so don't do anything.

    }

    # Check to make sure that hotRows is not empty.  If it is, then go to next patient
    if( nrow( hotRows ) < 1 ) {
      next;
    }

    # Subset the room space again, in case we lost any rooms by losing patients
    hotRooms = unique( hotRows$place);


    # Go through each room the patient inhabited.  We dropped dates during which we assumed the
    # patient was non-infectious, so each room has a list of dates the patient could have passed
    # their disease on to the next person.
    for( thisRoom in hotRooms ) {
      # Dates the patient was in the room
      patDates    = hotRows[ hotRows$patient == thisPatient & hotRows$place == thisRoom, 2];

      # Rows corresponding to other patients in the room
      otherPatRows = hotRows[ hotRows$patient != thisPatient & hotRows$place == thisRoom,];
      otherPatDates= otherPatRows$date;

      # If either of the date vectors are zero, then either the patient did not really stay
      # in that room or there are no other patients in that room.  In which case, skip.
      if( length( patDates ) == 0 | length( otherPatDates ) == 0 ) {
        next;
      }

      # Construct an array of the possible date differences.  This is probably overkill,
      # but affords flexibility if I want to create rules later
      # You only care about the row index of hits.  The row tells you which patient it was.
      # Positive values indicate that thisPatient was in the given room after.
      # Each element of timeMat is a thisPatient's date MINUS a thatPatient's date
      #
      # NOTICE:  this is the COMPLETE lookup; it can be decreased by exploiting presorted
      # dates.
      timeMat = repmatVec(t(patDates), length(otherPatDates), 1) - repmatVec(otherPatDates, 1, length(patDates));

      # NA if a patient was not in a given room BEFORE thisPatient, min number of days otherwise
      patWeights = apply( timeMat, 1, dayFunc);
      updateDf = data.frame(days=patWeights, patient=otherPatRows$patient );

      # Update the pseudo-adjacency matrix
      # Iterate through th econnected patients, populate output matrix with min difference
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
  # Nothing, but you could do
  #   patientMatrix[ patientMatrix==Inf ] = 0;


  # Return the patient matrix
  return( patientMatrix );

}