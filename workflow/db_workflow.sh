#Set pd and out based on specific project
#pd=~/projects/movebankdb/analysis/movebankdb
pd=~/projects/covid/analysis/movebankdb

#This is where csv files downloaded from movebank are staged prior to db import
#out="/Volumes/WD4TB/projects/movebankdb/active"
out="/Volumes/WD4TB/projects/covid"

src=~/projects/movebankdb/src

cd $pd

#-------------------------#
#---- Create database ----#
#-------------------------#

# Don't run this if database already exists!
# cat $src/db/create_db.sql | sqlite3 data/movebank.db

#-----------------------#
#---- Load datasets ----#
#-----------------------#

#See docs/notes.txt for notes about loading specific datasets

# Use miller to filter by run column and then take the study_id field
# need to use tail to remove first line, which is the header
studyIds=($(mlr --csv --opprint filter '$run == 1' then cut -f study_id ctfs/study.csv | tail -n +2))
#echo ${#studyIds[@]} #number of items in array
#echo ${studyIds[@]} #for some reason this is not printing all items

runid=`uuidgen`
sucf="${runid}_success.txt"
failf="${runid}_fail.txt"

for studyId in "${studyIds[@]}"
do 
  echo "*******"
  echo "Start processing study ${studyId}"
  echo "*******"
  
  #studyId=474651680
  
  #Run download script, then if no errors run load and validate scripts
  $src/db/get_study_data.r $studyId "$out/${studyId}" -a input
  
  if [ $? -eq 0 ]; then
    echo "Successfully downloaded data"

    echo "Loading data for study ${studyId}"
    $src/db/load_study_data.r ${studyId} "${out}/${studyId}"
  
    echo "Validating load for study ${studyId}"
    $src/db/import/validate_import.r ${studyId} "${out}/${studyId}"
    
    echo ${studyId} >> $sucf
  else
    echo "Failed to download data for study ${studyId}"
    echo ${studyId} >> $failf
  fi
done

#TODO
# Run analyze statement (or pgrama optimize?) on database

#Run this from bash:
#PRAGMA analysis_limit=400;
#PRAGMA optimize;

#Also look into vacuum
#https://www.sqlitetutorial.net/sqlite-vacuum/


