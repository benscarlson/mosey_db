src=~/projects/movebankdb/src
pd=~/projects/movebankdb/analysis/movebankdb

cd $pd

#-------------------------#
#---- Create database ----#
#-------------------------#

# Don't run this if database already exists!
# cat $src/db/create_db.sql | sqlite3 data/movebank.db

#-----------------------#
#---- Load datasets ----#
#-----------------------#

#TODO: Currently can't download using api
# LifeTrack White Stork SWGermany 2014-2018 Failed, maybe permission error
# LifeTrack White Stork Vorarlberg also failed
#TODO: #Look at this to see how to redirect stderr and stdout
#https://stat.ethz.ch/pipermail/r-help/2010-June/241789.html

out="/Volumes/WD4TB/projects/movebankdb/active"

# Use miller to filter by run column and then take the study_id field
# need to use tail to remove first line, which is the header
studyIds=($(mlr --csv --opprint filter '$run == 1' then cut -f study_id study.csv | tail -n +2))
#echo ${#studyIds[@]} #number of items in array
#echo ${studyIds[@]} #for some reason this is not printing all items

#TODO: run validate for studies in EFA04B7F-979B-41D7-8DA4-FB8904A98B92_success.txt
# I think last one did not load events due to issue reading event csv from disk
# read_csv gave a warning message related to the gps_fix_accuracy (or something) column
# more notes
#   662814098. this is the one that had the error and did not load any data

runid=`uuidgen`
sucf="${runid}_success.txt"
failf="${runid}_fail.txt"

for studyId in "${studyIds[@]}"
do 
  echo "*******"
  echo "Start processing study ${studyId}"
  echo "*******"
  
  studyId=662814098
  
  #Run download script, then if no errors run load and validate scripts
  $src/db/import/get_study_data.r $studyId "$out/${studyId}"
  
  if [ $? -eq 0 ]; then
    echo "Successfully downloaded data"

    echo "Loading data for study ${studyId}"
    $src/db/import/load_study_data.r ${studyId} "${out}/${studyId}"
  
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

#Database management

#Black Stork in Spain - Migra Program in Spain
$src/db/delete_study.r 682808477
rm -r ${out}/682808477
