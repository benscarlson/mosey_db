src=~/projects/movebankdb/src
pd=~/projects/movebankdb/analysis/movebankdb

cd $pd

datP="/Volumes/WD4TB/projects/movebankdb/active"

# Use miller to filter by run column and then take the study_id field
# need to use tail to remove first line, which is the header
studyIds=($(mlr --csv --opprint filter '$run == 1' then cut -f study_id study.csv | tail -n +2))
#echo ${#studyIds[@]} #number of items in array
#echo ${studyIds[@]} #for some reason this is not printing all items

for studyId in "${studyIds[@]}"
do 
  
  echo "Validating load for study ${studyId}"
  $src/db/import/validate_import.r ${studyId} "${datP}/${studyId}"

done

#TODO
# Run analyze statement (or pgrama optimize?) on database

#Run this from bash:
#PRAGMA analysis_limit=400;
#PRAGMA optimize;

#Also look into vacuum
#https://www.sqlitetutorial.net/sqlite-vacuum/
