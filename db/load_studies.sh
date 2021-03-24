#
# Usage: load_datasets.sh #syntax to run without any parameters

eval "$(docopts -h - : "$@" <<EOF
Usage: load_datasets.sh [options] <argv>...

Options:
      --help     Show help options.
      --version  Print program version.
----
load_datasets 0.1

EOF
)"

#TODO: change the name to something else?
#TODO: remove some of the hard-coded paths, such as the name of the database.
#TODO: pass in the path to the database.
#TODO: I should not pass in src, seems strange. Maybe I should have a MOVEDB_SRC env variable?
#----
#---- Set up variables
#----

#TODO: Need to make these optional
#See here for details: https://github.com/docopt/docopts
#TODO: Also I might need to do raw/<id>/*.csv instead of <id>/raw/*.csv
#  Otherwise I can't pass in raw and clean folders seperately.
#  For now, just passing in parent csvdir. Later, need to pass in raw, clean, db 
#   as optional parameters

csvdir=${argv[0]}
#raw=${argv[0]}
#clean=${argv[0]}
#db=${argv[1]}

#-----------------------#
#---- Load datasets ----#
#-----------------------#

#See docs/notes.txt for notes about loading specific datasets

# the study.csv file needs to have, at minimum, a column called "study_id"
# and a column called "run". The script will ignore any other columns.

# Use miller to filter by run column and then take the study_id field
# need to use tail to remove first line, which is the header
studyIds=($(mlr --csv --opprint filter '$run == 1' then cut -f study_id ctfs/study.csv | tail -n +2))
#echo ${#studyIds[@]} #number of items in array
#echo ${studyIds[@]} #for some reason this is not printing all items


status=load_status.csv

mkdir -p logs

for studyId in "${studyIds[@]}"
do 
  echo "*******"
  echo "Start processing study ${studyId}"
  echo "*******"
  
  #seems newline character is in studyId. Causes load_status file to have line breaks.
  # Attempt to remove it.
  #https://unix.stackexchange.com/questions/57124/remove-newline-from-unix-variable/57128
  

  #Reading study ids from csv results in \r at end. This removes them.
  studyId=${studyId%$'\r'}
  
  raw="${csvdir}/${studyId}/raw"
  clean="${csvdir}/${studyId}/clean"

  #------------------#
  #---- Download ----#
  #------------------#
  
  echo "Downloading study ${studyId}"
  $MOVEDB_SRC/db/get_study.r $studyId -r $raw -t 2>&1 | tee logs/$studyid.log
  
  if [ $? -eq 0 ]; then
    echo "Successfully downloaded study"
    echo $studyId,download,success >> $status
  else
    echo "Failed to download study ${studyId}"
    echo $studyId,download,fail >> $status
    continue
  fi
  
  #---------------#
  #---- Clean ----#
  #---------------#
  echo "Cleaning study ${studyId}"
  $MOVEDB_SRC/db/clean_study.r ${studyId} -c $clean -r $raw -t 2>&1 | tee -a logs/$studyid.log
    
  if [ $? -eq 0 ]; then
    echo "Successfully cleaned study"
    echo $studyId,clean,success >> $status
  else
    echo "Failed to download data for study ${studyId}"
    echo $studyId,clean,fail >> $status
    continue
  fi
  
  #---------------#
  #---- Import ---#
  #---------------#
  echo "Importing study ${studyId}"
  $MOVEDB_SRC/db/import_study.r -i ${studyId} -c $clean -t 2>&1 | tee -a logs/$studyid.log
  
  #Attempted to insert a study that was already there. Script failed but $? -eq 0 was true
  if [ $? -eq 0 ]; then
    echo "Successfully imported data"
    echo $studyId,import,success >> $status
  else
    echo "Failed to import study ${studyId}"
    echo $studyId,import,fail >> $status
    continue
  fi
  
  #------------------#
  #---- Validate ----#
  #------------------#
  echo "Validating import for study ${studyId}"
  $MOVEDB_SRC/db/validate_import.r ${studyId} -c $clean -t 2>&1 | tee -a logs/$studyid.log
    
  if [ $? -eq 0 ]; then
    echo "Successfully validated import"
    echo $studyId,validate,success >> $status
  else
    echo "Failed to validate import for study ${studyId}"
    echo $studyId,validate,fail >> $status
    continue
  fi
  
done

#TODO
# Run analyze statement (or pgrama optimize?) on database

#Run this from bash:
#PRAGMA analysis_limit=400;
#PRAGMA optimize;

#Also look into vacuum
#https://www.sqlitetutorial.net/sqlite-vacuum/


