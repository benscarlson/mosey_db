#
# Usage: load_datasets.sh #syntax to run without any parameters

eval "$(docopts -h - : "$@" <<EOF
Usage: load_datasets.sh [options] ...

Options:
      --help     Show help options.
      --version  Print program version.
      --csvdir=<csvdir> Path to store raw and clean csv directories
      --db=<db>  Path to database. Default is data/mosey.db
      --process=<process> Character string specifying which steps to process. Defaults to dciv. (download, clean, import, validate)
----
load_datasets 0.1

EOF
)"

#TODO: Need to echo to the log file as well as stdout. Maybe use tee as well?
#TODO: should have a way to selectively process certain steps. E.g., don't run download since we've already downloaded the data.
# Interface might be to have an optional 'process' flag where you send in the string dciv (download, clean, import, validate)
# which says which processes to run so to skip download, you send in --process civ. if not passed in, default to dciv
#TODO: make csvdir optional, just like db

#----
#---- Set up variables
#----

#TODO: Need to make these optional
#See here for details: https://github.com/docopt/docopts
#TODO: Also I might need to do raw/<id>/*.csv instead of <id>/raw/*.csv
#  Otherwise I can't pass in raw and clean folders seperately.
#  For now, just passing in parent csvdir.

[[ -z "$csvdir" ]] && csvdir=.
[[ -z "$db" ]] && db=data/mosey.db
[[ -z "$process" ]] && process=dciv

#-----------------------#
#---- Load datasets ----#
#-----------------------#

#See docs/notes.txt for notes about loading specific datasets

# the study.csv file needs to have, at minimum, a column called "study_id"
# and a column called "run". The script will ignore any other columns.

# Use miller to filter by run column and then take the study_id field
# need to use tail to remove first line, which is the header
studyIds=($(mlr --csv --opprint filter '$run == 1' then cut -f study_id ctfs/study.csv | tail -n +2))

status=load_status.csv

mkdir -p logs

for studyId in "${studyIds[@]}"
do 
  echo "*******"
  echo "Start processing study ${studyId}"
  echo "*******"
  
  #Reading study ids from csv results in \r at end. This removes them.
  studyId=${studyId%$'\r'}
  
  raw="${csvdir}/${studyId}/raw"
  clean="${csvdir}/${studyId}/clean"

  #------------------#
  #---- Download ----#
  #------------------#
  
  if [[ "$process" = *d* ]]; then
    echo "Downloading study ${studyId}"
    $MOSEYDB_SRC/db/get_study.r $studyId -r $raw -t 2>&1 | tee logs/$studyId.log
    exitcode=("${PIPESTATUS[@]}")

    #See here for info on how to store: https://www.mydbaworld.com/retrieve-return-code-all-commands-pipeline-pipestatus/
    #Since we used tee, $? contains a successful exit code

    if [ ${exitcode[0]} -eq 0 ]; then
      echo "Successfully downloaded study"
      echo $studyId,download,success >> $status
    else
      echo "Failed to download study ${studyId}"
      echo $studyId,download,fail >> $status
      continue
    fi
  fi

  #---------------#
  #---- Clean ----#
  #---------------#
  if [[ "$process" = *c* ]]; then
    echo "Cleaning study ${studyId}"
    $MOSEYDB_SRC/db/clean_study.r ${studyId} -c $clean -r $raw -t 2>&1 | tee -a logs/$studyId.log
    exitcode=("${PIPESTATUS[@]}")
  
    if [ ${exitcode[0]} -eq 0 ]; then
      echo "Successfully cleaned study"
      echo $studyId,clean,success >> $status
    else
      echo "Failed to download data for study ${studyId}"
      echo $studyId,clean,fail >> $status
      continue
    fi
  fi

  #---------------#
  #---- Import ---#
  #---------------#
  
  #Put relevant optional parameters into an array
  if [[ "$process" = *i* ]]; then
    params=()
    [[ ! -z "$db" ]] && params+=("-d $db")
  
    echo "Importing study ${studyId}"
    $MOSEYDB_SRC/db/import_study.r -i ${studyId} -c $clean "${params[@]}" -t 2>&1 | tee -a logs/$studyId.log
  
    exitcode=("${PIPESTATUS[@]}")
  
    if [ ${exitcode[0]} -eq 0 ]; then
      echo "Successfully imported data"
      echo $studyId,import,success >> $status
    else
      echo "Failed to import study ${studyId}"
      echo $studyId,import,fail >> $status
      continue
    fi
  fi
  
  #------------------#
  #---- Validate ----#
  #------------------#
  if [[ "$process" = *v* ]]; then
    echo "Validating import for study ${studyId}"
    
    params=()
    [[ ! -z "$db" ]] && params+=("-d $db")
    
    $MOSEYDB_SRC/db/validate_import.r ${studyId} -c $clean "${params[@]}" -t 2>&1 | tee -a logs/$studyId.log
  
    exitcode=("${PIPESTATUS[@]}")
    
    if [ ${exitcode[0]} -eq 0 ]; then
      echo "Successfully validated import"
      echo $studyId,validate,success >> $status
    else
      echo "Failed to validate import for study ${studyId}"
      echo $studyId,validate,fail >> $status
      continue
    fi
  fi
done

#TODO
# Run analyze statement (or pgrama optimize?) on database

#Run this from bash:
#PRAGMA analysis_limit=400;
#PRAGMA optimize;

#Also look into vacuum
#https://www.sqlitetutorial.net/sqlite-vacuum/


