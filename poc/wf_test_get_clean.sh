#!!!! START HERE !!!!
# This script works, now update the load_studies script to reflect changes.

wd=~/projects/movedb/analysis/test_get_clean
src=~/projects/movedb/src

#studyid=10763606 #White stork poland
#studyid=657640587 #1000 Cranes. Africa
#studyid=631036041 #1000 Cranes. Mongolia
#studyid=497087673 #1000 Cranes. Russia. Altai.
studyid=542653863 #1000 Cranes. Russia. Common Crane.csv

cd $wd

mkdir logs

$src/db/get_study.r $studyid -t 2>&1 | tee logs/$studyid.log

$src/db/clean_study.r $studyid -t 2>&1 | tee -a logs/$studyid.log

# cat $src/db/create_db.sql | sqlite3 data/move.db

$src/db/import_study.r -i $studyid -t 2>&1 | tee -a logs/$studyid.log

$src/db/validate_import.r $studyid -t 2>&1 | tee -a logs/$studyid.log

#Clean up
$src/db/delete_study.r $studyid 2>&1 | tee -a logs/$studyid.log

#-------------------------------#
#---- test load_datasets.sh ----#
#-------------------------------#

#What I want is to specify "out" and have out/id/raw and out/id/clean
#If I do this I need to hardcode the raw and clean folders
#In the future if I wanted to supersede these folders I could add optional parameters

#Each script can take an optional <csvdir> parameter. Folders for 'raw' and 'clean' are hard-coded under <csvdir>
wd=~/projects/movedb/analysis/test_get_clean
#csvdir=~/projects/movedb/analysis/test_get_clean/data

export MOVEDB_SRC=~/projects/movedb/src

cd $wd

# In order to run the script below, set up a study control file and an authentication file
# 1) In the control files directory $wd/ctfs, create a file called study.csv with 
# columns for (at minimum) a column 'study_id', 'run'. See example in src/examples/study.csv
# 2) In the main working directory, $wd, set up auth.yml with your authentication information.
# See the file src/examples/study.csv

#!!!! START HERE !!!!
# Full test run is working. Try with more than one study
# Then try for real

csvdir=/Volumes/WD4TB/projects/1000cranes/data

#TODO: try out set -e and set -x
$MOVEDB_SRC/db/load_studies.sh $csvdir

#interface
# get_study studyid -raw x
# clean_study studyid -raw x -clean y
# import_study studyid -clean y -db z
# validate_import studyid -clean y -db z

$MOVEDB_SRC/db/delete_study.r 496130871
