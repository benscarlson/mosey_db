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

# cat $src/db/create_db.sql | sqlite3 data/movebank.db

$src/db/import_study.r -i $studyid -t 2>&1 | tee -a logs/$studyid.log

$src/db/validate_import.r $studyid -t 2>&1 | tee -a logs/$studyid.log

#Clean up
$src/db/delete_study.r $studyid 2>&1 | tee -a logs/$studyid.log
