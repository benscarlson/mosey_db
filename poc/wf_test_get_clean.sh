wd=~/projects/movedb/analysis/test_get_clean
src=~/projects/movedb/src

#studyid=10763606 #White stork poland
studyid=657640587 #1000 Cranes. Africa

cd $wd

mkdir logs

$src/db/get_study_data.r $studyid -t 2>&1 | tee logs/$studyid.log

$src/db/clean_study_data.r $studyid -t 2>&1 | tee -a logs/$studyid.log
