wd=~/projects/mycoolproject/analysis
out=~/projects/mycoolproject/analysis/data
src=~/projects/movebankdb/src

cd $wd

# In order to run the script below, set up a study control file and an authentication file
# 1) In the control files directory $wd/ctfs, create a file called study.csv with 
# columns for (at minimum) a column 'study_id', 'run'. See example in src/examples/study.csv
# 2) In the main working directory, $wd, set up auth.yml with your authentication information.
# See the file src/examples/study.csv

$src/db/load_datasets.sh $src $out
  


