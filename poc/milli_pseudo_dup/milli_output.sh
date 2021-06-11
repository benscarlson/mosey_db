#---- Study 9648615

#.cleanP <- '/Volumes/WD4TB/projects/ms3/analysis/full_workflow_poc/data/9648615/clean'

cd /Volumes/WD4TB/projects/ms3/analysis/full_workflow_poc/data/9648615/clean

mlr --csv filter '$event_id == 194510657 || $event_id == 194500753' event_old.csv

# Result:
# 194510657,10197543,-6.1711953,37.2057857,2013-06-11 10:01:01.000,10197474,653,,,,,,,,,
# 194500753,10197543,-6.1711953,37.2057857,2013-06-11 10:01:01.000,10197474,653,1.07,,,,,,36.61,60,3

# New version should not have 194510657
# And 194500753 should have .001 milliseconds

mlr --csv filter '$event_id == 194510657 || $event_id == 194500753' event.csv

# Result is correct!
#194500753,10197543,-6.1711953,37.2057857,2013-06-11 10:01:01.001,10197474,653,1.07,,,,,,36.61,60,3

#Looks like around 7k additional records were removed!
cat event_old.csv | wc -l #5,499,146
cat event.csv | wc -l #5,492,064
#difference is 7,082 records. This matches exactly the number of duplicates (ignoring millis) found in the database (see the sql file)

#-------------------
#---- Study 8008992
#-----------------------#

#View(dupevts) to find events that are off by .001 

#.cleanP <- '/Volumes/WD4TB/projects/1000cranes/data/8008992/clean'

cd /Volumes/WD4TB/projects/1000cranes/data/8008992/clean

mlr --csv filter '$event_id == 171030229 || $event_id == 177704316' event_old.csv

#Raw data had difference of .001, but data written old way both had .000
#171030229,8538159,90.2069799,27.4193768,2013-02-09 09:00:52.000,8499102,653,,,,,,,,,
#177704316,8538159,90.2069799,27.4193768,2013-02-09 09:00:52.000,8499102,653,0.59,,,,,,7.42,5,3

mlr --csv filter '$event_id == 171030229 || $event_id == 177704316' event.csv

#New script correctly removed one duplicate and wrote milliseconds correctly
#177704316,8538159,90.2069799,27.4193768,2013-02-09 09:00:52.001,8499102,653,0.59,,,,,,7.42,5,3