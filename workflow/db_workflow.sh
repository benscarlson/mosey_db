#TODO: turn this into a script that can be called from a different project
# Also split the create db and load db actions into different scripts

#Set pd and out based on specific project
#pd=~/projects/movebankdb/analysis/movebankdb
pd=~/projects/covid/analysis/movebankdb

#This is where csv files downloaded from movebank are staged prior to db import
#out="/Volumes/WD4TB/projects/movebankdb/active"
out="/Volumes/WD4TB/projects/covid/data"

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

#Call db/load_datasets.sh