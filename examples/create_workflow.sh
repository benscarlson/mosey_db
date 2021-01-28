#Example workflow script that creates a database

pd=~/projects/mycoolproject/analysis/movebankdb

src=~/projects/movebankdb/src

cd $pd

#-------------------------#
#---- Create database ----#
#-------------------------#

# Don't run this if database already exists!
# Below is commented out to prevent accidental execution

# cat $src/db/create_db.sql | sqlite3 data/movebank.db