#Example workflow script that creates a database

wd=~/projects/mycoolproject/analysis

MOSEYDB_SRC=~/projects/mosey_db/src

cd $wd

mkdir -p data

#-------------------------#
#---- Create database ----#
#-------------------------#

# Don't run this if database already exists!
# Below is commented out to prevent accidental execution

# cat $MOSEYDB_SRC/db/create_db.sql | sqlite3 data/mosey.db