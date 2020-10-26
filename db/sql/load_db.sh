#TODO: many csv files have "NA" written, need to write these as ZLS instead
# make sure ZLS is loaded as null in database.

cd data/derived

sqlite3 database.db

.mode csv
PRAGMA foreign_keys = ON;

#Can't access env variables in sqlite, so need to manually change working directory here
.cd 'projects/8863543'

BEGIN;

#TODO: need to delete all rows where tag is from huj_storks.
# Might have to do this by listing all tag_id from the tag.csv file
.import "|tail -n +2 tag.csv" tag
.import "|tail -n +2 study.csv" study #Has "NULL" for NULL values. Update
.import "|tail -n +2 individual.csv" individual
.import "|tail -n +2 deployment.csv" deployment
.import "|tail -n +2 sensor.csv" sensor
.import "|tail -n +2 event.csv" event

#TODO: need to update "NULL" to NULL in every file. Uhg...
END;

.quit
