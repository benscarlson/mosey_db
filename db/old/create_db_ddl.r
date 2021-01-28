#creates initial database DDL based on tables
#once I have DDL, I should delte database and start over using just the DDL

library(DBI)
library(RSQLite)

studyid <- 164399988 #"SOI Lake Sempach Mallards"

.dir = file.path('~/scratch',studyid)

#TODO: don't overwrite if database already exists!!
#ls data/database.db
#touch data/database.db

db <- DBI::dbConnect(RSQLite::SQLite(), 'data/database.db')

std <- read_csv(file.path(.dir,'study.csv'))
dbCreateTable(db,'study',std)

ind <- read_csv(file.path(.dir,'individual.csv'))
dbCreateTable(db,'individual',ind)

read_csv(file.path(.dir,'sensor.csv')) %>%
dbCreateTable(db,'sensor',.)

read_csv(file.path(.dir,'tag.csv')) %>%
  dbCreateTable(db,'tag',.)

read_csv(file.path(.dir,'deployment.csv')) %>%
  dbCreateTable(db,'deployment',.)

#TODO: remove deploy_on/off ts. remove tag_id?
read_csv(file.path(.dir,'event.csv')) %>%
  dbCreateTable(db,'event',.)
