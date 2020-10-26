#!/usr/bin/env Rscript --vanilla

#TODO: update to pass in full dat folder

'
Load cleaned movebank data from csv to database

Usage:
load_study_data.r <studyid> <dat> [-t] [--seed=<seed>]
load_study_data.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git

' -> doc

#---- Parameters ----#

if(interactive()) {
  library(here)
  .wd <- '~/projects/movebankdb/analysis/movebankdb'
  .script <- 'src/db/load_study_data.r' #Currently executing script
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .studyid <- 10763606	#LifeTrack White Stork Poland
  .datP <- "/Volumes/My Book/projects/movebankdb/active/10763606"
} else {
  suppressPackageStartupMessages({
    library(docopt)
    library(rprojroot)
    library(R.utils)
  })
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  .eda <- as.logical(ag$eda)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .studyid <- as.integer(ag$studyid)
  .datP <- trimws(ag$dat)
  .datP <- ifelse(isAbsolutePath(.datP),.datP,file.path(.wd,.datP))
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(5326)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressPackageStartupMessages({
  library(DBI)
  library(lubridate)
  library(RSQLite)
})

source(rd('src/funs/breezy_funs.r'))

#---- Local parameters ----#

.dbPF <- file.path(.wd,'data/movebank.db')


#---- Initialize database ----#
db <- DBI::dbConnect(RSQLite::SQLite(), .dbPF)

#---------------------#
#---- Main script ----#
#---------------------#

invisible(dbExecute(db,'PRAGMA foreign_keys=ON'))

dbBegin(db)

#---- tag ----#
tag <- read_csv(file.path(.datP,'tag.csv'),col_type=cols())

rows <- tag %>% dbAppendTable(db, "tag", .)
message(glue('Inserted {rows} rows into the tag table'))

#---- study ----#
study <- read_csv(file.path(.datP,'study.csv'),col_type=cols())

rows <- study %>% 
  mutate_if(is.POSIXct,strftime,format='%Y-%m-%dT%TZ',tz='UTC') %>%
  dbAppendTable(db, "study", .)
message(glue('Inserted {rows} row into the study table'))

#---- sensor ----#
sensor <- read_csv(file.path(.datP,'sensor.csv'),col_type=cols())

rows <- sensor %>% dbAppendTable(db, "sensor", .)
message(glue('Inserted {rows} rows into the sensor table'))

#---- individual ----#
ind <- read_csv(file.path(.datP,'individual.csv'),col_type=cols())

rows <- ind %>% 
  mutate_if(is.POSIXct,strftime,format='%Y-%m-%dT%TZ',tz='UTC') %>%
  dbAppendTable(db, "individual", .)

message(glue('Inserted {rows} rows into the individual table'))

#---- deployment ----#
dep <- read_csv(file.path(.datP,'deployment.csv'),col_type=cols())

rows <- dep %>% 
  mutate_if(is.POSIXct,strftime,format='%Y-%m-%dT%TZ',tz='UTC') %>%
  dbAppendTable(db, "deployment", .)

message(glue('Inserted {rows} rows into the deployment table'))

#---- event ----#
cols <- cols(
  event_id = col_double(),
  individual_id = col_double(),
  lon = col_double(),
  lat = col_double(),
  timestamp = col_datetime(format = ""),
  tag_id = col_double(),
  sensor_type_id = col_double(),
  ground_speed = col_double(),
  gps_speed_accuracy_estimate = col_double(),
  gps_dop = col_double(),
  gps_hdop = col_double(),
  gps_vdop = col_double(),
  gps_satellite_count = col_double(),
  horizontal_accuracy = col_double(),
  time_to_fix = col_double(),
  fix_type = col_integer()
)

evt <- read_csv(file.path(.datP,'event.csv'),col_types = cols)

rows <- evt %>% 
  mutate_if(is.POSIXct,strftime,format='%Y-%m-%dT%TZ',tz='UTC') %>%
  dbAppendTable(db, "event", .)

message(glue('Inserted {format(rows,big.mark=",")} rows into the event table'))

#---- Finalize script ----#
dbCommit(db)
dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))
