#!/usr/bin/env Rscript --vanilla

'
Load cleaned movebank data from csv to database. Need to pass in either a studyid or the path to the data to load.

Usage:
load_study.r [--studyid=<studyid>] [--dat=<dat>] [--seed=<seed>] [-t] [-b]
load_study.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-i --studyid=<studyid>  The study id of the data to load. If not passed in, need to supply <dat>
-d --dat=<dat>  Folder containing the data to load. Defaults to <wd>/data/<studyid>/clean
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
-b --rollback   Rollback the transaction before exiting the script.

' -> doc

#---- Parameters ----#

if(interactive()) {
  library(here)
  
  .wd <- '~/projects/movedb/analysis/test_get_clean'
  .seed <- NULL
  .test <- TRUE
  .rollback <- TRUE
  rd <- here
  
  .studyid <- 631036041
  .datP <- .outP <- file.path(.wd,'data',.studyid,'clean')
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
  .rollback <- as.logical(ag$rollback)
  
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .studyid <- as.integer(ag$studyid)

  if(is.null(ag$dat)) {
    .datP <- file.path(.wd,'data',.studyid,'clean')
  } else {
    .datP <- trimws(ag$dat)
    .datP <- ifelse(isAbsolute(.dat),.dat,file.path(.wd,.dat))
  }
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(5326)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(knitr)
    library(lubridate)
    library(RSQLite)
}))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

#---- Local parameters ----#

#Can make this a parameter in the future
.dbPF <- file.path(.wd,'data/movebank.db')

#---- Initialize database ----#
db <- DBI::dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

fields <- read_csv(rd('src/fields.csv'),col_types=cols())

#---- Functions ----#

#Format POSIXct according to movebank before writing to the database
movebankTs <- function(x) strftime(x,format='%Y-%m-%d %H:%M:%OS3',tz='UTC')

loadInsert <- function(entity,fields) {
  #entity <- 'event'
  coltypes <- fields %>% 
    filter(table==entity & !is.na(type_clean)) %>% 
    pull('type_clean') %>% paste(collapse="")
  
  dat <- read_csv(file.path(.datP,glue('{entity}.csv')),col_type=coltypes)
  
  rows <- dat %>% 
    mutate_if(is.POSIXct,movebankTs) %>%
    dbAppendTable(db, entity, .)
}

#---------------------#
#---- Main script ----#
#---------------------#

invisible(dbExecute(db,'PRAGMA foreign_keys=ON'))

dbBegin(db)

entities <- c('tag','study','sensor','individual','deployment','event')

rows <- entities %>% map_int(loadInsert,fields=fields)

message(glue('Inserted the following rows into the database'))

tibble(entities,rows) %>% 
  kable(format.args = list(big.mark = ",")) %>%
  paste(collapse='\n') %>% message

#---- Finalize script ----#

if(.rollback) {
  message('Rolling back transaction because this is a test run.')
  dbRollback(db)
} else {
  dbCommit(db)
}

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))
