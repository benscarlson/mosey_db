#!/usr/bin/env Rscript --vanilla

'
Delete study

Usage:
delete_study.r <studyid>
delete_study.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.

' -> doc

#---- Parameters ----#

if(interactive()) {
  library(here)
  
  .wd <- '~/projects/movebankdb/analysis/movebankdb'
  .script <- 'src/db/delete_study.r' #Currently executing script
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .studyid <- 682808477	#Black Stork in Spain - Migra Program in Spain
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .studyid <- as.integer(ag$studyid)
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressPackageStartupMessages({
  library(DBI)
  library(knitr)
  library(RSQLite)
})

source(rd('src/funs/breezy_funs.r'))

#---- Local parameters ----#
.dbPF <- file.path(.wd,'data/movebank.db')

#---- Load data ----#

db <- dbConnect(RSQLite::SQLite(), .dbPF)

invisible(assert_that(length(dbListTables(db))>0))

#---- sql statements ----#

#-- event
evt_q <- glue_sql(
  'delete from event
  where individual_id in (
    select individual_id from individual where study_id = {.studyid})')

#-- sensor
sen_q <- glue_sql(
  'delete from sensor
  where tag_id in (
    select tag_id from deployment d
    inner join individual i on d.individual_id = i.individual_id
    where i.study_id = {.studyid})')

#-- tag
tag_q <- glue_sql(
  'delete from tag
  where tag_id in (
    select tag_id from deployment d
    inner join individual i on d.individual_id = i.individual_id
    where i.study_id = {.studyid})')

#-- deployment
dep_q <- glue_sql(
  'delete from deployment
  where individual_id in (
    select individual_id from individual where study_id = {.studyid})')

#-- individual
ind_q <- glue_sql(
  'delete from individual where study_id = {.studyid}')

#-- study
std_q <- glue_sql(
  'delete from study where study_id = {.studyid}')

#---- Execute sql ----#

#Have to turn foreign keys off because tag_id is fk in deployment, but I need deployment to delete tag
# since tag does not have study_id
invisible(dbExecute(db,'PRAGMA foreign_keys=OFF'))
dbBegin(db)

message("Deleted the following rows")
tibble(tb=c('event','sensor','tag','deployment','individual','study'), #
       q=c(evt_q,sen_q,tag_q,dep_q,ind_q,std_q)) %>% #
  mutate(rows=map_dbl(q,~{dbExecute(db, .)})) %>%
  select(tb,rows) %>% kable

invisible(dbExecute(db,'PRAGMA foreign_keys=ON')) #Make sure foreign keys are still valid

#dbRollback(db)

#---- Finalize script ----#

dbCommit(db)
dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))
