#!/usr/bin/env Rscript
# chmod 744 script_template.r #Use to make executable

'
Replace TRUE with 1 and FALSE with 0 for all columns in table.
Useful if loading from .import

Usage:
update_tf <table>
update_tf (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.

' -> doc

set.seed(5326)
t0 <- Sys.time()

library(here)
source(here('src/startup_script.r'))

spsm(library(DBI))
spsm(library(RSQLite))

source(here('src/funs/funs.r'))
# source(here('src/funs/themes.r'))
# theme_set(theme_eda)

#---- Parameters ----#

if(interactive()) {
  .wd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .table <- 'study'
} else {
  spsm(library(docopt))
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .table <- ag$table
}

.dbPF <- here('data/derived/database.db')

#---- Load data ----#

db <- dbConnect(RSQLite::SQLite(), .dbPF)
tb <- tbl(db,.table)

message(glue('Updating TRUE/FALSE to 1/0 on all fields in table {.table}'))

for(field in dbListFields(db,.table)) {
  #field<-'i_can_see_data'
  sql <- glue_sql(
  'update {`.table`} 
    set {`field`} = case 
      when {`field`} = "TRUE" then 1
      when {`field`} = "FALSE" then 0
  end
  where {`field`} in ("TRUE","FALSE")',.con=db)
  
  #message(sql)
  rowsaf <- dbExecute(db,sql)
  message(glue('Updated `{field}` affected {rowsaf} rows'))
}

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))