#!/usr/bin/env Rscript
# chmod 744 script_template.r #Use to make executable

'
Replace all cases of supplied string with NULL, for all columns in table
Useful when NA has been inserted instead of NULL. Common if using .import

Usage:
update_null <table> <value>
update_null (-h | --help)

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
  .table <- 'tag'
  .value <- 'NA'
} else {
  spsm(library(docopt))
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .table <- ag$table
  .value <- ag$value
}

.dbPF <- here('data/derived/database.db')

#---- Load data ----#

db <- DBI::dbConnect(RSQLite::SQLite(), .dbPF)
tb <- tbl(db,.table)

message(glue('Updating "{.value}" to null on table {.table}'))

for(field in dbListFields(db,.table)) {
  #field<-'comments'
  sql <- glue_sql('update {`.table`} set {`field`} = null where {`field`} = {.value}',.con=db)
  #message(sql)
  rowsaf <- dbExecute(db,sql)
  message(glue('Updated `{field}` affected {rowsaf} rows'))
}

message(glue('Script complete in {diffmin(t0)} minutes'))