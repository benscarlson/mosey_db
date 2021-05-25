#!/usr/bin/env Rscript --vanilla

'
Makes sure study imported correctly
Compares row counts between <clean>/*.csv and the corresponding tables <db>.

Usage:
validate_import.r <studyid> [--clean=<clean>] [--db=<db>] [-t] [--seed=<seed>]
script_template (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-c --clean=<clean>  Parent folder of the directory with the clean data that was imported into the database. Defaults to <wd>/data/<studyid>
-d --db=<db> Data in the <clean> directory will be imported into the database at <db>. Defaults to <wd>/data/move.db
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
-e --eda         Indicates eda mode, plots with additional info
' -> doc

isAbsolute <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

#---- Input Parameters ----#
if(interactive()) {
  library(here)
  
  .wd <- '~/projects/movedb/analysis/test_get_clean'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .studyid <- 631036041
  .outP <- file.path(.wd,'data',.studyid,'clean')
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
  
  if(length(ag$db)==0) {
    .dbPF <- file.path(.wd,'data','move.db')
  } else {
    .dbPF <- trimws(ag$db)
  }
  
  if(length(ag$clean)==0) {
    invisible(stopifnot(length(.studyid)>0))
    .cleanP <- file.path(.wd,'data',.studyid,'clean')
  } else {
    .cleanP <- ifelse(isAbsolute(ag$clean),ag$clean,file.path(.wd,ag$clean))
  }
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(knitr)
    library(RSQLite)
  }))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

#---- Local parameters ----#

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

#---------------------#
#---- Main script ----#
#---------------------#

#deployment
dep_q <- gsub('\n',' ',
'select count(*) from deployment d
inner join individual i on d.individual_id = i.individual_id
inner join study s on i.study_id = s.study_id
where s.study_id = {.studyid}')
#event
evt_q <- gsub('\n',' ',
'select count(*) from event e
inner join individual i on e.individual_id = i.individual_id
inner join study s on i.study_id = s.study_id
where s.study_id = {.studyid}')
#individual
ind_q <- 'select count(*) from individual i where i.study_id = {.studyid}'
#tag
tag_q <- gsub('\n',' ',
'select count(distinct t.tag_id) from study s
inner join individual i on s.study_id = i.study_id
inner join deployment d on i.individual_id = d.individual_id
inner join tag t on d.tag_id = t.tag_id
where i.study_id = {.studyid}')
#sensor
sen_q <- gsub('\n',' ',
'select count(distinct sen.sensor_id) from study s
inner join individual i on s.study_id = i.study_id
inner join deployment d on i.individual_id = d.individual_id
inner join tag t on d.tag_id = t.tag_id
inner join sensor sen on t.tag_id = sen.tag_id
where i.study_id = {.studyid}')
#study
std_q <- 'select count(*) from study where study_id = {.studyid}'

res <- tibble(tname=c('deployment','event','individual','sensor','study','tag'),
       q=map_chr(c(dep_q,evt_q,ind_q,sen_q,std_q,tag_q),glue)) %>%
  mutate(
    q=map_chr(q,glue),
    file_n=map_dbl(tname,~{
      fn <- file.path(.cleanP,glue('{.}.csv')) %>% path.expand
      as.integer(system(glue('wc -l < "{fn}"'),intern=TRUE)) - 1
    }),
    db_n=map_dbl(q,~{dbGetQuery(db, .)[1,1]}),
    equal=file_n==db_n
  ) %>%
  select(-q)

res %>%
  kable(col.names=c('Entity','File Rows','DB Rows','Equal'),
    format.args = list(big.mark = ",")) %>%
  paste(collapse='\n') %>% message



