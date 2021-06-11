#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

'
Check for presence of both exact and fuzzy (ignore milliseconds) dups based on timestamps.

Usage:
script_template <dat> <out> [--seed=<seed>] [-b] [-t]
script_template (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-b --rollback   Rollback transaction if set to true.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)

  .wd <- '~/projects/1000cranes/analysis/'
  #.wd <- '~/projects/covid/analysis/movedb'
  .seed <- NULL
  .rollback <- TRUE
  .test <- TRUE
  rd <- here::here
  
  # .datPF <- file.path(.wd,'data/dat.csv')
  # .outPF <- file.path(.wd,'figs/myfig.png')
  
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .rollback <- as.logical(ag$rollback)
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  source(rd('src/funs/input_parse.r'))
  
  #.list <- trimws(unlist(strsplit(ag$list,',')))
  .datPF <- makePath(ag$dat)
  .outPF <- makePath(ag$out)
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

theme_set(theme_eda)

#---- Local parameters ----#
.dbPF <- file.path(.wd,"data/move.db")
#.dbPF <- '/Volumes/WD4TB/projects/covid/analysis/movedb/data/move.db'

#---- Load control files ----#

#Also see where I use individual table to make inds ctfs
# inds <- read_csv(file.path(.wd,'ctfs/individual.csv'),col_types=cols()) %>%
#   filter(as.logical(run)) %>% select(-run)

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))

db <- dbConnect(RSQLite::SQLite(), .dbPF)

invisible(assert_that(length(dbListTables(db))>0))

#Load inds list from the database
inds <- 'select individual_id, study_id, local_identifier
from individual order by study_id, local_identifier' %>% 
  dbGetQuery(db,.) #%>% slice(1:5)


#====

#---- Perform analysis ----#

#Look at exactly duplicates (matching on millis as well)
sql <- 'select individual_id,timestamp as ts_sec, count(*) as num 
from event 
where individual_id = {.}
group by tag_id, sensor_type_id, timestamp
having num > 1'

tic('Checking for exact duplicates...')
dups <- inds %>%
  mutate(num=map_int(individual_id,~{
   dbGetQuery(db,glue_sql(sql)) %>% as_tibble %>% nrow
  }))
toc(log=TRUE)

ndups=sum(dups$num)

message(glue('There are a total of {ndups} exact duplicates'))

if(ndups>0) {
dups %>% filter(num>0) %>%
  kable(format.args = list(big.mark = ",")) %>%
  paste(collapse='\n') %>% message
}


#----- Ignore millis -----#

sql <- 'select individual_id, strftime("%Y-%m-%d %H:%M:%S", timestamp) as ts_sec, count(*) as num 
from event 
where individual_id = {.}
group by tag_id, sensor_type_id, ts_sec
having num > 1' #%>% dbGetQuery(db,.) %>% View

tic('Checking for fuzzy duplicates (ignoring milliseconds)...',quiet=FALSE)
dups2 <- inds %>%
  mutate(num=map_int(individual_id,~{
    #print(glue_sql(sql))
    dbGetQuery(db,glue_sql(sql)) %>% as_tibble %>% nrow
  }))
toc(log=TRUE)

ndups2=sum(dups2$num)

message(glue('There are a total of {ndups} fuzzy duplicates'))

if(ndups2>0) {
  dups %>% filter(num>0) %>%
    kable(format.args = list(big.mark = ",")) %>%
    paste(collapse='\n') %>% message
}

#---- Finalize script ----#

if(!.test) {
  library(git2r)
  library(uuid)
  
  .runid <- UUIDgenerate()
  .parPF <- file.path(.wd,"run_params.csv")
  
  #Update repo and pull out commit sha
  repo <- repository(rd('src'))
  
  rstat <- status(repo)
  if(length(rstat$staged) + 
     length(rstat$unstaged) + 
     length(rstat$untracked) > 0) {
    add(repo,'.')
    commit(repo, glue('script auto update. runid: {.runid}'))
  }
  
  
  .git_sha <- sha(repository_head(repo))
  
  #Save all parameters to csv for reproducibility
  #TODO: write this to a workflow database instead
  saveParams(.parPF)
}

if(.rollback) {
  message('Rolling back transaction because this is a test run.')
  dbRollback(db)
} else {
  dbCommit(db)
}

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))