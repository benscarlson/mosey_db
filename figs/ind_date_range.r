#!/usr/bin/env Rscript
# chmod 744 script_template.r #Use to make executable

'
Plots showing temporal extent of points per individual, for a given study.
Right now outputs two pdf. The first is bar with min/max. The second hows histograms. 

Usage:
ind_date_range.r <studyid> <out> [-t] [--seed=<seed>]
ind_date_range.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)

  .wd <- '~/projects/movebankdb/analysis/movebankdb'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .studyid <- 10531951
  .outPF <- file.path(.wd,'figs/Black_Stork_MPIAB_Latvia.pdf')
  
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
  .outPF <- ag$out
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

source(rd('src/funs/breezy_funs.r'))
source(rd('src/funs/themes.r'))
theme_set(theme_eda)

#---- Local parameters ----#
.dbPF <- file.path(.wd,'data/movebank.db')

#---- Load control files ----#

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

stdtb <- tbl(db,'study')

#---- Load data ----#

#---- Perform analysis ----#

study <- stdtb %>% filter(study_id==.studyid) %>% as_tibble

# #See individual-level metadata for the study
# tbl(db,'individual') %>%
#   filter(study_id==.studyid) %>%
#   select(individual_id,local_identifier, nick_name, sex, earliest_date_born,
#          latest_date_born, exact_date_of_birth) %>% as_tibble %>% View

sql <- 'select e.individual_id, i.local_identifier, e.lon, e.lat, e.timestamp
from event e
inner join individual i
on e.individual_id = i.individual_id
where i.study_id = ?'

t1 <- Sys.time()
#Remember dates coming back from sqlite are strings. Need to convert.
dat0 <- dbGetQuery(db, sql, params = list(.studyid)) %>% 
  as_tibble %>%
  mutate(timestamp=as.POSIXct(timestamp, format='%Y-%m-%dT%H:%M:%S', tz='UTC'))
message(glue('Query complete in {diffmin(t1)} minutes'))
#Query complete in 0.44 minutes
#Note using dbplyr is slower, Query complete in 0.79 minutes

dat <- dat0 %>%
  group_by(individual_id,local_identifier) %>%
  summarize(mints=as.Date(min(timestamp)),
            maxts=as.Date(max(timestamp)))

#Tricky to just output linerange geometry. If we have min and max
# for the line range, what do we put as x? The trick is to 
# just put the min value. That seems to set up the axes correctly.
p <- dat %>%
  ggplot(aes(x=mints,y=local_identifier,color=local_identifier)) +
  geom_linerange(aes(xmin=mints,xmax=maxts),show.legend=FALSE) +
  scale_x_date(date_breaks='1 years', date_labels = "%Y") +
  ggtitle(study$study_name) +
  xlab('Date') + ylab(NULL)

#---- Save output ---#
dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

h=9; w=9
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,height=h,width=w,type='cairo')
}


#---- Finalize script ----#

if(!.test) {
  suppressWarnings(
    suppressPackageStartupMessages({
      library(git2r)
      library(uuid)
  }))
  
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

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))