#!/usr/bin/env Rscript --vanilla

#TODO: if script fails, remove download folder?
#TODO: could put renaming of columns into the fields csv. 
#       could also do renaming during clean
'
Get and clean all data for a study from movebank api

Usage:
get_study_data.r <studyid> [--out=<out>] [-t] [--auth=<auth>] [--seed=<seed>]
get_study_data.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-o --out=<out>  Output folder. Defaults to <wd/data/studyid/raw>
-a --auth=<auth>  Authentication method. Can be password, keyring, or path to yml file. Default is keyring.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

isAbsolute <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

#---- Input Parameters ----#
if(interactive()) {
  library(here)
  
  #.wd <- '~/projects/covid/analysis/movebankdb'
  .wd <- '~/projects/movedb/analysis/test_get_clean'
  .seed <- NULL
  .test <- TRUE
  rd <- here

  .studyid <- 657640587
  .auth <- NULL
  #.out <- "/Volumes/WD4TB/projects/covid/data"
  #.out <- 'test'
  .outP <- file.path(.wd,'data',.studyid,'raw')
  
} else {
  suppressPackageStartupMessages({
    library(docopt)
    library(rprojroot)
  })
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .auth <- ag$auth
  .studyid <- as.integer(ag$studyid)
  
  if(is.null(ag$out)) {
    .outP <- file.path(.wd,'data',.studyid,'raw')
  } else {
    .out <- trimws(ag$out)
    .outP <- ifelse(isAbsolute(.out),.out,file.path(.wd,.out))
  }

}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressPackageStartupMessages({
  library(getPass)
  library(keyring)
  library(rmoveapi)
  library(tictoc)
  library(yaml)
})

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

#This sets the movebank output format for timestamps for write_csv
output_column.POSIXct <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%OS3", tz='UTC')
}

# If auth is null, look for auth.yml in the working directory
# otherwise based on user request
if(is.null(.auth) || grepl('.*\\.yml$',.auth)) {
  if(is.null(.auth)) {
    yamlPF <- file.path(.wd,'auth.yml')
  } else {
    yamlPF <- ifelse(isAbsolute(.auth),.auth,file.path(.wd,.auth))
  }
  cred <- read_yaml(yamlPF)
  setAuth(cred$user,cred$pass)
} else if(.auth=='keyring') {
  setAuth(key_get('movebank_user'),key_get('movebank_pass'))
} else if(.auth=='input') {
  setAuth(getPass('Movebank user:'),getPass('Movebank password:'))
} else {
  stop('Invalid authentication method')
}

dir.create(.outP,showWarnings=FALSE,recursive=TRUE)

invisible(assert_that(dir.exists(.outP)))

#---- Load data ----#

fields <- read_csv(file.path(.wd,'fields.csv'),col_types=cols())

message(glue('Downloading data for study {.studyid} from movebank'))

#---------------#
#---- study ----#
#---------------#
message('Getting study data')

attributes <- fields %>% filter(table=='study') %>% pull('field_name')

std <- getStudy(.studyid,params=list(attributes=attributes)) %>% 
  rename(study_id=id,study_name=name) 

message(glue('Study name is: {std$study_name}'))
message(glue('Reported num individuals: {std$number_of_individuals}, num events: {format(std$number_of_deployed_locations,big.mark=",")}'))
#View(t(std))

#--------------------#
#---- individual ----#
#--------------------#
message('Getting individual data')

attributes <- fields %>% filter(table=='individual') %>% pull('field_name')

ind <- getIndividual(.studyid,params=list(attributes=attributes),accept_license=TRUE) %>% 
  rename(individual_id=id) 

getIndividual(.studyid,params=list(attributes=attributes),accept_license=TRUE,urlonly=TRUE)


#View(ind0)

#----------------#
#---- sensor ----#
#----------------#
message('Getting sensor data')

attributes <- fields %>% filter(table=='sensor') %>% pull('field_name')

sens <- getSensor(.studyid,params=list(attributes=attributes)) %>% 
  rename(sensor_id=id)

#----------------#
#---- tag ----#
#----------------#
message('Getting tag data')

attributes <- fields %>% filter(table=='tag') %>% pull('field_name')

tag <- getTag(.studyid,params=list(attributes=attributes)) %>% 
  rename(tag_id=id)

#View(tag0)

#--------------------#
#---- deployment ----#
#--------------------#
message('Getting deployment data')

attributes <- fields %>% filter(table=='deployment') %>% pull('field_name')

dep <- getDeployment(.studyid,params=list(attributes=attributes)) %>% 
  rename(deployment_id=id)

#---------------#
#---- event ----#
#---------------#

message('Getting GPS (653) event data. This can take awhile...')

attributes <- fields %>% filter(table=='event') %>% pull('field_name')

tic()
status <- getEvent(.studyid,attributes,sensor_type_id=653,save_as=file.path(.outP,'event.csv'))
toc()

invisible(assert_that(status))

#------------------------------------#
#---- Write out raw data to csvs ----#
#------------------------------------#

message('Saving data to csv files')

#Event data was saved directly to disk
write_csv(std,file.path(.outP,'study.csv'),na="")
write_csv(ind,file.path(.outP,'individual.csv'),na="")
write_csv(sens,file.path(.outP,'sensor.csv'),na="")
write_csv(tag,file.path(.outP,'tag.csv'),na="")
write_csv(dep,file.path(.outP,'deployment.csv'),na="")

message(glue('Script complete in {diffmin(t0)} minutes'))
