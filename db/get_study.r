#!/usr/bin/env Rscript --vanilla

#TODO: if script fails, remove download folder?
#TODO: now that I'm requesting all entities in a standard way I could use a function
#       to download data.
#TODO: since I'm now saving just the raw files, I can save directly to disk using rmoveapi
#       I'll just need to replace all \r\n with "" because direct save does not do this.

'
Get and clean all data for a study from movebank api

Usage:
get_study_data.r <studyid> [--raw=<raw>] [-t] [--auth=<auth>] [--seed=<seed>]
get_study_data.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-r --raw=<raw>  Directory for saving csv files. Defaults to <wd>/data/<studyid>/raw.
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
  
  .wd <- '~/projects/movedb/analysis/test_get_clean'
  .seed <- NULL
  .test <- TRUE
  rd <- here

  .studyid <- 631036041
  .auth <- NULL
  .rawP <- file.path(.wd,'data',.studyid,'raw')
  
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
  
  if(length(ag$raw)==0) {
    .rawP <- file.path(.wd,'data',.studyid,'raw')
  } else {
    .rawP <- ifelse(isAbsolute(ag$raw),ag$raw,file.path(.wd,ag$raw))
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

dir.create(.rawP,showWarnings=FALSE,recursive=TRUE)

invisible(assert_that(dir.exists(.rawP)))

#---- Load data ----#

fields <- read_csv(rd('src/fields.csv'),col_types=cols())

message(glue('Downloading data for study {.studyid} from movebank'))

#---------------#
#---- study ----#
#---------------#
message('Getting study data')

attributes <- fields %>% filter(table=='study' & !is.na(name_raw)) %>% pull('name_raw')

std <- getStudy(.studyid,params=list(attributes=attributes)) #%>% 
  #rename(study_id=id,study_name=name) 

message(glue('Study name is: {std$name}'))
message(glue('Reported num individuals: {std$number_of_individuals}, num events: {format(std$number_of_deployed_locations,big.mark=",")}'))
#View(t(std))

#--------------------#
#---- individual ----#
#--------------------#
message('Getting individual data')

attributes <- fields %>% filter(table=='individual' & !is.na(name_raw)) %>% pull('name_raw')

ind <- getIndividual(.studyid,params=list(attributes=attributes),accept_license=TRUE) #%>% 
  #rename(individual_id=id) 

#View(ind)

#----------------#
#---- sensor ----#
#----------------#
message('Getting sensor data')

attributes <- fields %>% filter(table=='sensor' & !is.na(name_raw)) %>% pull('name_raw')

sens <- getSensor(.studyid,params=list(attributes=attributes)) #%>% 
  #rename(sensor_id=id)
#View(sens)

#----------------#
#---- tag ----#
#----------------#
message('Getting tag data')

attributes <- fields %>% filter(table=='tag' & !is.na(name_raw)) %>% pull('name_raw')

tag <- getTag(.studyid,params=list(attributes=attributes)) #%>% 
  #rename(tag_id=id)

#View(tag)

#--------------------#
#---- deployment ----#
#--------------------#
message('Getting deployment data')

attributes <- fields %>% filter(table=='deployment' & !is.na(name_raw)) %>% pull('name_raw')

dep <- getDeployment(.studyid,params=list(attributes=attributes)) #%>% 
  #rename(deployment_id=id)
#View(dep)

#---------------#
#---- event ----#
#---------------#

message('Getting GPS (653) event data. This can take awhile...')

attributes <- fields %>% filter(table=='event' & !is.na(name_raw)) %>% pull('name_raw')

tic()
status <- getEvent(.studyid,attributes,sensor_type_id=653,save_as=file.path(.rawP,'event.csv'))
toc()

invisible(assert_that(status))

#------------------------------------#
#---- Write out raw data to csvs ----#
#------------------------------------#

message('Saving data to csv files')

#Event data was saved directly to disk
write_csv(std,file.path(.rawP,'study.csv'),na="")
write_csv(ind,file.path(.rawP,'individual.csv'),na="")
write_csv(sens,file.path(.rawP,'sensor.csv'),na="")
write_csv(tag,file.path(.rawP,'tag.csv'),na="")
write_csv(dep,file.path(.rawP,'deployment.csv'),na="")

message(glue('Script complete in {diffmin(t0)} minutes'))
