#!/usr/bin/env Rscript --vanilla

#this code executes clean_study.r up to the point of removing duplicates. It also reads in timstamps as strings, so that they can be inspected exactly as they appear in the csv file.
# there is a bunch of code at the bottom that looks at patterns in milliseconds.

#TODO: can make studyid optional. It is only used if raw and out folders are not specified
#TODO: now that I'm requesting all entities in a standard way I could use a function

'
Clean all data for a study. Assumes data is available as csv files, formatted according to the output
of get_study_data.r

Usage:
clean_study_data.r <studyid> [--raw=<raw>] [--clean=<clean>] [-t] [--seed=<seed>]
clean_study_data.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-c --clean=<clean> Clean data will be placed in this directory. If not passed in or empty, defaults to <wd>/data/<studyid>/clean
-r --raw=<raw> Directory containing raw data. If not passed in or empty, defaults to <wd>/data/<studyid>/raw
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)
  
  .wd <- '~/projects/movedb/analysis/test_get_clean'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .studyid <- 8008992
  
  # .rawP <- file.path(.wd,'data',.studyid,'raw')
  # .cleanP <- file.path(.wd,'data',.studyid,'clean')
  
  #csvdir <- '/Volumes/WD4TB/projects/ms3/analysis/full_workflow_poc/data'
  #csvdir <- '/Volumes/WD4TB/projects/covid/analysis/movedb/csvs'
  csvdir <- '/Volumes/WD4TB/projects/1000cranes/data'
  .rawP <- file.path(csvdir,.studyid,'raw')
  .cleanP <- file.path(csvdir,.studyid,'clean')
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
  
  .studyid <- as.integer(ag$studyid)
  
  source(rd('src/funs/input_parse.r'))
  
  if(length(ag$raw)==0) {
    .rawP <- file.path(.wd,'data',.studyid,'raw')
  } else {
    #.rawP <- ifelse(isAbsolute(ag$raw),ag$raw,file.path(.wd,ag$raw))
    .rawP <- makePath(ag$raw)
  }
  
  if(length(ag$clean)==0) {
    .cleanP <- file.path(.wd,'data',.studyid,'clean')
  } else {
    #.cleanP <- ifelse(isAbsolute(ag$clean),ag$clean,file.path(.wd,ag$clean))
    .cleanP <- makePath(ag$clean)
  }
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(knitr)
  }))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

#This sets the movebank output format for timestamps for write_csv
output_column.POSIXct <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%OS3", tz='UTC')
}

dir.create(.cleanP,showWarnings=FALSE,recursive=TRUE)

invisible(assert_that(dir.exists(.cleanP)))

message(glue('Cleaning data for study {.studyid}'))

#---- Load data ----#
sensTypes <- read_csv(rd('src/sensor_type.csv'),col_types=cols()) %>% #lookup table for tag types
  rename(sensor_type_id=id)

fields <- read_csv(rd('src/fields.csv'),col_types=cols())

#---------------#
#---- study ----#
#---------------#
stdFields <- fields %>% 
  filter(table=='study' & !is.na(name_raw)) 

std <- read_csv(file.path(.rawP,'study.csv'),col_types=cols()) %>%
  rename(!!!setNames(stdFields$name_raw,stdFields$name_clean))

#--------------------#
#---- individual ----#
#--------------------#
message('Loading individual data')

indFields <- fields %>% filter(table=='individual' & !is.na(type_raw))

#Load raw data using specified column types, then rename columns to cleaned names
ind00 <- read_csv(file.path(.rawP,'individual.csv'),
                  col_types=paste(indFields$type_raw,collapse="")) %>%
  rename(!!!setNames(indFields$name_raw,indFields$name_clean))

spp <- unique(ind00$taxon_canonical_name)

message(glue('Found the following species: {paste(spp,collapse=", ")}'))

message(glue('Checking for individuals with invalid species identity (e.g. Homo sapiens)'))

#Have to explicitly allow taxon name to be NA, b/c it isn't always completely entered 
ind0 <- ind00  %>%
  filter(is.na(taxon_canonical_name) | !str_detect(tolower(taxon_canonical_name),'homo sapien'))

if(nrow(ind00)-nrow(ind0) > 0) {
  message(glue('Found and removed the following {nrow(ind00)-nrow(ind0)} invalid individuals'))
  
  ind00 %>% 
    filter(str_detect(tolower(taxon_canonical_name),'homo sapien') | 
             is.na(taxon_canonical_name)) %>%
    select(id,local_identifier,nick_name,taxon_canonical_name) %>%
    kable
  
} else {
  message('Did not find any invalid individuals')
}

#View(ind0)

#----------------#
#---- sensor ----#
#----------------#
message('Loading sensor data')

sensFields <- fields %>% filter(table=='sensor' & !is.na(type_raw))

sens0 <- read_csv(file.path(.rawP,'sensor.csv'),
                  col_types=paste(sensFields$type_raw,collapse="")) %>%
  rename(!!!setNames(sensFields$name_raw,sensFields$name_clean))

message('Found the following sensors:')

sens0 %>% distinct(sensor_type_id) %>% 
  left_join(sensTypes %>% select(sensor_type_id,name),by='sensor_type_id') %>%
  arrange(sensor_type_id) %>% 
  kable %>% paste(collapse='\n') %>% message

#View(sens0)

#----------------#
#---- tag ----#
#----------------#
message('Loading tag data')

tagFields <- fields %>% filter(table=='tag' & !is.na(type_raw))

tag0 <- read_csv(file.path(.rawP,'tag.csv'),
                 col_types=paste(tagFields$type_raw,collapse="")) %>%
  rename(!!!setNames(tagFields$name_raw,tagFields$name_clean))

message('Found the following tag types:')
tag0 %>% distinct(manufacturer_name) %>% arrange(manufacturer_name) %>% 
  kable %>% paste(collapse='\n') %>% message

#View(tag0)

#--------------------#
#---- deployment ----#
#--------------------#
message('Loading deployment data')

depFields <- fields %>% filter(table=='deployment' & !is.na(type_raw))

dep0 <- read_csv(file.path(.rawP,'deployment.csv'),
                 col_types=paste(depFields$type_raw,collapse="")) %>%
  rename(!!!setNames(depFields$name_raw,depFields$name_clean))

#If I filtered out individuals (e.g. if individual was a human) need to remove the deployment
#Then, remove any tags, sensors that don't have entries in deployment table
#Can have these if filtering out individuals, or can have these for lots of other reasons 
# (see Sarah's email)

#remove deployments for filtered individuals
dep <- dep0 %>% inner_join(ind0 %>% select(individual_id), by='individual_id')

if(nrow(dep0)-nrow(dep) > 0) {
  message(glue('Removed {nrow(dep0)-nrow(dep)} deployments for filtered individuals'))
} 

ind <- ind0 %>% inner_join(dep %>% distinct(individual_id),by='individual_id')

if(nrow(ind0)-nrow(ind)) {
  message(glue('Removed {nrow(ind0)-nrow(ind)} undeployed individuals'))
}

#remove undeployed tags. Note must use distinct() on dep b/c can have multiple deployments
tag <- tag0 %>% inner_join(dep %>% distinct(tag_id), by='tag_id')

if(nrow(tag0)-nrow(tag)) {
  message(glue('Removed {nrow(tag0)-nrow(tag)} undeployed tags'))
}

#remove all sensors that are attached to undeployed tags
sens <- sens0 %>% inner_join(tag %>% select(tag_id),by='tag_id')

if(nrow(sens0)-nrow(sens) > 0) {
  message(glue('Removed {nrow(sens0)-nrow(sens)} undeployed sensors'))
}

#---------------------#
#--- sanity checks ---#
#---------------------#

#Seems all combinations of missing deploy on and deploy off fields occur and are valid
# I have tested for NA, timestamp. but not NA, NA. Should work though.

# This checks for deploy off with no deploy on. Bad practice but does occur so can't do this check.
# Maybe move this to some sort of "health check" report
#invisible(assert_that(dep %>% filter(is.na(deploy_on_timestamp) & !is.na(deploy_off_timestamp)) %>% nrow == 0))  #should be 0

#Should never have a deployment that does not have a tag (I think this is what it does). 
#Might not be true if a tag has multiple value deployments?
invisible(assert_that(tag %>% anti_join(dep, by='tag_id') %>% nrow == 0)) #should be true

#Check to see if the same individual has the same tag with multiple deployments
#Should not be a problem because I'm filtering by date, but double check if this case comes up.
invisible(assert_that(dep %>% group_by(individual_id,tag_id) %>% summarize(num=n()) %>% 
                        filter(num > 1) %>% nrow == 0))

#Tag can be deployed multiple times, but on and off ts must be different
invisible(assert_that(dep %>% 
                        group_by(tag_id) %>% 
                        summarize(diff_on=length(unique(deploy_on_timestamp))==n(),
                                  diff_off=length(unique(deploy_off_timestamp))==n()) %>%
                        filter(diff_on==FALSE | diff_off==FALSE) %>% nrow == 0))

#Primary keys for all tables should be unique
invisible(assert_that(!any(duplicated(ind$individual_id))))
invisible(assert_that(!any(duplicated(dep$deployment_id))))
invisible(assert_that(!any(duplicated(tag$tag_id))))
invisible(assert_that(!any(duplicated(sens$sensor_id))))

#View(ind); View(dep); View(tag); View(sens)

#---------------#
#---- event ----#
#---------------#

#NOTE: 'visible' field summarizes:
# manually_marked_outier, import_marked_outlier, algorithm_marked_outlier, manually_marked_valid
# we want visible=TRUE
# 

message('Loading event data. This can take awhile...')

#Events are not loaded/renamed in the same way as other entities b/c there is not
# a 1-to-1 mapping between fields.

# coltypes <- fields %>% 
#   filter(table=='event' & !is.na(type_raw)) %>% 
#   pull('type_raw') %>% paste(collapse="")

#Load timestamps as character instead of posix
coltypes <- fields %>%
  filter(table=='event' & !is.na(type_raw)) %>%
  pull('type_raw')

coltypes[5] <- 'c'

coltypes <- paste(coltypes,collapse="")


#Note rmoveapi doesn't clean out line breaks \r\n when downloading directly to disk
#May need to address this issue in the future but there aren't any long text fields
#in the event table right now

evt0 <- read_csv(file.path(.rawP,'event.csv'),col_types=coltypes) %>%
  rename(lon=location_long,lat=location_lat)

message(glue('Retreived {format(nrow(evt0),big.mark=",")} events'))

evt00 <- evt0 %>%
  mutate(ts_str=timestamp, timestamp=as.POSIXct(timestamp,tz='UTC'))

#See report: event_raw for a table
evt <- evt00 %>% 
  filter(is.na(visible) | visible==TRUE) %>% #I think visible should always be true
  filter(!is.na(lon) & !is.na(lat)) %>% 
  filter(lon!=0 & lat!=0) %>% 
  filter(!is.na(timestamp)) %>% 
  filter(timestamp!=0) %>% 
  filter(timestamp < Sys.time()) %>% #note this date should be based on the downloaded date
  filter(is.na(eobs_status) | eobs_status=='A') #Seems for eobs tags, if eobs_status != A, then we don't have lon/lat

message(glue('Filtered out {format(nrow(evt0)-nrow(evt),big.mark=",")} bad records'))

#---- Deployment filtering ----#

#Regarding individual_id that is na: email from Sarah: 
# subject: Half a million missing individual_id in Donana event data
# see evt_ind_na_for_sarah.r.
# do inner join with deployment individual_id, tag_id to filter out undeployed records
# See checks for special cases below.
#Why ind id is na? Event records can be associated with a study that are not from animals in the study
# Filter these out by joining to deployment table, which has an entry for all animals in the study
# Furthermore, need to filter records that are outside of the deploy on/off dates
#
# Originally I thought need to have a deploy on timestamp, but deploy off can be NA
#   BUT! for Study 3807090, individuals 10366617 and 10366474 both have NA for deploy on but have a value for deploy off

#TODO: from Sarah: "Basically assuming the user has organized their study correctly, you want to include all/only locations that have an animal identifier."
#TODO:  so, this can be a second way to check, see if all locations have an animal identifier. Note: animal identifier is synonomous with "individual.local_identifier"

#If there is a deployment there are several cases
# 1) if deploy on and deploy off both not NA, timestamp needs to be between deploy on and deploy off
# 2) if only deploy on is NA, timestamp just needs to be < deploy off
# 3) if only deploy off is NA, timestamp just needs to be > deploy on
# 4) if deploy on and deploy off are both NA, take all records

#TODO: I don't think #4 is working correctly

message('Starting deployment filtering')

message('Found the following deploy on/off patterns')

dep %>% mutate(
  na_dep_on=is.na(deploy_on_timestamp),
  na_dep_off=is.na(deploy_off_timestamp)) %>%
  distinct(na_dep_on,na_dep_off) %>%
  mutate(across(.fns=~ifelse(.,NA,'timestamp'))) %>%
  arrange(na_dep_on,na_dep_off) %>% 
  kable %>% paste(collapse='\n') %>% message

#This should filter according to the cases above. 
#TODO: need to test each case to make sure it works.
evtdep <- evt %>% 
  inner_join(dep %>% 
               select(deployment_id,individual_id,tag_id,deploy_on_timestamp,deploy_off_timestamp),
             by=c('individual_id', 'tag_id')) %>%
  filter( 
    (timestamp >= deploy_on_timestamp & timestamp < deploy_off_timestamp) | #case 1
      (is.na(deploy_on_timestamp) & timestamp < deploy_off_timestamp) | #case 2
      (timestamp >= deploy_on_timestamp & is.na(deploy_off_timestamp)) | #case 3
      (is.na(deploy_on_timestamp) & is.na(deploy_off_timestamp)) #case 4
  )

invisible(assert_that(evtdep %>% filter(timestamp < deploy_on_timestamp) %>% nrow == 0)) #No cases where timestamp is < deploy on. Also covers case where deploy on is NA
invisible(assert_that(evtdep %>% filter(timestamp > deploy_off_timestamp) %>% nrow == 0)) #No cases where timestamp is > deploy off. Also covers case where deploy off is NA.

message(glue('Filtered {format(nrow(evt) - nrow(evtdep),big.mark=",")} undeployed events'))

#TODO: check to make sure all event.tag_id exist in the tag table

#All records with individual_id=NA should be filtered out
invisible(assert_that(evtdep %>% filter(is.na(individual_id)) %>% nrow == 0))

#Make sure the same tag does not have duplicate sensor types (e.g. can't have two gps sensors on the same tag)
invisible(assert_that(evtdep %>% inner_join(sens,by=c('tag_id','sensor_type_id')) %>% nrow==evtdep %>% nrow))

#---- Now that all filtering is complete, check milliseconds ---#

#Check these out. Do any of these individuals have sub-second resolution?
# Super high daily points
# Grus nigricollis - BHUTAN-MPIAB GPRS (8008992)
# - Sam Taen Ling (3928), > 6000
# - Gaen Tro Ling (3965), > 6000
# - Tse Chig Ling (3913)
# 
# GPS telemetry of Common Cranes, Sweden (10722328)
# - 7558, > 2500

#How many different types of milliseconds are there?
evtdep %>% mutate(millis=str_sub(ts_str,-4)) %>% distinct(millis) %>% pull('millis')

#20873986 58 values, bi-modal
#8008992 33 values, bi-modal
evtdep %>% mutate(millis=str_sub(ts_str,-4)) %>% group_by(millis) %>% 
  summarize(num=n()) %>% arrange(millis) %>% kable #View

table(is.na(evtdep$ground_speed))

#Pick out an individual with groundspeed=NA and examine it's timestamps
evtdep %>% filter(is.na(ground_speed)) %>% distinct(individual_id)

ind %>% filter(local_identifier=='7558')
#Try to find an individual that has NA and legit values for ground speed
evtdep %>% group_by(individual_id,na=is.na(ground_speed)) %>% summarize(num=n()) %>% 
  arrange(individual_id) %>% View

evtdep %>% filter(individual_id==10197543) %>%
  arrange(individual_id,tag_id,sensor_type_id,timestamp) %>% 
  select(event_id,individual_id,tag_id,sensor_type_id,lon,lat,timestamp,ts_str,ground_speed) %>%
  slice(3:13) %>% select(event_id,individual_id,tag_id,ts_str,ground_speed) %>% kable
  View


evtdep %>% filter(individual_id==28715112) %>%
  arrange(tag_id,sensor_type_id,timestamp) %>%
  group_by(date=as.Date(timestamp)) %>%
  summarize(num=n()) %>%
  arrange(desc(num))

#Maximum number of events at 1-second resolution is 60*60*24 = 86,400 events
#55370912 on 2015-03-05 has 6000+ points and appears to have 1 second resolution for part of the day
#55626725 on 2015-03-06 has 3000+ points and appears to have 1 second resolution for part of the day
#28715112 on 2014-09-24 has 2500+ points. Seems to have 30 second resolution for parts of the day
evtdep %>% filter(individual_id==28715112 & as.Date(timestamp)=='2014-09-24') %>%
  arrange(tag_id,sensor_type_id,timestamp) %>%
  select(event_id,individual_id,tag_id,sensor_type_id,lon,lat,timestamp,ts_str,ground_speed) %>%
  View

#Which individuals have unusual milliseconds?
evtdep %>% mutate(millis=str_sub(ts_str,-4),millis_num=as.numeric(millis)) %>% 
  select(individual_id,millis,millis_num) %>% 
  filter(!millis_num %in% c(0,0.001,0.999)) %>% View

#----
#---- Removing duplicates ----#
#---- 

message('Removing duplicates')

# Per Sarah, duplicate records can exist becuase more complete
# version of the records with additional metadata values (ground speed) are added
# these more complete records are supposed to have higher event_id but this is not
# always the case.
#From Sarah: "If and when the same data are collected by a base station and imported to the study, 
# the more complete version of the same record is added. So the studies end up with semi-duplicate 
# records, with the later record added to the database being more complete."
# NOTE: I don't find this to be the case. In some cases, first event has data and the second is NA
# So, need to figure out a good way to filter. 
# Can't just filter out where ground_speed=NA, b/c what if both dups have ground_speed=NA?
# For a set of duplicates, I should always take the one that has ground_speed. 
# What if both have NA? Then just take one of them.
#
# TODO: also check that two depolyments (with non-overlapping dates) works correctly.
#   It should, but double check. See 164399988 "SOI Lake Sempach Mallards"


# This creates group-level records for duplicates and assignes a dup_id
# so that duplicates can be matched in the full dataset
# allna shows cases where ground_speed is NA for both duplicate records
# usually just one record will have NA and the other will have a value
# when this is not the case, nomnotna will have value > 1


message('Identifying duplicates...')
t1 <- Sys.time()
dupgrps <- evtdep %>% 
  group_by(individual_id,tag_id,sensor_type_id,timestamp) %>% #in case we have multiple sensors
  summarize(
    num=n(), #NOTE: revisit allna, numnotna, uniq. Mainly useful for investigation but not used for main workflow.
    allna=all(is.na(ground_speed)),
    numnotna=sum(!is.na(ground_speed)),
    uniq=length(unique(round(ground_speed,5)))) %>%
  filter(num > 1) %>%
  ungroup() %>%
  mutate(dup_id=row_number())

message(glue('Complete in {diffmin(t1)} minutes'))

message(glue('There are {nrow(dupgrps)} duplication groups'))



# Try to figure out what is going on with timestamps & milliseconds

library(lubridate)
evtdep %>% filter(individual_id==10197543 & timestamp==ymd_hms('2013-06-11 10:01:01'))

#Timestamps are off by 1 millisecond. Can't see that when using 0S3 b/c of truncation and impresise floating point representation
evtdep %>% filter(event_id %in% c(194500753,194510657)) %>% 
  mutate(ts_OS3=strftime(timestamp,format='%Y-%m-%d %H:%M:%OS3',tz='UTC'),
         ts_OS6=strftime(timestamp,format='%Y-%m-%d %H:%M:%OS6',tz='UTC')) %>% View


#Try to locate all records that are off by a small amount (e.g. 1 ms)


#do antijoin to remove all records in dupgrps
noExactDup <- evtdep %>%
  anti_join(dupgrps,by=c('individual_id','tag_id','sensor_type_id','timestamp')) %>%
  arrange(individual_id,tag_id,timestamp,event_id)

#Should be empty
noExactDup %>% 
  group_by(individual_id,tag_id,sensor_type_id,timestamp) %>% #in case we have multiple sensors
  summarize(
    num=n()) %>%
  filter(num > 1) %>%
  ungroup() %>% nrow # result is 0 as expected

#then, do grouping again but use %OS3 to group timestamps. This will match, e.g. '.000' and '.001'
# This is becuase (using %OS6), the fractional seconds are
# .000 -> .000000, and .001 -> .000999.
# Then, because of truncation, %OS3 results in .000 for both.
# So, going from csv -> POSIXct -> csv we have
# .000 -> .000000 -> .000 and
# .001 -> .000999 -> .000

dupgrps_ts3 <- noExactDup %>% 
  mutate(ts=strftime(timestamp,format='%Y-%m-%d %H:%M:%OS3',tz='UTC')) %>%
  group_by(individual_id,tag_id,sensor_type_id,ts) %>% #in case we have multiple sensors
  summarize(
    num=n()) %>%
  filter(num > 1) %>%
  ungroup()  %>%
  mutate(dup_id=row_number())

#finally, use these new groups to extract the individual events from evtdep
#These are the rows that are off by just 1 or a few ms

pseudodup <- evtdep %>% 
  mutate(ts=strftime(timestamp,format='%Y-%m-%d %H:%M:%OS3',tz='UTC'),
         ts6=strftime(timestamp,format='%Y-%m-%d %H:%M:%OS6',tz='UTC')) %>%
  inner_join(dupgrps_ts3,by=c('individual_id','tag_id','sensor_type_id','ts')) %>%
  arrange(individual_id,tag_id,timestamp,event_id)

pseudodup %>% select(ground_speed,timestamp,ts,ts6) %>% View
#Every pair has one event with .000 and one with .001
#The one with .001 is always the one with more complete data
#So, maybe this is what Sarah meant by taking the 'later' one.




#----- older investigations ----
dupgrps %>% filter(individual_id==10197543 & timestamp==ymd_hms('2013-06-11 10:01:01'))

evtdep %>% 
  filter(event_id %in% c(194500753,194510657)) %>% 
  group_by(individual_id,tag_id,sensor_type_id,timestamp) %>% #in case we have multiple sensors
  summarize(
    num=n(), #NOTE: revisit allna, numnotna, uniq. Mainly useful for investigation but not used for main workflow.
    allna=all(is.na(ground_speed)),
    numnotna=sum(!is.na(ground_speed)),
    uniq=length(unique(round(ground_speed,5))))

x <- evtdep %>% 
  filter(event_id %in% c(194500753,194510657))

#mlr --csv filter '$event_id == 194500753 || $event_id == 194510657' event.csv

all(x$individual_id==x$individual_id[1])
all(x$tag_id==x$tag_id[1])
all(x$sensor_type_id==x$sensor_type_id[1])
all(x$timestamp==x$timestamp[1])
x$timestamp[1]
x$timestamp[2]

as.numeric(x$timestamp[1])
as.numeric(x$timestamp[2])
x$timestamp[1]==x$timestamp[2]

class(x$timestamp[1])
class(x$timestamp[2])

strftime(x$timestamp[1],format='%Y-%m-%d %H:%M:%OS6',tz='UTC')
strftime(x$timestamp[2],format='%Y-%m-%d %H:%M:%OS6',tz='UTC')

#When printing, certain milliseconds are internally rounded down, which results in printing one less
# Than the correct number. Note .001 is printed as .000999, but .002 is printed as .002
#https://stackoverflow.com/questions/7726034/how-r-formats-posixct-with-fractional-seconds
as.POSIXct('2013-06-11 10:01:01.001',tz='UTC') %>% strftime(format='%Y-%m-%d %H:%M:%OS3',tz='UTC')
as.POSIXct('2013-06-11 10:01:01.002',tz='UTC') %>% strftime(format='%Y-%m-%d %H:%M:%OS6',tz='UTC')
as.POSIXct('2013-06-11 10:01:01.003',tz='UTC') %>% strftime(format='%Y-%m-%d %H:%M:%OS6',tz='UTC')

as.POSIXct('2013-06-11 10:01:01.487',tz='UTC') %>% strftime(format='%Y-%m-%d %H:%M:%OS6',tz='UTC')
as.POSIXct('2013-06-11 10:01:01.488',tz='UTC') %>% strftime(format='%Y-%m-%d %H:%M:%OS6',tz='UTC')

x <- as.numeric(as.POSIXct('2013-06-11 10:01:01.488',tz='UTC')) - 
  as.numeric(as.POSIXct('2013-06-11 10:01:01.487',tz='UTC')) 

round(x,3)

ts <- as.POSIXct('2013-06-11 10:01:01.001',tz='UTC')
strftime(ts,format='%Y-%m-%d %H:%M:%OS3',tz='UTC')
strftime(ts,format='%Y-%m-%d %H:%M:%OS6',tz='UTC')

ymd_hms('2013-06-11 10:01:01.001') %>% strftime(format='%Y-%m-%d %H:%M:%OS6',tz='UTC')

#as.numeric does not retain any differences in milliseconds
as.POSIXct('2013-06-11 10:01:01.001',tz='UTC') %>% as.numeric()
as.POSIXct('2013-06-11 10:01:01.002',tz='UTC') %>% as.numeric()

#See this discussion
#https://stackoverflow.com/questions/15383057/accurately-converting-from-character-posixct-character-with-sub-millisecond-da

#The floating point 
as.POSIXct('2013-06-11 10:01:01.001',tz='UTC') %>% as.numeric %>% print(digits=20) #smaller
as.POSIXct('2013-06-11 10:01:01.000999',tz='UTC') %>% as.numeric %>% print(digits=20) #smaller
as.POSIXct('2013-06-11 10:01:01.000009',tz='UTC') %>% as.numeric %>% print(digits=20) #larger!

#!!!!!!!! REMOVE !!!!!!!!!!!!!!!
evtnodup %>% filter(event_id %in% c(194500753,194510657)) %>% View

