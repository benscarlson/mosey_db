#!/usr/bin/env Rscript --vanilla

#TODO: can make studyid optional. It is only used if raw and out folders are not specified
#TODO: now that I'm requesting all entities in a standard way I could use a function
#TODO: populate event table with study_id

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
  
  .wd <- '~/projects/mosey_db_test/analysis/test1'
  .seed <- NULL
  .test <- TRUE
  rd <- here

  .studyid <- 10449318

  csvdir <- file.path(.wd,'data/csvs')
  
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
    .rawP <- makePath(ag$raw)
  }
  
  if(length(ag$clean)==0) {
    .cleanP <- file.path(.wd,'data',.studyid,'clean')
  } else {
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

source(rd('src/funs/mbts.r'))

#This sets the movebank output format for timestamps for write_csv
output_column.POSIXct <- mbts

dir.create(.cleanP,showWarnings=FALSE,recursive=TRUE)

invisible(assert_that(dir.exists(.cleanP)))

#---- Count records for each entity. Fail if there are any with <= zero records
#TODO: make a countlines() function that counts lines given a filename, use this in get_study.r as well
norecs <- tibble(entity=c('study','individual','sensor','tag','deployment','event')) %>%
  mutate(num=map_int(entity,~{
    x <- file.path(.rawP,glue('{.x}.csv')) %>% path.expand
    glue('wc -l < "{x}"') %>% system(intern=T) %>% trimws %>% 
      as.integer - 1 %>% as.integer
  })) %>%
  filter(num <= 0) %>%
  pull(entity) %>% sort

invisible(assert_that(length(norecs)==0,
  msg=glue('The following entities had no records: {paste(norecs,collapse=", ")}. Exiting.')))

message(glue('Cleaning data for study {.studyid}'))
message(glue('Loading raw data from {.rawP}'))
#---- Load data ----#
sensTypes <- read_csv(rd('src/sensor_type.csv')) %>% #lookup table for tag types
  rename(sensor_type_id=id)

fields <- read_csv(rd('src/fields.csv'))

#---------------#
#---- study ----#
#---------------#
stdFields <- fields %>% 
  filter(table=='study' & !is.na(name_raw)) 

std <- read_csv(file.path(.rawP,'study.csv')) %>%
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
    select(individual_id,local_identifier,nick_name,taxon_canonical_name) %>%
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

coltypes <- fields %>% 
  filter(table=='event' & !is.na(type_raw)) %>% 
  pull('type_raw') %>% paste(collapse="")

#Note rmoveapi doesn't clean out line breaks \r\n when downloading directly to disk
#May need to address this issue in the future but there aren't any long text fields
#in the event table right now

evt0 <- read_csv(file.path(.rawP,'event.csv'),col_types=coltypes) %>%
  rename(lon=location_long,lat=location_lat)

message(glue('Retreived {format(nrow(evt0),big.mark=",")} events'))

#See report: event_raw for a table
evt <- evt0 %>% 
  filter(is.na(visible) | visible==TRUE) %>% #I think visible should always be true
  filter(!is.na(lon) & !is.na(lat)) %>% 
  filter(lon!=0 & lat!=0) %>% 
  filter(lon >= -180 & lon <= 180) %>%
  filter(lat >= -90 & lat <= 90) %>%
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

#----
#---- Removing duplicates ----#
#---- 

message('Removing duplicates')

# Per Sarah, duplicate records can exist becuase more complete
# version of the records with additional metadata values (ground speed) are added
# these more complete records are supposed to have higher event_id but this is not
# always the case.
# Furthermore, I've discovered that many of these pseudo-duplicates are off by < 1 second (often .001 or .999)
# This means the only way to remove these is to de-duplicate based on 1 second resolution.
#From Sarah: "If and when the same data are collected by a base station and imported to the study, 
# the more complete version of the same record is added. So the studies end up with semi-duplicate 
# records, with the later record added to the database being more complete."
# NOTE: I don't find this to be the case. In some cases, first event has data and the second is NA
#
# TODO: also check that two depolyments (with non-overlapping dates) works correctly.
#   It should, but double check. See 164399988 "SOI Lake Sempach Mallards"


# This creates group-level records for duplicates and assignes a dup_id
# so that duplicates can be matched in the full dataset
# allna shows cases where ground_speed is NA for both duplicate records
# usually just one record will have NA and the other will have a value
# when this is not the case, nomnotna will have value > 1

#TODO: need to truncate timestamp to seconds, and then group by that, since duplicate records can have different milliseconds
message('Identifying duplicates...')
#Need to do de-duplication against a timestamp that does not include milliseconds
evtdep <- evtdep %>% mutate(ts_sec=trunc(timestamp,units='secs'))

t1 <- Sys.time()

dupgrps <- evtdep %>% 
  group_by(individual_id,tag_id,sensor_type_id,ts_sec) %>% #include sensor_type_id in case we have multiple sensors
  summarize(num=n()) %>%
  filter(num > 1) %>%
  ungroup() %>%
  mutate(dup_id=row_number())

message(glue('Complete in {diffmin(t1)} minutes'))

message(glue('There are {nrow(dupgrps)} duplication groups'))

# Some dup groups have records that are all identical (except for event_id)
#   and do not. For those that do not, values for each record should be a subset 
#   of one of the records. Given this information, de-duplication works like this.
# 1) assume all rows in a group have information that is a subset of another row
#   in the group. so, we want to identify the row with more information. To do this,
#   sum up the number of non-na values in each row.
#   select the row with the higher number of valid nonna values
#   use filter(nonna==max(nonna)) this will take the row with the values,
#   or if the records are all the same, it will take all records
# 2) then, do a distinct() operation on all rows but event_id. This will remove the
#   remaining records that are exact duplicates. Note that we should never have
#   a record with the same individual_id, timestamp, and sensor and tag.
# 3) if after distinct operation, a dup group has more than one record, this indicates
#   that two records had conflicting nonna values, or one record has na for a column
#   and the other had a value, and this pattern was reversed for another column (i.e.
#   all records were not subsets of one record).
#   if the non-subset case occcured AND it happened more on one record than the other
#   then it is possible that the row with less information will be filtered out and
#   some information might be lost. Seems like a rare case though.
#   TODO: I can possibly do a test for the above condition.

#This does the de-duping
#Note use of function to round numeric columns to 8 decimal points
#TODO: check csv files, how many decimal places are are in those?
# need to do this otherwise distinct will not work
if(nrow(dupgrps) > 0) {
  
  message(glue('There are {dupgrps %>% filter(num>2) %>% nrow} dup groups that have > 2 records'))
  
  #This gets all duplicate records
  dupevts <- evtdep %>%
    inner_join(dupgrps,by=c('individual_id','tag_id','sensor_type_id','ts_sec')) %>%
    mutate(ts_str=mbts(timestamp)) %>% #write ts out to string in order to examine milliseconds
    arrange(individual_id,tag_id,timestamp,event_id)
  
  #Look at any dup groups with > 2 records
  #dupevts %>% filter(num>2) %>% View #dup_id: 6489
  
  message(glue('There are {nrow(dupevts)} duplicated records'))
  
  message('Removing duplicates...')
  
  t1 <- Sys.time()
  
  # Old method uses distinct() to pick undifferenciable rows from within duplication groups.
  # This doesn't make sense. If, after doing a filter, there are still multiple rows per group, this
  # means there is more than one row with the same number of values. In this case, we don't have any criteria to 
  # distinguish among the rows so just pick one. This is the approach I take below using slice(1)
  
  # undup <- dupevts %>% 
  #   mutate(numvals=rowSums(!is.na(.))) %>%
  #   group_by(dup_id) %>% 
  #   filter(numvals==max(numvals)) %>%
  #   distinct(
  #     across(-event_id,.fns=~ifelse(is.numeric(.),round(.,8),.)),
  #     .keep_all=TRUE) %>%
  #   ungroup

  #first, de-dup based on rows with the most data
    undup <- dupevts %>% 
      mutate(numvals=rowSums(!is.na(.))) %>%
      group_by(dup_id) %>% 
      filter(numvals==max(numvals)) %>%
      ungroup
    
    #Some records might have the same number of values
    #If this occurs, we can't differenciate further, so just pick one from the group
    
    y <- undup %>%
      group_by(dup_id) %>%
      mutate(num=n()) %>%
      ungroup %>%
      filter(num > 1)
    
    if(nrow(y) > 0) {
    'A total of {nrow(y)} records in {length(unique(y$dup_id))} duplication groups had records that could not be differenciated, so one row was selected per group.' %>% glue %>% message
      #View(y)
      undup <- undup %>%
        group_by(dup_id) %>%
        slice(1) %>%
        ungroup
    }

  rm(y)

  message(glue('Complete in {diffmin(t1)} minutes'))

  #This will look at any remaining duplicated groups
  # undup %>% filter(dup_id %in% undup[duplicated(undup$dup_id),]$dup_id) %>% View
  
  #Make sure no dup groups with more than one record
  # This can indicate that two records had conflicting info, or that one was not a subset of the other
  invisible(assert_that(anyDuplicated(undup$dup_id)==0)) 
  # Make sure all dup groups have at least one record
  invisible(assert_that(length(dupgrps$dup_id)==length(undup$dup_id)))

  #Undup now contains unduplicated records from all duplication groups. 
  # First, do antijoin with dupevts to figure out which records to remove
  dups <- dupevts %>% anti_join(undup, by='event_id')
  #Then, antijoin this to the full event data set to remove the dups.
  evtnodup <- evtdep %>% anti_join(dups, by='event_id')
  
  #--- do some final checking ---#
  
  message(glue('Removed {format(nrow(dups),big.mark=",")} duplicated records'))
  #The number of final non-duplicated records should be equal to the number of deployed
  # records, minus the full dataset of duplicates, plus the unduplicated records.
  invisible(assert_that(nrow(evtnodup)==nrow(evtdep) - nrow(dupevts) + nrow(undup)))

  #Double check there are no duplicate records
  message('Making sure there are no more duplicates...')
  t1 <- Sys.time()
  
  numdups <- evtnodup %>% 
    group_by(individual_id,tag_id,sensor_type_id,ts_sec) %>% #in case we have multiple sensors
    summarize(num=n()) %>%
    filter(num > 1) %>%
    ungroup() %>%
    mutate(dup_id=row_number()) %>% nrow
  
  message(glue('Complete in {diffmin(t1)} minutes'))
  
  invisible(assert_that(numdups==0,msg=glue('Found {numdups} duplicates. Exiting')))
  
} else {
  evtnodup <- evtdep
}

#Make sure all events are associated with a tag
invisible(assert_that(evtnodup %>% anti_join(dep,by='tag_id') %>% nrow == 0))

#Make sure there are still some valid events
invisible(assert_that(nrow(evtnodup) > 0))

#---- Finalize the event dataset ----#

#From Sarah regarding eobs vs. gps versions.
# Eobs was the first company for which we provided a live feed or custom format for GPS units, 
# so they created variables specific to eobs, but then later realized that it is better to 
# create variables that are not specific to a particular company when possible. So those eobs 
# variables continue to be used, and will generally be redundant with the non-eobs versions, 
# unless the eobs definition in the attribute dictionary says something more specific. 
# For example, the definition for "eobs speed accuracy estimate" says these values are not 
# very reliable for eobs tags.
# 
# So, where we have redundant gps and eobs, collapse these fields
# We don't take eobs_speed_accuracy assessment b/c data dictionary says it is very poor.

#TODO: Make this more dynamic based on fields.csv
evtFinal <- evtnodup %>%
  mutate(
    horizontal_accuracy=ifelse(is.na(eobs_horizontal_accuracy_estimate), 
      gps_horizontal_accuracy_estimate,eobs_horizontal_accuracy_estimate),
    time_to_fix=ifelse(is.na(eobs_used_time_to_get_fix),gps_time_to_fix,eobs_used_time_to_get_fix),
    fix_type=ifelse(is.na(eobs_type_of_fix),gps_fix_type,eobs_type_of_fix)
  ) %>%
  select(event_id,individual_id,lon,lat,timestamp,tag_id,sensor_type_id,
         ground_speed,gps_speed_accuracy_estimate,
         gps_dop,gps_hdop,gps_vdop,gps_satellite_count,
         horizontal_accuracy,time_to_fix,fix_type)

#--- print summary ---#
message(glue('Final num individuals: {nrow(ind)}, num events: {format(nrow(evtFinal),big.mark=",")}'))

#------------------------#
#---- Compare totals ----#
#------------------------#

message(
'Compare final totals to what is reported by movebank.
The totals should be similar but might not be exactly the same.
The totals reported by the movebank api might even be different than the totals reported on the movebank web page.
This might have to do with backend processes that are not updated on movebank yet.
In all cases I have looked at the totals from the script seem to match the web page exactly.')

mbstats <- std %>% 
  select(
    number_of_individuals,
    number_of_deployments,
    number_of_deployed_locations,
    timestamp_first_deployed_location,
    timestamp_last_deployed_location)

cstats <- tibble(number_of_individuals=nrow(ind),
       number_of_deployments=nrow(dep),
       number_of_deployed_locations=nrow(evtFinal),
       timestamp_first_deployed_location=min(evtFinal$timestamp),
       timestamp_last_deployed_location=max(evtFinal$timestamp))

bind_rows(mbstats,cstats) %>% t %>% as.data.frame %>% 
  rename(Movebank=V1,Script=V2) %>% 
  kable %>% paste(collapse='\n') %>% message

#--------------------------------------#
#---- Write out final data to csvs ----#
#--------------------------------------#

message('Saving data to csv files')

write_csv(std,file.path(.cleanP,'study.csv'),na="")
write_csv(ind,file.path(.cleanP,'individual.csv'),na="")
write_csv(sens,file.path(.cleanP,'sensor.csv'),na="")
write_csv(tag,file.path(.cleanP,'tag.csv'),na="")
write_csv(dep,file.path(.cleanP,'deployment.csv'),na="")
write_csv(evtFinal,file.path(.cleanP,'event.csv'),na="")

message(glue('Script complete in {diffmin(t0)} minutes'))
