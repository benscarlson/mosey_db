#!/usr/bin/env Rscript --vanilla

#TODO: updated rmoveapi to strip \r\n and trim ws. Don't need to do this in this script
# anymore. But, can't hurt to double check incoming data. see rmoveapi test_get_data.r
#TODO: if script fails, remove download folder?
#TODO: get sensor type names so I can print names instead of ids

'
Get and clean all data for a study from movebank api

Usage:
get_study_data.r <studyid> <out> [-t] [--auth=<auth>] [--seed=<seed>]
get_study_data.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
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
  
  .wd <- '~/projects/covid/analysis/movebankdb'
  #.script <- 'src/db/get_study_data.r' #Currently executing script
  .seed <- NULL
  .test <- TRUE
  rd <- here

  .studyid <- 1228193116
  .auth <- NULL
  .out <- "/Volumes/WD4TB/projects/covid/data"
  #.out <- 'test'
  
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
  .out <- trimws(ag$out)
  .outP <- ifelse(isAbsolute(.out),.out,file.path(.wd,.out))
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressPackageStartupMessages({
  library(DBI)
  library(getPass)
  library(keyring)
  library(knitr)
  library(rmoveapi)
  library(RSQLite)
  library(yaml)
  #library(R.utils)
})

source(rd('src/funs/breezy_funs.r'))


#---- Local parameters ----#
.evtrawP <- file.path(.outP,'event_raw.csv')
.dbPF <- file.path(.wd,'data/movebank.db')

#---- Initialize database ----#
db <- DBI::dbConnect(RSQLite::SQLite(), .dbPF)

invisible(assert_that(length(dbListTables(db))>0))


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

message(glue('Downloading and cleaning data for study {.studyid}'))

#---- Load data ----#
sensTypes <- read_csv(rd('src/sensor_type.csv'),col_types=cols()) %>% #lookup table for tag types
  rename(sensor_type_id=id)

#---------------#
#---- study ----#
#---------------#
message('Getting study data')

attributes <- dbListFields(db,'study')
#Need to rename request attributes if local database has different
# names than movebank
attributes[attributes=='study_id'] <- 'id'
attributes[attributes=='study_name'] <- 'name'

std0 <- getStudy(.studyid,params=list(attributes=attributes))

std <- std0 %>% 
  rename(study_id=id,study_name=name) 

message(glue('Study name is: {std$study_name}'))
message(glue('Reported num individuals: {std$number_of_individuals}, num events: {format(std$number_of_deployed_locations,big.mark=",")}'))
#View(t(std))
invisible(assert_that(!std %>% map(~str_detect(.,'[\r\n]')) %>% unlist %>% any(na.rm=TRUE))) #Check for nonprinting line breaks
invisible(assert_that(!std %>% map(~str_detect(.,'^\\s+')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space in beginning
invisible(assert_that(!std %>% map(~str_detect(.,'\\s+$')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space at end

#--------------------#
#---- individual ----#
#--------------------#
message('Getting individual data')

attstr <- 'id, local_identifier, nick_name, study_id, ring_id, sex, taxon_id, taxon_canonical_name, access_profile_id, 
default_profile_eventdata_id, earliest_date_born, latest_date_born, exact_date_of_birth, external_id, external_id_namespace_id, 
i_am_owner, death_comments, comments'
attributes <- trimws(str_split(attstr,',')[[1]])

#TODO: can fail here if I don't have access. Need to add in error handling
ind00 <- getIndividual(.studyid,params=list(attributes=attributes),accept_license=TRUE)  %>% 
  rename(individual_id=id) 

spp <- unique(ind00$taxon_canonical_name)

message(glue('Found the following species: {paste(spp,collapse=", ")}'))

if(all(is.na(ind00$taxon_canonical_name))) {
  message('Species name is empty for all individuals. Assume this is okay. Proceeding without checking for invalid species')
  ind0 <- ind00
} else {
  message(glue('Checking for individuals with invalid species identity (e.g. Homo sapiens)'))
  
  #Below code will also remove any individuals where species is NA
  ind0 <- ind00  %>%
    filter(!str_detect(tolower(taxon_canonical_name),'homo sapien'))
  
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
}
  


#View(ind0)
#In many cases species names have not been uploaded, so can't check the condition below
#invisible(assert_that(ind0 %>% filter(is.na(taxon_canonical_name)) %>% nrow == 0)) #All individuals have species assigned
invisible(assert_that(!ind0 %>% map(~str_detect(.,'[\r\n]')) %>% unlist %>% any(na.rm=TRUE))) #Check for nonprinting line breaks
invisible(assert_that(!ind0 %>% map(~str_detect(.,'^\\s+')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space in beginning
invisible(assert_that(!ind0 %>% map(~str_detect(.,'\\s+$')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space at end

#----------------#
#---- sensor ----#
#----------------#
message('Getting sensor data')

sens0 <- getSensor(.studyid) %>% rename(sensor_id=id)

message('Found the following sensors:')

sens0 %>% distinct(sensor_type_id) %>% 
  left_join(sensTypes %>% select(sensor_type_id,name),by='sensor_type_id') %>%
  arrange(sensor_type_id) %>% kable

#View(sens0)

invisible(assert_that(!sens0 %>% map(~str_detect(.,'[\r\n]')) %>% unlist %>% any(na.rm=TRUE))) #Check for nonprinting line breaks
invisible(assert_that(!sens0 %>% map(~str_detect(.,'^\\s+')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space in beginning
invisible(assert_that(!sens0 %>% map(~str_detect(.,'\\s+$')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space at end

#----------------#
#---- tag ----#
#----------------#
message('Getting tag data')

attstr <- 'beacon_frequency,comments,id,local_identifier,manufacturer_name,model,processing_type,serial_no,tag_failure_comments,tag_production_date,weight'
attributes <- trimws(str_split(attstr,',')[[1]])

tag0 <- getTag(.studyid,params=list(attributes=attributes)) %>% 
  select(tag_id=id,local_identifier,manufacturer_name,everything()) %>%
  select(-comments,everything()) #This puts comment at the end

message('Found the following tag types:')
tag0 %>% distinct(manufacturer_name) %>% arrange(manufacturer_name) %>% kable

#View(tag0)

invisible(assert_that(!tag0 %>% map(~str_detect(.,'[\r\n]')) %>% unlist %>% any(na.rm=TRUE))) #Check for nonprinting line breaks
invisible(assert_that(!tag0 %>% map(~str_detect(.,'^\\s+')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space in beginning
invisible(assert_that(!tag0 %>% map(~str_detect(.,'\\s+$')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space at end


#--------------------#
#---- deployment ----#
#--------------------#
message('Getting deployment data')

attstr <- 'id, local_identifier, individual_id, tag_id, deploy_on_timestamp, deploy_off_timestamp'
#attstr <- 'access_profile_id, animal_life_stage, animal_mass, animal_reproductive_condition, attachment_type, behavior_according_to, comments, data_processing_software, deploy_off_latitude, deploy_off_longitude, deploy_off_person, deploy_off_timestamp, deploy_on_latitude, deploy_on_longitude, deploy_on_person, deploy_on_timestamp, deployment_end_comments, deployment_end_type, duty_cycle, external_id, external_id_namespace_id, geolocator_calibration, geolocator_light_threshold, geolocator_sensor_comments, geolocator_sun_elevation_angle, habitat_according_to, i_am_owner, id, individual_id, local_identifier, location_accuracy_comments, manipulation_comments, manipulation_type, partial_identifier, study_id, study_site, tag_id, tag_readout_method'
attributes <- trimws(str_split(attstr,',')[[1]])

dep0 <- getDeployment(.studyid,params=list(attributes=attributes)) %>% rename(deployment_id=id)

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
#NOTE: if running script manually, invisible(asser_that()) will only print message if there is an error

invisible(assert_that(!sens0 %>% map(~str_detect(.,'[\r\n]')) %>% unlist %>% any(na.rm=TRUE))) #Check for nonprinting line breaks
invisible(assert_that(!sens0 %>% map(~str_detect(.,'^\\s+')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space in beginning
invisible(assert_that(!sens0 %>% map(~str_detect(.,'\\s+$')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space at end

#Seems all combinations of missing deploy on and deploy off fields occur and are valid
# I have tested for NA, timestamp. but not NA, NA. Should work though.

#should never have a deploy off without deploy on. the other three possibilities are valid
#   invisible(assert_that(dep %>% filter(is.na(deploy_on_timestamp) & !is.na(deploy_off_timestamp)) %>% nrow == 0))  #should be 0

#actually okay if both deploy on and deploy off are na, but this is rare. If it happens double check data.
#need to include this case in the event table logic below
#   invisible(assert_that(dep %>% filter(is.na(deploy_on_timestamp) & is.na(deploy_off_timestamp)) %>% nrow == 0))

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

message('Getting GPS (653) event data. This can take awhile...')

attributes <- c('event_id','individual_id','location_long','location_lat','timestamp',
                'sensor_type_id','tag_id',
                'ground_speed','gps_speed_accuracy_estimate',
                'visible',
                'gps_dop','gps_hdop','gps_vdop',
                'eobs_horizontal_accuracy_estimate','gps_horizontal_accuracy_estimate',
                'location_error_numerical', 'location_error_percentile', 'location_error_text',
                'gps_fix_type', 'gps_satellite_count', 'gps_time_to_fix', 'eobs_type_of_fix', 
                'eobs_used_time_to_get_fix', 'eobs_status')
t1 <- Sys.time()
evt0 <- getEvent(.studyid,attributes,sensor_type_id=653,save_as=.evtrawP) #'~/scratch/sempach.csv' ts_start='2013-01-01',
t2 <- Sys.time()
message(glue('Complete in {diffmin(t1,t2)} minutes'))

cols <- cols(
  event_id = col_double(),
  individual_id = col_double(),
  location_long = col_double(),
  location_lat = col_double(),
  timestamp = col_datetime(format = ""),
  tag_id = col_double(),
  sensor_type_id = col_double(),
  ground_speed = col_double(),
  gps_speed_accuracy_estimate = col_double(),
  visible = col_logical(),
  gps_dop = col_double(),
  gps_hdop = col_double(),
  gps_vdop = col_double(),
  eobs_horizontal_accuracy_estimate = col_double(),
  gps_horizontal_accuracy_estimate = col_double(),
  location_error_numerical = col_double(), 
  location_error_percentile = col_double(), 
  location_error_text = col_character(),
  gps_fix_type = col_double(), 
  gps_satellite_count = col_double(), 
  gps_time_to_fix = col_double(), 
  eobs_type_of_fix = col_double(), 
  eobs_used_time_to_get_fix = col_double(), 
  eobs_status = col_character()
)

message('Loading raw data...')
#Need to clean up text fields because rmovebankapi doesn't
# do this when downloading directly to disk.
evt0 <- read_csv(.evtrawP,col_types = cols) %>% rename(lon=location_long,lat=location_lat)%>%
  dplyr::mutate_if(is.character,
    function(x) {trimws(stringr::str_replace_all(x, '[\r\n]',' '))})
message(glue('Retreived {format(nrow(evt0),big.mark=",")} raw events'))

#Too much data for these checks to work. Need to try a different way and/or assume above 
# string replacement works.
# invisible(assert_that(!evt0 %>% map(~str_detect(.,'[\r\n]')) %>% unlist %>% any(na.rm=TRUE))) #Check for nonprinting line breaks
# invisible(assert_that(!evt0 %>% map(~str_detect(.,'^\\s+')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space in beginning
# invisible(assert_that(!evt0 %>% map(~str_detect(.,'\\s+$')) %>% unlist %>% any(na.rm=TRUE))) #Check for white space at end

#See report: event_raw for a table
evt <- evt0 %>% #evt0_1 %>% #If used api, its evt0, if web ui its evt0_1. streamline this.
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

message('Starting deployment filtering')

message('Found the following deploy on/off patterns')
dep %>% mutate(
    na_dep_on=is.na(deploy_on_timestamp),
    na_dep_off=is.na(deploy_off_timestamp)) %>%
  distinct(na_dep_on,na_dep_off) %>%
  mutate(across(.fns=~ifelse(.,NA,'timestamp'))) %>%
  arrange(na_dep_on,na_dep_off) %>% kable

evtdep <- evt %>% 
  inner_join(dep %>% 
    select(deployment_id,individual_id,tag_id,deploy_on_timestamp,deploy_off_timestamp),
    by=c('individual_id', 'tag_id')) %>%
  filter( 
    ((timestamp >= deploy_on_timestamp) & (timestamp < deploy_off_timestamp | is.na(deploy_off_timestamp))) |
    (is.na(deploy_on_timestamp) & (timestamp < deploy_off_timestamp | is.na(deploy_off_timestamp)))
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
# need to do this otherwise distinct will not work
if(nrow(dupgrps) > 0) {
  
  message(glue('There are {dupgrps %>% filter(num>2) %>% nrow} dup groups that have > 2 records'))
  
  #This gets all duplicate records
  dupevts <- evtdep %>%
    inner_join(dupgrps,by=c('individual_id','tag_id','sensor_type_id','timestamp')) %>%
    arrange(individual_id,tag_id,timestamp,event_id)
  
  #Look at any dup groups with > 2 records
  #dupevts %>% filter(num>2) %>% View
  
  message(glue('There are {nrow(dupevts)} duplicated records'))
  

  message('Removing duplicates...')
  
  t1 <- Sys.time()
  
  undup <- dupevts %>% 
    mutate(numvals=rowSums(!is.na(.))) %>%
    group_by(dup_id) %>% 
    filter(numvals==max(numvals)) %>%
    distinct(
      across(-event_id,.fns=~ifelse(is.numeric(.),round(.,8),.)),
      .keep_all=TRUE) %>%
    ungroup

  message(glue('Complete in {diffmin(t1)} minutes'))

  #This will look at any remaining duplicated groups
  # undup %>% filter(dup_id %in% undup[duplicated(undup$dup_id),]$dup_id) %>% View
  
  #Make sure no dup groups with more than one record
  # This can indicate that two records had conflicting info, or that one was not a subset of the other
  invisible(assert_that(anyDuplicated(undup$dup_id)==0)) 
  # Make sure all dup groups have at least one record
  invisible(assert_that(length(dupgrps$dup_id)==length(undup$dup_id)))

  #Undup now contains all unduplicated records. Do antijoin with dupevts to figure out which
  # records to throw away.
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
    group_by(individual_id,tag_id,sensor_type_id,timestamp) %>% #in case we have multiple sensors
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
evtFinal <- evtnodup %>%
  mutate(
    horizontal_accuracy=ifelse(is.na(eobs_horizontal_accuracy_estimate), gps_horizontal_accuracy_estimate,eobs_horizontal_accuracy_estimate),
    time_to_fix=ifelse(is.na(eobs_used_time_to_get_fix),gps_time_to_fix,eobs_used_time_to_get_fix),
    fix_type=ifelse(is.na(eobs_type_of_fix),gps_fix_type,eobs_type_of_fix)
  ) %>%
  select(event_id,individual_id,lon,lat,timestamp,tag_id,sensor_type_id,
         ground_speed,gps_speed_accuracy_estimate,
         gps_dop,gps_hdop,gps_vdop,gps_satellite_count,
         horizontal_accuracy,time_to_fix,fix_type)

#--- print summary ---#
message(glue('Final num individuals: {nrow(ind)}, num events: {format(nrow(evtFinal),big.mark=",")}'))

# evtFinal %>%
#   group_by(individual_id) %>%
#   summarize(num=n(),start_dte=as.Date(min(timestamp)),end_dte=as.Date(max(timestamp))) %>% 
#   arrange(individual_id) %>% kable

#--------------------------------------#
#---- Write out final data to csvs ----#
#--------------------------------------#

message('Saving data to csv files')

write_csv(std,file.path(.outP,'study.csv'))
write_csv(ind,file.path(.outP,'individual.csv'))
write_csv(sens,file.path(.outP,'sensor.csv'))
write_csv(tag,file.path(.outP,'tag.csv'))
write_csv(dep,file.path(.outP,'deployment.csv'))
write_csv(evtFinal,file.path(.outP,'event.csv'))

message(glue('Script complete in {diffmin(t0)} minutes'))
