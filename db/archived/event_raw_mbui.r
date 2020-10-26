#Mostly working code to convert data from movebank ui to api format

#----
#---- Use this if I had to download from movebank web UI
#----
sed -i '' '1s/\-/\_/g' event_raw_ui.csv 
#sed -i '' '1s/location\_long/lon/' event_raw.csv #Don't need to do this, just rename after loading
#sed -i '' '1s/location\_lat/lat/' event_raw.csv #Don't need to do this, just rename after loading

#Cols for reading file from movebank UI
cols <- cols(
  event_id = col_double(),
  lon = col_double(), #should be location_long, don't rename using sed
  lat = col_double(), #should be location_lon
  timestamp = col_datetime(format = ""),
  ground_speed = col_double(),
  manually_marked_outlier = col_logical(),
  visible = col_logical(),
  
  #These don't exist in the file. Need to use identifiers below
  # individual_id = col_double(),
  # tag_id = col_double(),
  # sensor_type_id = col_double(),
  
  #I will need to use these to reverse-lookup id's using metadata tables
  tag_local_identifier = col_integer(),
  individual_local_identifier = col_character(),
  sensor_type = col_character()
)

evt0 <- read_csv(.evtrawP,col_types = cols) %>%
  select(event_id,lon,lat,timestamp,ground_speed,manually_marked_outlier,visible,
         tag_local_identifier,individual_local_identifier,sensor_type)

#cd ~/projects/movebankdb
#knitcopy ~/projects/movebankdb/src/reports/event_raw/event_raw.rnw ~/projects/movebankdb/analysis/sempach.pdf #make sure input file is set to obs.csv

#What to filter out
# * Timestamps that are more than one day in the future
# * Visible == FALSE
# * Manually_marked_outlier==TRUE
# * Missing lon, lat, timestamp
# * Events later than today's date
# * Events before or after valid deployment
# * Removing duplicate timestamps
#TODO: 
#   Remove points after animal died

#TODO: how do I get sensor_id? I have sensor_type. 
# I use use tag_id and sensor_type_id. Get sensor_type_id by joining with sensor_type

#Check tag_id
unique(evt0$tag_local_identifier)
any(is.na(evt0$tag_local_identifier)) #Should be false
any(is.null(evt0$tag_local_identifier)) #Should be false

#Check individual_id
unique(evt0$individual_local_identifier)
any(is.na(evt0$individual_local_identifier)) #Should be false
any(is.null(evt0$individual_local_identifier)) #Should be false

#Check sensor_type
unique(evt0$sensor_type)
any(is.na(evt0$sensor_type)) #Should be false
any(is.null(evt0$sensor_type)) #Should be false

#!!!!!!!Danger, hard coding here. Should join to sensor_type table
evt0_1 <- evt0 %>% 
  left_join(
    tag %>% select(tag_id,tag_local_identifier=local_identifier),
    by='tag_local_identifier') %>% 
  left_join(
    ind %>% select(individual_id,individual_local_identifier=local_identifier),
    by='individual_local_identifier') %>%
  mutate(sensor_type_id=653) %>% #!!!!!!!Danger, hard coding here. Should join to sensor_type table
  left_join(
    sens,by=c('sensor_type_id','tag_id')) #%>% head %>% View


nrow(evt0)==nrow(evt0_1) #Should be true

unique(evt0_1$tag_id)
any(is.na(evt0_1$tag_id)) #Should be false
any(is.null(evt0_1$tag_id)) #Should be false

unique(evt0_1$individual_id)
any(is.na(evt0_1$individual_id)) #Should be false
any(is.null(evt0_1$individual_id)) #Should be false

unique(evt0_1$sensor_id)
any(is.na(evt0_1$sensor_id)) #Should be false
any(is.null(evt0_1$sensor_id)) #Should be false