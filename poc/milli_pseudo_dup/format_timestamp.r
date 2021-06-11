x <- std$timestamp_last_deployed_location

class(x)
attr(x,"tzone") #UTC

#Format directly from movebank api (event direct download)
#'2019-02-20 13:13:01.000'

#Format downloaded using the movebank web ui 
#'2019-04-16 06:14:42.000'

format(x, "%Y-%m-%d %H:%M:%OS3")
#"2021-03-07 10:47:06.000"

format(x, "%Y-%m-%d %H:%M:%OS3", tz='UTC') #Note same as above, since x is UTC
#"2021-03-07 10:47:06.000"

#This sets the output format for timestamps for write_csv
#Stack exchange has an example using POSIXt but only POSIXct seems to work
output_column.POSIXct <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%OS3", tz='UTC')
}

write_csv(std,file.path(.outP,'study.csv'),na="")

format_csv(std)

#Timestamps seem to be read-in assuming UTC

#Value in raw file is: 
# 2019-02-20 12:57:37.000
y <- read_csv(file.path(.outP,'event.csv'),col_types = 'ddddTddddldddddddcdddddc')

y1 <- y$timestamp[1] #"2019-02-20 12:57:37 UTC"
attr(y1,"tzone") #UTC

#Output is the same when using output_column
y %>% slice(1) %>% select(timestamp) %>% format_csv %>% cat
#timestamp
#2019-02-20 12:57:37.000


#Investigate why latest_date_born is a timestamp with zero time and not a date
x <- ind %>% filter(individual_id==657665432) %>% pull('latest_date_born')
class(x)

req <- 'https://www.movebank.org/movebank/service/direct-read?attributes=id,latest_date_born&entity_type=individual&study_id=657640587'

auth <- httr::authenticate(cred$user,cred$pass)

resp <- httr::GET(req, auth)

cont <- httr::content(resp, as='text', encoding='UTF-8')

#confirmed: latest_date_born comes back as a timestamp with 0 time:
# 2019-05-01 00:00:00.000

#So, it could be that all dates are stored as timestamps in movebank.