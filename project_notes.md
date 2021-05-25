## TODO
* First thing to do
* Second thing to do

## Technical documentation

Movebank may store all date fields as timestamps with zero time. Just to make things easy I will do the same. I believe this only applies to earliest_date_born, latest_date_born, exact_date_of_birth.

### Milliseconds

Storing and processing timestamps with milliseconds takes some special processing

* Loading timestamps from csv. Use read_csv(). Automatically parses milliseconds
* Writing timestamps to csv. Use write_csv(). Need to set output_column attribute to desired format

```r
output_column.POSIXct <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%OS3", tz='UTC')
}
```

* Writing timestamps to database. Stored in database as text strings, so need to format first.

```r
movebankTs <- function(x) strftime(x,format='%Y-%m-%d %H:%M:%OS3',tz='UTC')

dat %>% 
  mutate_if(is.POSIXct,movebankTs) %>%
  dbAppendTable(db, entity, .)
```

* Reading timestamps from database. Need to convert from string timestamps. Use ymd_hms(), because as.POSIXct does not seem to store milliseconds correctly.

```r
dbGetQuery(db, sql, params = list(.studyid)) %>% 
  as_tibble %>%
  mutate(timestamp=ymd_hms(timestamp))
```  