# Getting started

## Using movedb


This section assumes you have a movedb sqlite file. See below if you want to create and populate a new database.

Each example below shows you how to run a query on the database using dplyr, and the equivilent syntax using sql.

````{r}
library(DBI)
library(dplyr)
library(RSQLite)

.wd <- '~/projects/mycoolproject/analysis'
.dbPF <- file.path(.wd,'data/movebank.db')

db <- DBI::dbConnect(RSQLite::SQLite(), .dbPF)

invisible(assert_that(length(dbListTables(db))>0)) # Ensure that you have loaded the database correctly

stdtb <- tbl(db,'study')
indtb <- tbl(db,'individual')
evttb <- tbl(db,'event')
````

#### Get get info on all studies in the database

````{r}
stdtb %>% select(study_id,study_name) %>% as_tibble

'select study_id, study_name from study' %>%
  dbGetQuery(db,.)
````

#### How many individuals in the database?

Note the differences between these two approaches. The first pulls all data into R and then counts the rows. The second counts the rows in the database and just returns a single number.

````{r}

indtb %>% as_tibble %>% nrow

'select count(*) as num from individual' %>%
  dbGetQuery(db,.)
````

#### How many individuals per study?

````{r}
indtb %>% group_by(study_id) %>% summarize(num=n())
  
'select study_id, count(*) as num 
  from individual 
  group by study_id' %>%
  dbGetQuery(db,.)
````

#### How many locations per individual, for a particular study?

````{r}

evttb %>% 
  inner_join(indtb,by='individual_id') %>% 
  filter(study_id==954787575) %>% 
  group_by(individual_id) %>%
  summarize(num=n())

'select i.individual_id, count(*) as num
  from event e 
  inner join individual i
  on e.individual_id = i.individual_id
  where study_id = 954787575
  group by i.individual_id' %>%
  dbGetQuery(db,.)
````

Joining to a really large event table can take a long time. An alternative way to run this query is to use an sql subquery. I dont think this approach is possible using dbplyr.

````{r}
'select individual_id, count(*) as num
  from event e 
  where individual_id in (
    select individual_id from individual where study_id = 954787575
  )
  group by individual_id' %>%
  dbGetQuery(db,.)

````

#### Find start and end dates, per project

````{r}
evttb %>% 
  inner_join(indtb,by='individual_id') %>% 
  group_by(study_id) %>%
  summarize(min=min(timestamp), max=(timestamp))

'select i.individual_id, min(timestamp) as min, max(timestamp) as max
  from event e 
  inner join individual i
  on e.individual_id = i.individual_id
  group by i.study_id' %>%
  dbGetQuery(db,.)
````

#### Get all data between start and end dates, for all projects

````{r}
evttb %>% 
  filter(between(timestamp,'2019-09-01','2019-10-01'))

'select * from event 
  where timestamp between "2019-09-01" and "2019-10-01"' %>%
  dbGetQuery(db,.)
````

## Create and populate a new movebankdb

Warning! I haven't tested this yet. Please let me know if you run into issues.

### Create a new database

You can create a new database by running the script `src/db/create_db.sql` from the command line.

Set the location of the source code. This should be wherever you placed the movebankdb project. Here, I've placed it in `~/projects`

`src=~/projects/movebankdb/src`

Navigate to the directory where you want to create a movebankdb.

`cd ~/projects/mycoolproject/analysis/data`

Create the database

`cat $src/db/create_db.sql | sqlite3 mydata.db`

You can already view the database, even without any data. One great option is to use [db browser](https://sqlitebrowser.org/)

### Populate the database

TOOD: example using get, clean, load, validate scripts.

TODO: example using workflow script that does all of the above.