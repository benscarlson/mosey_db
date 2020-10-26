--- THIS is maintenence on an old version of the database.
--- All changes have been made to initilization and loading scripts
--- and all data has been re-downloaded. So, these changes are no longer relevant.

--- TODO: I changed column affinities in db browser, but need to update the data where appropriate
---   Change TRUE/FALSE to 1/0
----  Change numeric dates to text dates
----TODO: update all 'NA' values to NULL in all tables/columns

---
--- SQL that needs to be executed after creating databases from scratch
---
--- NONE

---
--- All SQL below does not need to be re-executed after creating database from scratch
---  becuase I fixed the creation scripts. (Double check!)
---

---
---  Update numeric dates to character strings
---  Only occurs for data input using DBI. In the future, use .import instead
--- 

---  2020-02-18. 
--- deploy_off_timestamp
select deploy_off_timestamp d,  
	strftime('%Y-%m-%dT%H:%M:%SZ',datetime(deploy_off_timestamp, 'unixepoch')) as str_ts
from deployment 
where cast(d as integer) = d
limit 10

-- wherever deploy_off is a number, convert it to a character
update deployment  
	set deploy_off_timestamp = strftime('%Y-%m-%dT%H:%M:%SZ',datetime(deploy_off_timestamp, 'unixepoch'))
where cast(deploy_off_timestamp as integer) = deploy_off_timestamp

--- deploy_on_timestamp

--Spot check a couple values in csvs to make sure dates/times converted correctly
select * from deployment where deployment_id = 6781221 -- This row has a numeric date for deploy_on: 1308697200
select * from individual where individual_id = 6781216 --study_id: 3807090
-- deploy_on from csv file: 2011-06-21T23:00:00Z
-- deploy_on from database: 1308697200
-- deploy_on from query: 2011-06-21T23:00:00Z. Matches!!
select deploy_on_timestamp d,  
	strftime('%Y-%m-%dT%H:%M:%SZ',datetime(deploy_on_timestamp, 'unixepoch')) as str_ts
from deployment 
where deployment_id = 6781221

select * from deployment where deployment_id = 895342116 -- This row has a numeric date for deploy_on: 1560816000
select * from individual where individual_id = 895341898 --study_id: 170501269
-- deploy_on from csv file: 2019-06-18T00:00:00Z
-- deploy_on from database: 1560816000
-- deploy_on from query: 2019-06-18T00:00:00Z. Matches!!
select deploy_on_timestamp d,  
	strftime('%Y-%m-%dT%H:%M:%SZ',datetime(deploy_on_timestamp, 'unixepoch')) as str_ts
from deployment 
where deployment_id = 895342116

-- wherever deploy_on is a number, convert it to a character
select count(*) from deployment  where cast(deploy_on_timestamp as integer) = deploy_on_timestamp --68 rows

update deployment  
	set deploy_on_timestamp = strftime('%Y-%m-%dT%H:%M:%SZ',datetime(deploy_on_timestamp, 'unixepoch'))
where cast(deploy_on_timestamp as integer) = deploy_on_timestamp --68 rows affected

--- Change data affinity on deployment
--- Updated create_db.sql so don't have to do this in the future.
--- To change data affinity, I used DB Browser UI.

--- Event table: event.timestamp ---

-- shows all rows where timestamp is a number
select count(*) from event where cast(timestamp as integer) = timestamp --2,432,952

-- wherever timestamp is a number, convert it to a character
update event  
	set timestamp = strftime('%Y-%m-%dT%H:%M:%SZ',datetime(timestamp, 'unixepoch'))
where cast(timestamp as integer) = timestamp --2,432,952 rows affected

--- Change data affinity on event.timestamp
--- Updated create_db.sql so don't have to do this in the future.
--- To change data affinity, I used DB Browser UI.

---
--- Update NA values to null. Make sure upstream processes are updated so I won't have to do this in the future
---

--- 2020-02-18. Deployment table
select * from deployment where deploy_off_timestamp = 'NA' --55 rows
select * from deployment where deploy_on_timestamp = 'NA' -- 1 rows
select * from deployment where local_identifier = 'NA' -- 68 rows

update deployment set deploy_off_timestamp = null where deploy_off_timestamp = 'NA' -- 55 rows affected
update deployment set deploy_on_timestamp = null where deploy_on_timestamp = 'NA' -- 1 rows affected
update deployment set local_identifier = null where local_identifier = 'NA' -- 68 rows affected


--- 2020-02-19. Change data affinity on deployment
select count(*) from event where upper(trim(event_id)) = 'NA' -- 0 rows
select count(*) from event where upper(trim(ground_speed)) = 'NA' --185,223 rows
select count(*) from event where upper(trim(timestamp)) = 'NA' --

update event set ground_speed = null where upper(trim(ground_speed)) = 'NA'  --185,223 rows affected
---

create index idx_event_timestamp on event (timestamp);

--- 2020-02-25. Add index
create index idx_event_individual_id on event (individual_id);

---- Fix NA values in individual table (all rows not where)
update individual set nick_name = null where nick_name = 'NA'
update individual set sex = null where sex = 'NA'
update individual set access_profile_id = null where access_profile_id = 'NA'
update individual set default_profile_eventdata_id = null where default_profile_eventdata_id = 'NA'
update individual set earliest_date_born = null where earliest_date_born = 'NA'
update individual set latest_date_born = null where latest_date_born = 'NA'
update individual set exact_date_of_birth = null where exact_date_of_birth = 'NA'
update individual set external_id = null where external_id = 'NA'
update individual set external_id_namespace_id = null where external_id_namespace_id = 'NA'
update individual set death_comments = null where death_comments = 'NA'

--- 2020-03-01. Updated column affinities in db browser to better match data (T/F data, Dates)

---
--- 2020-03-03. 
---

-- Set all NA values to null. See script src/db/update_null.r

update study 
	set has_quota = case 
		when has_quota = 'TRUE' then 1
		when has_quota = 'FALSE' then 0
	end
where has_quota in ('TRUE','FALSE')

update study 
	set i_am_owner = case 
		when i_am_owner = 'TRUE' then 1
		when i_am_owner = 'FALSE' then 0
	end
where i_am_owner in ('TRUE','FALSE')

-- created script to do the rest. See script src/db/update_tf.r

-- update study timestamps

update study
	set timestamp_first_deployed_location = strftime('%Y-%m-%dT%H:%M:%SZ',datetime(timestamp_first_deployed_location, 'unixepoch'))
where cast(timestamp_first_deployed_location as integer) = timestamp_first_deployed_location 

update study
	set timestamp_last_deployed_location = strftime('%Y-%m-%dT%H:%M:%SZ',datetime(timestamp_last_deployed_location, 'unixepoch'))
where cast(timestamp_last_deployed_location as integer) = timestamp_last_deployed_location

-- 2020-09-03 Add error columns to event table
-- run these at the command line and they are very quick. Hangs in db browser.
-- Seems I don't need to specify NULL for column defintion (NULL must be assumed)
alter table event add column gps_dop REAL;
alter table event add column gps_hdop REAL;
alter table event add column gps_vdop REAL;
alter table event add column gps_satellite_count REAL;
alter table event add column horizontal_accuracy REAL;
alter table event add column time_to_fix REAL;