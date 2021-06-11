---- ms3 database

select count(*) from event 
where event_id in (194510657,194500753) 
group by timestamp

strftime(x + 0.0005, "%Y-%m-%d %H:%M:%OS3", tz='UTC')

--Duplicates if we in include milliseconds (introduced since R does not write milliseconds faithfully)
select individual_id,timestamp as ts_sec, count(*) as num from event 
group by individual_id, tag_id, sensor_type_id, timestamp
having num > 1

--duplicates if we ignore milliseconds
--Result: 7082 rows returned in 111971ms
select individual_id,strftime('%Y-%m-%d %H:%M:%S', timestamp) as ts_sec, count(*) as num from event 
group by individual_id, tag_id, sensor_type_id, ts_sec
having num > 1

select strftime('%Y-%m-%d %H:%M:%S', 'now', 'utc')

--Don't specify 'utc', if the ts already represents utc. This makes strftime assume the supplied timestamp is in local time, and converts based on this.
select timestamp, strftime('%Y-%m-%d %H:%M:%S', timestamp) as ts_sec from event 
where event_id in (194510657,194500753) 

---
--- 1000 cranes database
---

--Duplicates if we in include milliseconds with timestamp (introduced since R does not write milliseconds faithfully)
select individual_id,timestamp, count(*) as num 
from event 
group by individual_id, tag_id, sensor_type_id, timestamp
having num > 1

