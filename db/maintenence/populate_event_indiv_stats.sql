insert into event_indiv_stats
select
	individual_id,
	min(timestamp) as ts_min,
	max(timestamp) as ts_max,
	min(lon) as lon_min,
	max(lon) as lon_max,
	avg(lon) as lon_mean,
	min(lat) as lat_min,
	max(lat) as lat_max,
	avg(lat) as lat_mean
from event
group by individual_id