
select i.study_id, t.study_name, 
	count(*) as num_individuals, 
	sum(s.num) as num_points,
	strftime('%Y',min(s.min_ts)) as min_year,
	strftime('%Y',max(s.max_ts)) as max_year
from individual i
inner join study t
on i.study_id = t.study_id
inner join 
	(select individual_id, count(*) as num,
		min(timestamp) as min_ts,
		max(timestamp) as max_ts
	from event 
	group by individual_id) s
on i.individual_id = s.individual_id
group by i.study_id