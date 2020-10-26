-- runs in ~25 seconds

-- explain query plan
select s.study_name,i.study_id, i.local_identifier,i.nick_name,q.num, q.min_ts, q.max_ts
from individual i inner join (
	select individual_id, count(*) as num, min(timestamp) as min_ts, max(timestamp) as max_ts
	from event
	group by individual_id) q
on q.individual_id = i.individual_id
inner join study s on i.study_id = s.study_id
order by i.study_id