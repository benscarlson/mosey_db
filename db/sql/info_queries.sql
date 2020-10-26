-- number of individuals per study
select s.name, count(*) 
from  individual i
inner join study s on i.study_id = s.study_id
group by s.name

-- start/end dates for all individuals in a study
explain query plan 
select i.individual_id, count(*), min(timestamp), max(timestamp)
from event e
inner join individual i on e.individual_id = i.individual_id
inner join study s on i.study_id = s.study_id
where s.study_id = 10763606
group by i.individual_id

-- number of individuals and total observations per year for a single study
explain query plan 
select year, count(*) as individuals, sum(obs) as obs, min(min_ts) as min_ts, max(max_ts) as max_ts
from (
	select i.individual_id, strftime('%Y',timestamp) as year, count(*) as obs, min(timestamp) as min_ts, max(timestamp) as max_ts
	from event e
	inner join individual i on e.individual_id = i.individual_id
	inner join study s on i.study_id = s.study_id
	where s.study_id = 177380177 and i.taxon_canonical_name = 'Ciconia ciconia'
	group by i.individual_id, year)
group by year

select i.*
from individual i 
inner join study s on i.study_id = s.study_id
where s.study_id = 10763606

-- number of points given study and duration
select count(*)
from event e
inner join individual i on e.individual_id = i.individual_id
where i.study_id = 10763606 and e.timestamp >= '2013-04-01' and e.timestamp < '2013-09-01'

-- number of points per study. takes 3 min to run. Maybe save into info table?
select i.study_id, s.name, count(*)
from event e
inner join individual i on e.individual_id = i.individual_id
inner join study s on i.study_id = s.study_id
group by i.study_id
