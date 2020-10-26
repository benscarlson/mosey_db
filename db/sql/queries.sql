-- number of locations per study
select s.name, count(*) 
from event e
inner join individual i on e.individual_id = i.individual_id
inner join study s on i.study_id = s.study_id
group by s.name



---
--- counts per study
--- 

--- deployment
select count(*) from deployment d 
inner join individual i on d.individual_id = i.individual_id 
inner join study s on i.study_id = s.study_id
where s.study_id = 9493881

---- event
select count(*) from event e 
inner join individual i on e.individual_id = i.individual_id 
inner join study s on i.study_id = s.study_id
where s.study_id = 9493881

---- individual
select count(*) from individual i where i.study_id = 9493881

---- sensor
-- need to do distinct tag_id because same tag can be on multiple deployments
select count(distinct sen.sensor_id) from study s
inner join individual i on s.study_id = i.study_id
inner join deployment d on i.individual_id = d.individual_id
inner join tag t on d.tag_id = t.tag_id
inner join sensor sen on t.tag_id = sen.tag_id
where i.study_id = 9493881

---- tag
select count(distinct t.tag_id) from study s
inner join individual i on s.study_id = i.study_id
inner join deployment d on i.individual_id = d.individual_id
inner join tag t on d.tag_id = t.tag_id
where i.study_id = 9493881