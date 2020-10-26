--- tag
select t.* from tag t
inner join deployment d on t.tag_id = d.tag_id
inner join individual i on d.individual_id = i.individual_id 
where i.study_id = 24442409

--- sensor
select s.* from sensor s
inner join tag t on s.tag_id = t.tag_id
inner join deployment d on t.tag_id = d.tag_id
inner join individual i on d.individual_id = i.individual_id 
where i.study_id = 24442409

--- deployment
select d.* from deployment d 
inner join individual i on d.individual_id = i.individual_id 
where i.study_id = 24442409

--- study
select * from study where study_id = 24442409

--- individual
select * from individual i where i.study_id = 24442409

--- event
select e.* from event e 
inner join individual i on e.individual_id = i.individual_id 
where i.study_id = 24442409