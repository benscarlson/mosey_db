SELECT count(*), max(deploy_on_timestamp)
FROM `deployment`
WHERE (`individual_id` = 10157691.0) -- AND `deploy_on_timestamp` < '2010-08-09'

select * from deployment limit 3

--SELECT count(*), max(timestamp)
select *
FROM `event`
WHERE (`individual_id` = 6781206.0) limit 3 -- AND `timestamp` >= '2010-01-01'

select * from event limit 3

-- no no! some timestamps are numeric, others are characters
-- it would be better to store all as characters, for readability. but, will it be slower?
-- note here are valid formats: https://www.sqlite.org/lang_datefunc.html. e.g. this is ok: '2013-10-07T08:23:19.120Z'
-- seems use strftime to convert from numeric to character format