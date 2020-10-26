
--- Number of individuals, per study and species (a study can have individuals from more than one species)
select s.name, i.taxon_canonical_name, count(*) from study s
inner join individual i
on s.study_id = i.study_id
group by s.name, i.taxon_canonical_name
order by i.taxon_canonical_name

--- Number of individuals, per species
select i.taxon_canonical_name, count(*) from study s
inner join individual i
on s.study_id = i.study_id
group by i.taxon_canonical_name
order by i.taxon_canonical_name


select distinct taxon_canonical_name from individual