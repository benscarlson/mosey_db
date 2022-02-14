-- TODO: not sure if I need this table
-- TODO: this should be moved out to the project that uses it (mosey_view)

CREATE TABLE IF NOT EXISTS `event_indiv_stats` (
	`individual_id`	INTEGER,
	`ts_min` TEXT,
	`ts_max` TEXT,
	`lon_min` REAL,
	`lon_max` REAL,
	`lon_mean` REAL,
	`lat_min` REAL,
	`lat_max` REAL,
	`lat_mean` REAL,
	FOREIGN KEY(individual_id) REFERENCES individual(individual_id)
);