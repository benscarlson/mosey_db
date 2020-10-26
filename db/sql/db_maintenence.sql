
--- All below changes have been reflected in initialization/load scripts
--- If rebuilding/reloading the database, no need to repeat these actions.

begin transaction;
alter table event add column fix_type INTEGER;
commit;