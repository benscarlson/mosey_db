wd=~/projects/movebankdb/analysis/movebankdb
src=~/projects/movebankdb/src

cd $wd

#---- load datasets ----#
pd=~/projects/movebankdb/analysis/movebankdb
out="/Volumes/WD4TB/projects/movebankdb/active"

$src/db/load_datasets.sh $pd $out

#---- Figures ----#

$src/figs/ind_date_range.r 10531951 figs/ind_date_range/Black_Stork_MPIAB_Latvia.pdf
  


