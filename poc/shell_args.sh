#PATH=$PATH:../..

eval "$(docopts -h - : "$@" <<EOF
Usage: shell_args.sh [options] <argv>...

Options:
      --help     Show help options.
      --version  Print program version.
----
breezy 0.1

EOF
)"

#echo ${argv[0]}
#echo ${argv[1]}

pd=${argv[0]}
out=${argv[1]}

echo $pd
echo $out
#----
#---- Set up variables
#----

#to=${argv[0]}
#pn=${to##*/} #project name