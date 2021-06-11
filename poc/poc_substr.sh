STR='GNU/Linux is an operating system'
SUB='Linux'
if [[ "$STR" == *"$SUB"* ]]; then
  echo "It's there."
fi

process='dciv'
sub='d'
if [[ "$process" == *"$sub"*]]; then
  echo "found it"
fi

[[ ! -z "$db" ]]

if [ 0 -eq 0 ]; then
    echo "Successfully imported data"
fi

str=a
#Make sure that brackets don't touch the two operands, or command will fail!
if ["$str" = a]; then echo "success"; fi
if [ "$str" = a ]; then echo "success"; fi #works
if [[ "$str" = a ]]; then echo "success"; fi #double brackets is preferred

#This works!
#process='dciv'
process='civ'
sub='d'
if [[ "$process" = *"$sub"* ]]; then
  echo "found it"
fi

process='dciv'
#sub='d'
if [[ "$process" = *d* ]]; then
  echo "found it"
fi

#We put quotes around a variable in case the variable is empty, then we'll get the empty string
