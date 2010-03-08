#!/bin/sh

# Build timber script
SCRIPT="$2/bin/timberc"
DATADIR="$2/share/timberc-1.0.3"

START="#!/bin/sh\\n\\nexec timberc \${1+\"\$@\"} --datadir $DATADIR \\n"
#printf $START d
#echo $START
/bin/echo "#!/bin/sh" > $SCRIPT
/bin/echo " " >> $SCRIPT
/bin/echo "exec $DATADIR/timberc \${1+\"\$@\"} --datadir $DATADIR" >> $SCRIPT
