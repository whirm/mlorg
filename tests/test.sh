#! /bin/sh
# Small testing framework

FILE=parser/syntax
MLORG=../_build/main.native

$MLORG --backend xml $FILE.org -o $FILE.xml
$MLORG --backend org $FILE.org -o $FILE-2.org
$MLORG --backend xml $FILE-2.org -o $FILE-2.xml
sed -i $FILE-2.xml -e 's,syntax-2.org,syntax.org,g'
diff $FILE.xml $FILE-2.xml | tee $FILE.diff | less
