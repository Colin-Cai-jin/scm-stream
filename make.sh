#!/bin/sh
funcs=`sed -nr '/^\(define/s/^\(define[^ \t]*[ \t]+(\([ \t]*)?([^ \t]+).*/\2/p' stream.scm`
echo Select your Scheme:
echo '1) Chez Scheme' 
echo '2) Racket' 
echo 'Type 1/2'
read ch
mkdir test
echo make directory ./test
if [ $ch -eq 1 ]; then
	echo make test/stream.ss
	echo '(library (stream)' >test/stream.ss
	echo '(export ' $funcs ')' >>test/stream.ss
	echo '(import (scheme))' >>test/stream.ss
	cat stream.scm >>test/stream.ss
	echo ')' >>test/stream.ss
	for i in examples/*.scm; do
		F=`echo $i|sed 's/examples\(\/.*\.\)scm$/test\1ss/'`
		echo make $F
		echo '(import (scheme) (stream))' >$F
		cat $i >>$F
	done
elif [ $ch -eq 2 ]; then
	echo make test/stream.rkt
	echo '#lang scheme' >test/stream.rkt
	echo '(provide' $funcs ')' >>test/stream.rkt
	cat stream.scm >>test/stream.rkt
	for i in examples/*.scm; do
		F=`echo $i|sed 's/examples\(\/.*\.\)scm$/test\1rkt/'`
		echo make $F
		echo '#lang scheme' >$F
		echo '(require "stream.rkt")' >>$F
		cat $i >>$F
	done
fi
