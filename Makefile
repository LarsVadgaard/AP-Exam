zip : clean
	mkdir -p release
	make -C report/
	cp report/trash/report.pdf release
	zip handin.zip -r handin -x *.beam *.eqc* *.git* *appm.cabal *.hdev*
	mv handin.zip release

clean :
	rm -rf release
	rm -rf handin/appm/.stack-work
