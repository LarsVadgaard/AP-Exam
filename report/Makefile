main    := report
outfile := report
outdir  := trash

compile := pdflatex --output-directory=$(outdir) \
					 -shell-escape -file-line-error \
					 -jobname $(outfile) $(main)

default: compile
compile:
	@mkdir -p $(outdir)
	$(compile)
	$(compile)
clean:
	@rm -rf $(outdir)

.PHONY: default compile clean
