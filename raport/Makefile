.SHELL := /bin/bash

.PHONY: compile clean delete clean_all

#FILENAME = MGO
FILENAME = $(shell ls | grep *.tex | cut -d'.' -f 1)

all: compile

compile:
	@echo "Compiling LaTeX file..."
	pdflatex $(FILENAME)
	biber $(FILENAME) 
	pdflatex $(FILENAME)
	pdflatex $(FILENAME)

clean:
	@echo "Cleaning log files..."
	@rm -f *.aux *.log *.bbl *.blg *.bcf *.run.xml *.out *.fdb_latexmk *.synctex.gz *.fls *.toc *.lof *.lot

delete:
	@echo "Deleting pdf file."
	@rm -f *.pdf

clean_all:
	@echo "Cleaning pdf and log files..."
	@rm -f *.aux *.log *.bbl *.blg *.bcf *.run.xml *.out *.fdb_latexmk *.synctex.gz *.fls *.pdf *.toc *.lof *.lot
	
