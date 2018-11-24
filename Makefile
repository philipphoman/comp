#
# Makefile
#
# created on Wed Nov 21 2018
# Philipp Homan, <philipp dot homan at mssm dot edu>
#-----------------------------------------------------------------------

# default project name
PROJ = comp
#-----------------------------------------------------------------------

# Usually no edits below this line
#-----------------------------------------------------------------------
# Output directory
OUTP = output

EXT = ext

# Figures directory
FIGS = $(OUTP)/figures

# Tables directory
TABLES = $(OUTP)/tables

# R output directory
ROUT = $(OUTP)/R

# tex output directory
TEXOUT = $(SRC)

# Source directory
SRC = src

# Data directory
DATA = data

# tmp directory
TMP = $(OUTP)/tmp

# directory for additional pdf files
LIB = lib

# Filename for manuscript
MANUSCRIPT =$(SRC)/$(PROJ)_ms.pdf

# executables
RCC = R CMD BATCH
RM = rm -Rf
#TEX = xelatex -output-directory=../$(TEXOUT)
TEX = xelatex 
BIBTEX = bibtex
LATEXMK = latexmk -pdflatex='xelatex -interaction nonstopmode'  -shell-escape -pdf -bibtex -f 
PYTHONBIN = python
EMACSINIT = $(EXT)/$(PROJ)_dotemacs 
EMACS = emacs -l ../$(EMACSINIT)
EMACSMSARGS = --batch -f org-latex-export-to-latex --kill
EMACSPARGS =  --batch -f org-beamer-export-to-latex --kill
VIEWBIN = pdfview
PDFMERGEBIN = ext/pdfmerge
CPBIN = cp
MKDIRBIN = mkdir

# list R files
RFILES = $(SRC)/$(PROJ)_do.R \

# list data files
DATAFILES = $(DATA)/$(PROJ).csv \

# list tex files
TEXFILES = $(ORGFILES:$(SRC)/$(PROJ)_ms.org=$(SRC)/$(PROJ)_ms.tex)

# list org files
ORGFILES = $(wildcard $(SRC)/$(PROJ)*.org)

# list additional library files
PDFLIB = $(wildcard $(LIB)/$(PROJ)*.*)

# indicator files to show R file has been run
ROUTFILES = $(RFILES:$(SRC)/%.R=$(ROUT)/%.Rout) $(SRC)/ptsd_func.R \
$(SRC)/$(PROJ)_load.R

# indicator files to show tex has run
TEXOUTFILES = $(TEXFILES:$(SRC)/%.tex=$(SRC)/%.aux)

# replace tex with pdf to get pdf tex files
PDFTEXFILES = $(TEXOUTFILES:$(SRC)/%.aux=$(SRC)/%.pdf)

# R file dependencies
#$(ROUT)/%.Rout: $(SRC)/%.R $(DATAFILES) \
#                $(SRC)/$(PROJ)_load.R $(SRC)/$(PROJ)_func.R
#	cd $(SRC) && $(RCC) $(notdir $<) ../$(ROUT)/$*.Rout 
$(ROUT)/%.Rout: $(SRC)/%.R $(DATAFILES) \
                $(SRC)/$(PROJ)_load.R 
  #cd $(SRC) && $(RCC) $(notdir $<) ../$(ROUT)/$*.Rout 
	echo "Running $(notdir $<), this may take a while ..." \
	&& cd $(SRC) && $(RCC) $(notdir $<) 


# Rule for $(TEXFILES)
# Convert every org file to LaTeX this is done from within the subfolder
# so be careful with relative paths
$(SRC)/%.tex: $(SRC)/%.org $(ROUTFILES) $(PDFLIB)
	@if [ "$(notdir $<)" = "$(PROJ)_ms.org" ]; then \
		echo "Exporting manuscript from org to LaTeX" \
		&& cd $(SRC) && $(EMACS) $(PROJ)_ms.org $(EMACSMSARGS); \
	else \
		echo "Exporting $(notdir $<) from org to LaTeX" \
		&& cd $(SRC) && $(EMACS) $(notdir $<) $(EMACSPARGS); \
	fi

# Rule for $(TEXOUTFILES)
# Run every tex file this is done from within the subfolder so be
# careful with relative paths
$(SRC)/%.aux: $(SRC)/%.tex $(ROUTFILES) $(PDFLIB)
	#cd $(SRC) && $(TEX) $(notdir $<)
	#$(BIBTEX) $(SRC)/$*
	#cd $(SRC) && $(TEX) $(notdir $<)
	#cd $(SRC) && $(TEX) $(notdir $<) 
	cd $(SRC) && $(LATEXMK) $(notdir $<)

# Default entry
manuscript: figures tex

# make figures
figures: analysis 

# run tex files
tex: analysis $(TEXOUTFILES) $(TEXFILES) 

analysis: $(ROUTFILES)

texclean: 
	$(RM) $(TEXOUT)/$(PROJ)*.tex
	$(RM) $(TEXOUT)/$(PROJ)*.aux

Rclean: 
	$(RM) $(ROUT)/$(PROJ)*.*

tmpclean:
	$(RM) $(TMP)/*.*

test:
	echo $(PROJ)
