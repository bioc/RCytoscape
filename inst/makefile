
help:
	egrep "^#" makefile | sed "s/^#//"

# --- build
# build and install
#

build:
	(cd ../..; R CMD build --no-vignettes RCytoscape)
	(cd ../..; R CMD install RCytoscape)

test:
	R --vanilla < test.R > /dev/null

testverbose:
	R --vanilla < test.R

# --- check
# check, but ignore man pages and vignette
#

check:
	(cd ../..; R CMD check --no-manual --no-vignettes RCytoscape)

# --- checkfull
# check everything
#

checkfull:
	(cd ../..; R --vanilla CMD check RCytoscape)


# --- codoc
# check just the man pages
#

codoc:
	(cd ../..; R CMD check --no-manual --no-vignettes --no-tests --no-examples RCytoscape)


# --- sweave
# creates RCytoscape.tex, runs all embedded examples
# run this before the pdf target
#

sweave: 
	(cd doc; R --no-save < ../sweave.R)

# --- pdf
# make and open RCytoscape.pdf, the vignette 
#

pdf:  doc/RCytoscape.tex
	(cd doc; rm *.pdf; /usr/texbin/latex --output-format=pdf  RCytoscape.tex; open RCytoscape.pdf)

# --- vanillaTest
# run all the unit tests, in a clean context
#
vanillaTest:
	R --vanilla < vanillaTest.R > vanillaTest.out 2>&1
