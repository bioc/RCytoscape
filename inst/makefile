build:
	(cd ../..; R CMD build --no-vignettes RCytoscape)
	(cd ../..; R CMD install RCytoscape)

test:
	R --vanilla < test.R > /dev/null

testverbose:
	R --vanilla < test.R


check:
	(cd ../..; R CMD check --no-latex --no-vignettes RCytoscape)

checkfull:
	(cd ../..; R CMD check RCytoscape)

codoc:
	(cd ../..; R CMD check --no-latex --no-vignettes --no-tests --no-examples RCytoscape)


sweave: 
	(cd doc; R --no-save < ../sweave.R)

pdf:  doc/RCytoscape.tex
	(cd doc; rm *.pdf; /usr/texbin/latex --output-format=pdf  RCytoscape.tex; open RCytoscape.pdf)

