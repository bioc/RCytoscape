build:
	(cd ../..; R CMD build --no-vignettes RCytoscape)
	(cd ../..; R CMD install RCytoscape)

test:
	R --vanilla < test.R > /dev/null

testverbose:
	R --vanilla < test.R


check:
	(cd ../..; R CMD check --no-latex RCytoscape)
