.PHONY: project_3
project_3: 
	docker build -t project_3 .
	python scripts/proj3final.ipynb
	Rscript scripts/proj3_Rscript.R
	Rscript -e "library(rmarkdown); rmarkdown::render('scripts/proj3_final.Rmd', 'html_document')"  && mv proj3_final.html results
