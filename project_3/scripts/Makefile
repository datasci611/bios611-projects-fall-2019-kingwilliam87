.PHONY: project_3
project_3: 
	docker build -t project_3 .
	python proj3final.ipynb
	R proj3_Rscript.R
	Rscript -e "library(rmarkdown); rmarkdown::render('proj3_final.Rmd', 'html_document')"  && mv proj3_final.html ../results
