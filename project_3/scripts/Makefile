.PHONY: project_3
project_3: 
	docker build -t project_3 .
	python project3final.ipynb
	Rscript -e "library(rmarkdown); rmarkdown::render('project3_final.Rmd', 'html_document')"
