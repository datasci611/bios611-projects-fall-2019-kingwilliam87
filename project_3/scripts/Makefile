.PHONY: project_3
project_3:
	docker image build -t project_3 .
	python scripts/project3final.ipynb
	Rscript -e "library(rmarkdown); rmarkdown::render('project3final.Rmd', 'html_document')"
