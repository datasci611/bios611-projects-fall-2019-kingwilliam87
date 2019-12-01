FROM rocker/verse:latest
RUN Rscript -e "install.packages('tidyverse',dependencies=TRUE)"
RUN Rscript -e "install.packages('zoo',dependencies=TRUE)"
RUN Rscript -e "install.packages('ggplot2',dependencies=TRUE)"
RUN Rscript -e "install.packages('dplyr',dependencies=TRUE)"
RUN Rscript -e "install.packages('lubridate',dependencies=TRUE)"

FROM continuumio/anaconda3
RUN conda install -c conda-forge statsmodels
RUN pip install pandas==0.25.0 --user
RUN pip install matplotlib --user
RUN pip install datetime --user
RUN pip install numpy --user
