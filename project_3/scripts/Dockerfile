FROM rocker/verse:3.5.1
FROM continuumio/anaconda3
RUN conda install -c conda-forge statsmodels
RUN Rscript -e "install.packages('tidyverse')"
RUN Rscript -e "install.packages('zoo')"
RUN Rscript -e "install.packages('magrittr')"
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('tibble')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('lubridate')"
