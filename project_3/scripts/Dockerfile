FROM rocker/verse:latest
RUN conda install -c conda-forge statsmodels
RUN Rscript -e "install.packages('tidyverse')"
RUN Rscript -e "install.packages('zoo')"
RUN Rscript -e "install.packages('magrittr')"
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('tibble')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('lubridate')"

FROM continuumio/anaconda3
RUN pip install pandas==0.25.0 --user
RUN pip install matplotlib --user
RUN pip install datetime --user
RUN pip install numpy --user
RUN pip install os --user
RUN pip install sklearn --user
