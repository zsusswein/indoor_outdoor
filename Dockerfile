FROM rocker/tidyverse:4
COPY data/ /home/rstudio/data/
COPY code/ /home/rstudio/code/
COPY dependencies.R dependencies.R
RUN Rscript dependencies.R
WORKDIR /home/rstudio
RUN Rscript code/generate_modularity_class_fits.R
RUN Rscript code/generate_figure_1.R
RUN Rscript code/generate_supp_figs.R
