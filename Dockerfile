FROM rocker/r-ver:4.3.1
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libmagick++-dev \
    libyaml-dev \
  && rm -rf /var/lib/apt/lists/*
RUN R -e "install.packages(c('dotenv', 'dplyr', 'fastLink', 'httr', 'Matrix',\
    'purrr', 'rvest', 'stringdist', 'stringr', 'tidyr', 'yaml', 'magick',\
    'png', 'tibble', 'caret', 'tidyverse', 'digest', 'jsonlite', 'xml2',\
    'here', 'renv'), repos='https://cloud.r-project.org')"
