ARG CACHEBUST=1
FROM rocker/r-ver:4.3.1

# Install system dependencies needed by stringi/stringr, magick, XML, etc.
RUN apt-get update && apt-get install -y \
    libicu-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libmagick++-dev \
    libyaml-dev \
  && rm -rf /var/lib/apt/lists/*

# Install R packages (including tidyverse and all its dependencies) in one line
RUN R -e "install.packages( \
    c('dotenv','dplyr','xgboost','httr','Matrix','purrr','rvest','stringdist',\
'stringr','tidyr','yaml','magick','png','tibble','caret','tidyverse','digest',\
'jsonlite','xml2','here','renv'), \
    repos='https://cloud.r-project.org', dependencies=TRUE \
  )"

# Install GitHub CLI
RUN apt-get update && apt-get install -y \
      curl \
      gnupg \
      apt-transport-https \
  && curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
     | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg \
  && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] \
      https://cli.github.com/packages stable main" \
      > /etc/apt/sources.list.d/github-cli.list \
  && apt-get update && apt-get install -y gh \
  && rm -rf /var/lib/apt/lists/* 
