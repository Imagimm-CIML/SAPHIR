FROM rocker/shiny:latest
RUN apt-get update \
    && apt-get install -y\
    libcurl4-openssl-dev\
    libnode-dev\
    libpoppler-cpp-dev\
    libfftw3-dev\
    && mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install library
RUN R -e "install.packages(c(\
    'shiny',\
    'ggplot2',\
    'stringr',\
    'shinydashboard',\
    'shinyFiles',\
    'shinycssloaders',\
    'ijtiff',\
    'RImageJROI',\
    'plotly',\
    'BiocManager',\
    'shinyjs',\
    'V8',\
    'Rcpp',\
    'pillar',\
    'readtext',\
    'magick',\
    'png',\
    'shinyWidgets'))"

RUN R -e "BiocManager::install('EBImage')"

RUN mkdir /srv/shiny-server/saphir
# copy the app to the image
COPY app.R /srv/shiny-server/saphir/
# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 /srv/shiny-server/

EXPOSE 3838
