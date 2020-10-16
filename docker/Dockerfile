FROM rocker/shiny-verse:4.0.2

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8
ENV LC_ALL en_US.UTF-8

RUN useradd -m workspace \
  && su workspace -c "mkdir -p /home/workspace/files/R/4.0.2; ln -s /home/workspace/files/R /home/workspace/R"

RUN set -ex \
    && deps='curl texlive-latex-extra' \
    && apt-get update \
    && apt-get install -y $deps \
    && apt-get install -y default-jre \
    && apt-get install -y default-jdk \
    && apt-get install -y liblzma-dev \
    && apt-get install -y libtiff5-dev \
    && apt-get install -y libbz2-dev \
    && apt-get install -y libcurl4-openssl-dev \
    && apt-get install -y libv8-dev \
    && apt-get install -y gdal-bin proj-bin libgdal-dev libproj-dev \
    && R CMD javareconf \
    && rm -rf /var/lib/apt/lists/*

## Essential packages
# xaputils and its dependencies
RUN R -e "install.packages( c('IRdisplay', 'dbplyr', 'knitr', 'PivotalR', 'RPostgreSQL'), quiet=TRUE, repos='https://cran.r-project.org' )"
RUN R -e "install.packages( c('tibble'), quiet=TRUE, repos='https://cran.r-project.org' )"
COPY xaputils_*.tar.gz xaputils.tar.gz
RUN R -e "install.packages('./xaputils.tar.gz', repos=NULL, type='source', quiet=FALSE)"

RUN R -e "install.packages(c('mockery', 'IRkernel'), quiet=TRUE)"
RUN R -e "install.packages( c('ggplot2', 'ggvis', 'hflights', 'BiocManager','rgdal'), quiet=TRUE, repos='https://cran.r-project.org' )" \
    && R -e "BiocManager::install('Rhtslib')"
# XAP-13326 dplyr back to version 0.5.0
RUN R -e "remotes::install_version('dplyr', version='0.5.0', upgrade='ask')"

# XAP-14120 packages pre-installed to help speed up ggquickeda Mini-App
RUN R -e "install.packages( c('shinyFiles', 'RPostgres'), quiet=TRUE, repos='https://cran.r-project.org' )"
RUN R -e "remotes::install_github('smouksassi/ggquickeda@aridhiadre')"

RUN rm /usr/local/lib/R/etc/Rprofile.site \
  && echo "options(repos = c(CRAN='https://cran.ma.imperial.ac.uk'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

ADD kernel /usr/bin/
ADD mini-app /usr/bin/
ADD --chown=workspace test /home/workspace/test
USER workspace
WORKDIR /home/workspace
ENTRYPOINT /usr/bin/kernel
