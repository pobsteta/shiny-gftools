FROM pobsteta/r-base

MAINTAINER Pascal Obstétar "pascal.obstetar@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0   
    
# On ajoute le dépôt QGIS unstable
#RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable
RUN echo "deb http://ppa.launchpad.net/ubuntugis/ubuntugis-unstable/ubuntu xenial main" > /etc/apt/sources.list.d/qgis.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key 6B827C12C2D425E227EDCA75089EBE08314DF160

# system library dependency for the app
RUN apt-get update && apt-get install -y --allow-unauthenticated \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'shinythemes', 'shinyjs', 'leaflet', 'leaflet.extras', 'DT'), repos='https://cloud.r-project.org/')"

# library for gftools
RUN R -e "install.packages(c('ggplot2', 'dplyr', 'data.table', 'raster', 'sp', 'devtools', 'tidyr', 'readr', 'tibble', 'reshape2'), repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('hadley/tidyverse')"
RUN R -e "devtools::install_github('r-spatial/sf')"
RUN R -e "devtools::install_github('pobsteta/gftools')"

# copy the app to the image
RUN mkdir /root/shinyproxy-gftools
COPY shinyproxy-gftools /root/shinyproxy-gftools

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/shinyproxy-gftools')"]
