FROM pobsteta/r-full

MAINTAINER Pascal Obstétar "pascal.obstetar@gmail.com"

# library for latex
RUN apt-get update -y &&\
	apt-get install -y texlive texlive-latex-extra texlive-latex3 texlive-fonts-extra texlive-lang-french

# library for gftools
RUN R -e "devtools::install_github('pobsteta/gftools')"

# copy the app to the image
RUN mkdir /root/spgftools
COPY spgftools /root/spgftools
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/spgftools')"]
