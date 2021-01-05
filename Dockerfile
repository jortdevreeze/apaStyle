FROM rocker/rstudio:4.0.3

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libxml2 \
    git

RUN install2.r devtools

COPY ./requirements.txt .
COPY ./install-package.r .

RUN Rscript install-package.r requirements.txt