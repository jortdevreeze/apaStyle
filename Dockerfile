FROM rocker/rstudio:4.0.3

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libxml2 \
    git

COPY ./requirements.txt .
RUN cat requirements.txt | xargs install2.r --error \
    --deps TRUE