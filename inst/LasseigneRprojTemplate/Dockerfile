{\rtf1\ansi\ansicpg1252\cocoartf2708
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;\f1\fmodern\fcharset0 Courier;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;}
\margl1440\margr1440\vieww18680\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 FROM bioconductor/bioconductor_docker:RELEASE_3_14\
\
# location for mounting\
#RUN mkdir /home/rstudio/data\
\
\
##----------------------------------------------------------------------------##\
##  R PACKAGES\
##----------------------------------------------------------------------------##\
\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 #### General CRAN Packages #### \
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 RUN R -e 'install.packages(c("tidyverse", "remotes",  "BiocManager", "devtools", \'93
\f1\fs26 \expnd0\expndtw0\kerning0
\outl0\strokewidth0 \strokec2 knitr\'94, \'93lintr\'94, \'93styler\'94, \'93here\'94
\f0\fs24 \kerning1\expnd0\expndtw0 \outl0\strokewidth0 ))\'92 \
\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 #### Project Specific CRAN Packages #### \
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 # RUN R -e 'install.packages(c(\'93# insert CRAN Packages here #\'94))\'92 \
\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 #### Project Specific GitHub Packages #### \
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 # RUN R -e 'devtools::install_github(\'93# insert GitHub repos here #\'94)\'92 \
\
# RUN R -e 'remotes::install_github("# insert GitHub repos here #\'94)'\
\
#### Project Specific Bioconductor Packages ####\
\
# RUN R -e 'BiocManager::install(c("# insert Bioconductor Packages here #\'94"))'\
\
##----------------------------------------------------------------------------##\
## SYSTEM PACKAGES\
##----------------------------------------------------------------------------##\
\
RUN apt-get update --yes && \\\
  apt-get upgrade --yes\
\
RUN apt-get install --yes \\\
  python-dev \\\
  python3-pip \\\
  python3-dev \\\
  python3-pip \\\
  python3-tk\
\
\
RUN apt-get install --yes \\\
  libcurl4-openssl-dev \\\
  libssl-dev \\\
  libcairo2-dev libxt-dev \\\
  libxml2-dev \\\
  libudunits2-dev \\\
  libhdf5-dev \\\
  libv8-dev \\\
  libgdal-dev \\\
  xorg libx11-dev libglu1-mesa-dev \\\
  samtools libboost-all-dev \\\
  libgsl-dev\
\
##----------------------------------------------------------------------------##\
## PYTHON PACKAGES\
##----------------------------------------------------------------------------##\
\
\
# RUN pip3 install #pythonPackageHere\
\
\
\
}