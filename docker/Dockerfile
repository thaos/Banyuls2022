# https://stackoverflow.com/questions/54437030/how-can-i-create-a-docker-image-to-run-both-python-and-r
# https://stackoverflow.com/questions/27701930/how-to-add-users-to-docker-container
# https://stackoverflow.com/questions/24991136/docker-build-could-not-resolve-archive-ubuntu-com-apt-get-fails-to-install-a
# https://stackoverflow.com/questions/25845538/how-to-use-sudo-inside-a-docker-container


FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends build-essential r-base python3.6 python3-pip python3-setuptools python3-dev python-is-python2 libpng-dev libgdal-dev libnetcdf-dev netcdf-bin git wget myproxy ssh vim gdebi-core  sudo


RUN wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-2022.02.2-485-amd64.deb
RUN gdebi --non-interactive rstudio-server-2022.02.2-485-amd64.deb

RUN useradd -rm -d /home/sthao -s /bin/bash -g root -G sudo -u 1001 sthao
RUN echo 'sthao:0000' | chpasswd

USER sthao



WORKDIR /home/sthao

# COPY requirements.txt /app/requirements.txt

# RUN pip3 install -r requirements.txt


RUN mkdir /home/sthao/R
RUN echo ".libPaths('/home/sthao/R')" > /home/sthao/.Rprofile
RUN Rscript -e "install.packages( c('magrittr', 'reticulate', 'rgdal', 'ncdf4', 'ncdf4.helpers', 'xml2', 'sp', 'raster', 'rgeos'), lib = '/home/sthao/R')"
RUN Rscript -e "reticulate::install_miniconda()"
RUN Rscript -e "reticulate::py_install('cdsapi')"

RUN git clone https://github.com/thaos/Banyuls2022.git
RUN pip3 install cdsapi

COPY .dodsrc .dodsrc


