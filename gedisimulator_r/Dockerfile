# Dockerfile equivalent to singularity.txt
FROM fedora:latest

# Update package manager and install dependencies
RUN dnf install -y \
    libgeotiff \
    libgeotiff-devel \
    mercurial \
    git \
    make \
    gcc \
    gsl \
    gsl-devel \
    wget \
    gcc-c++ \
    unzip \
    tcsh \
    gdal \
    gdal-devel \
    geos-devel \
    proj-devel \
    tbb-devel \
    netcdf-devel \
    hdf5 \
    hdf5-devel \
    R \
    fontconfig-devel \
    freetype-devel \
    libxml2-devel \
    pandoc \
    udunits2-devel \
    openssl-devel \
    abseil-cpp-devel \
    libgit2-devel \
    fribidi-devel

# Set up environment variables
ENV MAKEFLAGS="-j3"
ENV ARCH=x86_64
ENV GEDIRAT_ROOT="/root/src/gedisimulator"
ENV HANCOCKTOOLS_ROOT="/root/src/tools"
ENV LIBCLIDAR_ROOT="/root/src/libclidar"
ENV CMPFIT_ROOT="/root/src/minpack"
ENV GSL_ROOT="/usr/local/lib"

# Create necessary directories
RUN mkdir -p /root/src /root/bin/x86_64 /root/bin/csh /root/renv

# Set working directory
WORKDIR /root/src

# Clean up any existing directories
RUN rm -rf libclidar tools voxelate

# Clone repositories
RUN git clone https://bitbucket.org/StevenHancock/libclidar.git && \
    git clone https://bitbucket.org/StevenHancock/tools.git
    
# Download and extract cmpfit
#RUN mkdir -p $CMPFIT_ROOT && \
#    cd $CMPFIT_ROOT && \
#    wget https://www.physics.wisc.edu/~craigm/idl/down/cmpfit-1.2.tar.gz && \
#    tar -xvf cmpfit-1.2.tar.gz && \
#    mv cmpfit-1.2/* $CMPFIT_ROOT/ && \
#    rm cmpfit-1.2.tar.gz

# Download and extract cmpfit via GitHub sparse checkout
# This replaces the broken wisc.edu link and the local COPY command
RUN git clone --filter=blob:none --no-checkout https://github.com/sthancock/gedisimulator.git $CMPFIT_ROOT && \
    cd $CMPFIT_ROOT && \
    git sparse-checkout init --cone && \
    git sparse-checkout set packages/cmpfit-1.2 && \
    git checkout && \
    mv packages/cmpfit-1.2/* . && \
    rm -rf packages .git

# Clone GEDI simulator and build tools
RUN git clone https://StevenHancock@bitbucket.org/StevenHancock/gedisimulator.git && \
    cd $GEDIRAT_ROOT && \
    make THIS=gediRat && \
    mv gediRat /usr/bin/ && \
    cp *.csh /usr/bin/ && \
    cp *.bash /usr/bin/ && \
    make THIS=gediMetric && \
    mv gediMetric /usr/bin/ && \
    make THIS=mapLidar && \
    mv mapLidar /usr/bin/ && \
    make THIS=collocateWaves && \
    mv collocateWaves /usr/bin/ && \
    make THIS=lasPoints && \
    mv lasPoints /usr/bin/ && \
    make THIS=addNoiseHDF && \
    mv addNoiseHDF /usr/bin/ && \
    make THIS=lgw2hdf && \
    mv lgw2hdf /usr/bin/ && \
    make THIS=fitTXpulse && \
    mv fitTXpulse /usr/bin/
    
# Copy renv files and restore R packages
WORKDIR /root/renv
COPY *.R ./
RUN mkdir -p /usr/share/doc/R/html && \
    Rscript install_packages.R

# Default command
CMD ["R"]
