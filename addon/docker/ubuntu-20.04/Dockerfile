FROM ubuntu:20.04
MAINTAINER Morten Hammer "Morten.Hammer@ntnu.no"
ENV DEBIAN_FRONTEND=noninteractive

# Update apt packages
RUN apt update
RUN apt upgrade -y

# Install necessary packages
RUN apt install -y git
RUN apt install -y gfortran make liblapack-dev libstdc++-9-dev
RUN apt install -y python3 python3-numpy python3-matplotlib python3-distutils

# Make python3 the default python
RUN ln -s /usr/bin/python3 /usr/bin/python

# Clean storage area for state information of package resource
RUN rm -rf /var/lib/apt/lists/*

# Add thermo user
RUN groupadd -r thermo && useradd --no-log-init -mr -g thermo thermo
USER thermo
WORKDIR /home/thermo/

# Fetch thermopack
RUN git clone https://github.com/SINTEF/thermopack.git
WORKDIR /home/thermo/thermopack

# Build thermopack and its python interface
RUN make optim
RUN cd addon/pycThermopack; make optim

# Install the python interface
USER root
RUN cd addon/pycThermopack; python3 setup.py install

# Enter container as thermo user
USER thermo
