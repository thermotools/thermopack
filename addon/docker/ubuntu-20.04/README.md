# ubuntu-thermopack based on ubuntu-20.04 docker container

On a system with docker installed, build ubuntu-thermopack:

```bash
# Build ubuntu-thermopack image in current folder
docker build -t ubuntu-thermopack .
```

```bash
# Run the container in bash terminal (No GUI enabled)
docker run -ti ubuntu-thermopack
```

# Running the container on Linux enabeling visualization of matplotlib plots

See ex. [https://www.geeksforgeeks.org/running-gui-applications-on-docker-in-linux/](https://www.geeksforgeeks.org/running-gui-applications-on-docker-in-linux/) for a more detailed guide on how to get the X server display working with the docker container.


```bash
# Get the cookie to connect X Server Displays
xauth list
# Get sequence number form DISPLAY
echo $DISPLAY
# Run ubuntu-thermopack container
docker run -ti --net=host -e DISPLAY -v /tmp/.X11-unix ubuntu-thermopack
# Add X Server Display cookie in ubuntu-thermopack
xauth add <xauth list and DISPLAY output>
```

# Running the container on Windows enabeling visualization of matplotlib plots

See ex. [https://dev.to/darksmile92/run-gui-app-in-linux-docker-container-on-windows-host-4kde](https://dev.to/darksmile92/run-gui-app-in-linux-docker-container-on-windows-host-4kde) for setting up an X server on Windows, and connecting to the server from the docker container. After starting the X server run the container:

```bash
# Get IP
ipconfig
# Set DISPLAY variable
set-variable -name DISPLAY -value YOUR-IP:0.0
# Run ubuntu-thermopack
docker run -ti --rm --net=host -e DISPLAY=$DISPLAY ubuntu-thermopack
```

# Running the container on MacOS enabeling visualization of matplotlib plots

See ex. [https://kartoza.com/en/blog/how-to-run-a-linux-gui-application-on-osx-using-docker/](https://kartoza.com/en/blog/how-to-run-a-linux-gui-application-on-osx-using-docker/) for setting up an X server on MacOS, and connecting to the server from the docker container.
