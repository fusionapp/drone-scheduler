FROM debian:stretch
COPY ["dist/drone-scheduler", "/usr/local/bin/"]
ENTRYPOINT ["/usr/local/bin/drone-scheduler"]
