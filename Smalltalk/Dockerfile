FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y gnu-smalltalk && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY *.st ./

CMD ["gst", "item.st", "location.st", "player.st", "main.st", "locations.st"]
