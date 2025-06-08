#!/bin/sh

docker build -t smalltalk-game .
docker run -it --rm -v "$(pwd)":/app smalltalk-game
