FROM torus/violet:latest

RUN apt-get update
RUN apt-get install -y libsqlite3-dev

CMD make build && make run
