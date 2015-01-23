FROM ubuntu:14.04
MAINTAINER Matt Jones <jonesmr@gmail.com>

RUN apt-get update && apt-get install -y \
    libffi6 \
    libmysqlclient-dev \
    libpcre3-dev

RUN mkdir -p /deploy/bin && mkdir -p /deploy/static
ADD .cabal-sandbox/bin/hub /deploy/bin/hub
ADD static /deploy/static/

EXPOSE 5999

ENTRYPOINT ["/deploy/bin/hub", "--port", "5999", "--staticdir", "/deploy/static"]
