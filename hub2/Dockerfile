FROM haskell:7.8
MAINTAINER Matt Jones <jonesmr@gmail.com>

RUN apt-get update
RUN apt-get install -y git libpcre3-dev mysql-client libmysqlclient-dev

RUN cabal update
RUN cabal --version

RUN git clone https://github.com/mrjones/weather.git /src/weather
WORKDIR /src/weather/hub2
RUN cabal sandbox init
RUN cabal install

ENTRYPOINT ["/src/weather/hub2/.cabal-sandbox/bin/hub"]
