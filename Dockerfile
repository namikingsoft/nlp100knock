FROM haskell:7.10

# update & install package
RUN apt-get update -y \
 && apt-get install -y vim \
 && rm -rf /var/lib/apt/lists/*

# environment
ENV PATH=/app/bin:/root/.cabal/bin:$PATH

# deploy
COPY nlp100knock.cabal /tmp/app/
RUN cd /tmp/app \
 && cabal update \
 && cabal install --only-dep --enable-test \
 && rm -rf /tmp/app
COPY . /app
RUN cd /app \
 && cabal test

# workdir
WORKDIR /app
VOLUME /app

# entry point
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["idle"]
