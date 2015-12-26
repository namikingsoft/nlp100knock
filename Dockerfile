FROM haskell:7.10

# update & install package
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 \
 && echo 'deb http://download.fpcomplete.com/debian jessie main' \
    | tee /etc/apt/sources.list.d/fpco.list \
 && apt-get update -y \
 && apt-get install -y stack vim curl \
 && rm -rf /var/lib/apt/lists/*

# deploy by stack
COPY nlp100knock.cabal /tmp/app/
COPY stack.yaml /tmp/app/
RUN cd /tmp/app \
 && mkdir src && mkdir test \
 && stack build --only-dependencies --test \
 && rm -rf /tmp/app
COPY . /app
RUN cd /app \
 && stack test \
 && echo ':set prompt "\ESC[32m> \ESC[m"' > /root/.ghci

# deploy by cabal
#RUN cd /tmp/app \
# && cabal update \
# && cabal install --only-dep --enable-test \
# && rm -rf /tmp/app
#COPY . /app
#RUN cd /app \
# && cabal test

# setting
WORKDIR /app
ENV PATH=/app/bin:/root/.local/bin:/root/.cabal/bin:$PATH

# entry point
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["idle"]
