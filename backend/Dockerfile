FROM zsol/haskell-platform-2013.2.0.0
# FROM darinmorrison/haskell

MAINTAINER lemmih@gmail.com

# RUN apt install zlib1g-dev

EXPOSE 3000

COPY . /home/haskell/src/
RUN sudo chown --recursive haskell:haskell src/

RUN cabal update
RUN cd src/ && cabal install --jobs=1

CMD /home/haskell/.cabal/bin/lesschobo
