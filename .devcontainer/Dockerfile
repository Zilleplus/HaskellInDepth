FROM ubuntu:lunar

WORKDIR /opt/app

RUN apt-get update
RUN apt-get install -y vim fd-find wget git curl

RUN apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh 
RUN echo 'export PATH=$PATH:/root/.ghcup/bin' >> /root/.bashrc 

# required by chapter 3
RUN apt install zlib1g-dev 
# add ghcup to the path
ENV PATH=$PATH:/root/.ghcup/bin

# add gen-hie to the path
RUN cabal install implicit-hie
ENV PATH=$PATH:/root/.cabal/bin