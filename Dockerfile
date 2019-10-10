FROM debian:unstable
#FROM debian/chicken

RUN apt-get -y update
#RUN apt-get -y dist-upgrade
RUN apt-get -y install chicken-bin git rlwrap gcc libssl-dev
#RUN apt-get -y autoremove
#RUN apt-get -y clean
RUN git clone https://github.com/TurtleKitty/Vaquero /home/vaquero
WORKDIR /home/vaquero
RUN sh ./bin/get_eggs.sh
RUN sh ./bin/compile.sh
CMD sleep 1; rlwrap ./vaquero repl

