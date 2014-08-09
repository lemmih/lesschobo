# FROM phusion/passenger-nodejs
FROM golden/meteor-dev

ADD . /tmp/app/

WORKDIR /tmp/app/
RUN mrt install
RUN meteor bundle app.tar.gz
RUN tar -xzvf app.tar.gz
RUN mkdir /opt/application/
RUN ls /opt/application/
RUN cp -r bundle /opt/application/

WORKDIR /opt/application/

EXPOSE 3000

CMD PORT=3000 MONGO_URL=mongodb://$MONGO_PORT_27017_TCP_ADDR/lesschobo node bundle/main.js

