# FROM phusion/passenger-nodejs
FROM golden/meteor-dev

ADD smart.json /tmp/app/
WORKDIR /tmp/app/
RUN mrt install

ADD . /tmp/app/
RUN mrt install

RUN meteor bundle --directory /opt/application/

WORKDIR /opt/application/

EXPOSE 3000

CMD ROOT_URL=http://talentsrs.com/ \
    PORT=3000 \
    MONGO_URL=mongodb://$MONGO_PORT_27017_TCP_ADDR/lesschobo \
    METEOR_SETTINGS=$(cat /tmp/app/settings.json) \
    node main.js

