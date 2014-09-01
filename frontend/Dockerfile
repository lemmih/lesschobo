# FROM phusion/passenger-nodejs
FROM golden/meteor-dev

# RUN apt-get update && apt-get install -y phantomjs

ADD .meteor/packages /tmp/app/.meteor/packages
ADD .meteor/release /tmp/app/.meteor/release
WORKDIR /tmp/app/
# This is a hack to force meteor to install the dependencies.
RUN meteor bundle tmp.tgz

ADD . /tmp/app/

RUN meteor bundle --directory /opt/application/

WORKDIR /opt/application/

EXPOSE 3000

CMD ROOT_URL=http://talentsrs.com/ \
    PORT=3000 \
    MONGO_URL=mongodb://$MONGO_PORT_27017_TCP_ADDR/lesschobo \
    METEOR_SETTINGS=$(cat /tmp/app/settings.json) \
    node main.js

