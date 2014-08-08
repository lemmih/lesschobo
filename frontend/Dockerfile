# FROM phusion/passenger-nodejs
FROM golden/meteor-dev

ADD . /opt/application/
RUN mrt install

EXPOSE 3000

CMD meteor run



