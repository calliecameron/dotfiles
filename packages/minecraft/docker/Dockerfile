FROM debian:bookworm

RUN apt-get update && \
    apt-get install -y \
    ca-certificates-java && \
    apt-get install -y \
    curl \
    firefox-esr \
    less \
    libpulse0 \
    locales \
    lsb-release \
    mesa-utils \
    nano \
    openjdk-17-jre \
    xdg-utils && \
    rm -rf /var/lib/apt/lists/*

RUN sed -i '/en_GB.UTF-8/s/^# //g' /etc/locale.gen && locale-gen

RUN curl -o 'atlauncher-1.4.1.deb' -q 'https://download.nodecdn.net/containers/atl/app/dist/linux/atlauncher-1.4-1.deb' && \
    apt-get update && \
    apt install -y './atlauncher-1.4.1.deb' && \
    rm -rf /var/lib/apt/lists/* && \
    rm 'atlauncher-1.4.1.deb'

COPY config_diff.sh /config_diff.sh

ARG UID
ARG GID
RUN mkdir /minecraft && mkdir /minecraft/atlauncher && chown -R "$UID:$GID" /minecraft
WORKDIR /minecraft

CMD atlauncher --working-dir /minecraft/atlauncher
