FROM ubuntu:24.04 AS glfw

RUN apt-get update && \
    apt-get -y install \
    build-essential \
    cmake \
    unzip \
    wget \
    xorg-dev && \
    rm -rf /var/lib/apt/lists/*

RUN wget -O 'glfw-3.4.zip' 'https://github.com/glfw/glfw/releases/download/3.4/glfw-3.4.zip' && \
    unzip 'glfw-3.4.zip' && \
    cmake -S 'glfw-3.4' -B 'glfw-3.4/build' -D BUILD_SHARED_LIBS=ON -D GLFW_BUILD_WAYLAND=OFF -D GLFW_BUILD_EXAMPLES=OFF -D GLFW_BUILD_TESTS=OFF -D GLFW_BUILD_DOCS=OFF && \
    cmake --build 'glfw-3.4/build' && \
    mkdir /glfw && \
    cp glfw-3.4/build/src/libglfw* /glfw && \
    rm -rf 'glfw-3.4'

FROM ubuntu:24.04

RUN apt-get update && \
    apt-get -y upgrade && \
    apt-get -y dist-upgrade && \
    apt-get -y install \
    ca-certificates-java \
    wget && \
    wget -O /etc/apt/keyrings/packages.mozilla.org.asc https://packages.mozilla.org/apt/repo-signing-key.gpg && \
    echo 'deb [signed-by=/etc/apt/keyrings/packages.mozilla.org.asc] https://packages.mozilla.org/apt mozilla main' > /etc/apt/sources.list.d/mozilla.list && \
    printf 'Package: *\nPin: origin packages.mozilla.org\nPin-Priority: 1000\n' > /etc/apt/preferences.d/mozilla && \
    apt-get update && \
    apt-get -y install \
    curl \
    firefox \
    htop \
    less \
    libflite1 \
    libmtdev1t64 \
    libpulse0 \
    locales \
    lsb-release \
    mesa-utils \
    nano \
    openjdk-17-jdk \
    socat \
    unzip \
    xclip \
    xdg-utils \
    xfonts-base \
    xsel && \
    rm -rf /var/lib/apt/lists/* && \
    fc-cache

RUN sed -i '/en_GB.UTF-8/s/^# //g' /etc/locale.gen && locale-gen

RUN wget -O 'atlauncher-1.4.1.deb' 'https://download.nodecdn.net/containers/atl/app/dist/linux/atlauncher-1.4-1.deb' && \
    apt-get update && \
    apt-get -y --fix-broken install './atlauncher-1.4.1.deb' && \
    rm -rf /var/lib/apt/lists/* && \
    rm 'atlauncher-1.4.1.deb'

RUN mkdir -p /opt/minecraft
COPY --from=glfw /glfw /opt/minecraft/glfw
COPY minecraft-config-diff /usr/bin/minecraft-config-diff
RUN chmod ugo=rx /usr/bin/minecraft-config-diff
COPY minecraft-dynmap-diff /usr/bin/minecraft-dynmap-diff
RUN chmod ugo=rx /usr/bin/minecraft-dynmap-diff

WORKDIR /minecraft

CMD atlauncher --working-dir /minecraft/atlauncher
