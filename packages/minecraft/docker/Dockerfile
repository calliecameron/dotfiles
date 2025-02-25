FROM ubuntu:24.04 AS glfw

RUN apt-get update && \
    apt-get install -y \
    build-essential \
    cmake \
    unzip \
    wget \
    xorg-dev && \
    rm -rf /var/lib/apt/lists/*

RUN wget -q -O 'glfw-3.4.zip' 'https://github.com/glfw/glfw/releases/download/3.4/glfw-3.4.zip' && \
    unzip 'glfw-3.4.zip' && \
    cmake -S 'glfw-3.4' -B 'glfw-3.4/build' -D BUILD_SHARED_LIBS=ON -D GLFW_BUILD_WAYLAND=OFF -D GLFW_BUILD_EXAMPLES=OFF -D GLFW_BUILD_TESTS=OFF -D GLFW_BUILD_DOCS=OFF && \
    cmake --build 'glfw-3.4/build' && \
    mkdir /glfw && \
    cp glfw-3.4/build/src/libglfw* /glfw && \
    rm -rf 'glfw-3.4'

FROM ubuntu:24.04

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get dist-upgrade -y && \
    apt-get install -y \
    ca-certificates-java \
    software-properties-common \
    wget && \
    add-apt-repository ppa:kisak/kisak-mesa && \
    wget -q https://packages.mozilla.org/apt/repo-signing-key.gpg -O- > /etc/apt/keyrings/packages.mozilla.org.asc && \
    echo 'deb [signed-by=/etc/apt/keyrings/packages.mozilla.org.asc] https://packages.mozilla.org/apt mozilla main' > /etc/apt/sources.list.d/mozilla.list && \
    printf 'Package: *\nPin: origin packages.mozilla.org\nPin-Priority: 1000\n' > /etc/apt/preferences.d/mozilla && \
    apt-get update && \
    apt-get install -y \
    curl \
    firefox \
    less \
    libflite1 \
    libmtdev1t64 \
    libpulse0 \
    locales \
    lsb-release \
    mesa-utils \
    nano \
    openjdk-17-jre \
    socat \
    unzip \
    xclip \
    xdg-utils \
    xfonts-base \
    xsel && \
    rm -rf /var/lib/apt/lists/* && \
    fc-cache

RUN sed -i '/en_GB.UTF-8/s/^# //g' /etc/locale.gen && locale-gen

RUN curl -o 'atlauncher-1.4.1.deb' -q 'https://download.nodecdn.net/containers/atl/app/dist/linux/atlauncher-1.4-1.deb' && \
    apt-get update && \
    apt install -y './atlauncher-1.4.1.deb' && \
    rm -rf /var/lib/apt/lists/* && \
    rm 'atlauncher-1.4.1.deb'

RUN mkdir -p /opt/minecraft
COPY --from=glfw /glfw /opt/minecraft/glfw
COPY config_diff.sh /opt/minecraft/config_diff.sh
RUN chmod ugo=rx /opt/minecraft/config_diff.sh
COPY dynmap_diff.sh /opt/minecraft/dynmap_diff.sh
RUN chmod ugo=rx /opt/minecraft/dynmap_diff.sh

RUN mkdir -p /opt/archipelago && \
    cd /opt/archipelago && \
    wget -q -O archipelago.tar.gz 'https://github.com/ArchipelagoMW/Archipelago/releases/download/0.5.1/Archipelago_0.5.1.Hotfix1_linux-x86_64.tar.gz' && \
    tar xf archipelago.tar.gz && \
    rm archipelago.tar.gz

WORKDIR /minecraft

CMD atlauncher --working-dir /minecraft/atlauncher
