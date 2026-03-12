FROM ubuntu:24.04 AS glfw

RUN apt-get update && \
    apt-get -y install \
    build-essential \
    cmake \
    libwayland-bin \
    libwayland-dev \
    libxkbcommon-dev \
    unzip \
    wayland-protocols \
    wget \
    xorg-dev && \
    rm -rf /var/lib/apt/lists/*

RUN wget -O glfw-3.4.zip https://github.com/glfw/glfw/releases/download/3.4/glfw-3.4.zip && \
    unzip glfw-3.4.zip && \
    wget -O glfw-wayland-minecraft-cursorfix.tar.gz https://aur.archlinux.org/cgit/aur.git/snapshot/glfw-wayland-minecraft-cursorfix.tar.gz && \
    echo '36dc6f5db7ed17e5d186b72c0c33496bad3efb4733a12251f80ec6a54ef8d428  glfw-wayland-minecraft-cursorfix.tar.gz' > checksum && \
    sha256sum -c checksum && \
    tar -xf glfw-wayland-minecraft-cursorfix.tar.gz && \
    for patch in glfw-wayland-minecraft-cursorfix/*.patch; do patch -d glfw-3.4 -p1 < "${patch}"; done && \
    cmake -S 'glfw-3.4' -B 'glfw-3.4/build' -D BUILD_SHARED_LIBS=ON -D GLFW_BUILD_EXAMPLES=OFF -D GLFW_BUILD_TESTS=OFF -D GLFW_BUILD_DOCS=OFF && \
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
    wget -O /usr/share/keyrings/prismlauncher-archive-keyring.gpg https://prism-launcher-for-debian.github.io/repo/prismlauncher.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/prismlauncher-archive-keyring.gpg] https://prism-launcher-for-debian.github.io/repo noble main" > /etc/apt/sources.list.d/prismlauncher.list && \
    apt-get update && \
    apt-get -y install \
    curl \
    firefox \
    htop \
    less \
    libflite1 \
    libmtdev1t64 \
    libpulse0 \
    libxv1 \
    locales \
    lsb-release \
    mesa-utils \
    mesa-utils-extra \
    nano \
    openjdk-17-jdk \
    prismlauncher \
    socat \
    unzip \
    va-driver-all \
    vainfo \
    vdpau-driver-all \
    xclip \
    xdg-utils \
    xfonts-base \
    xsel && \
    rm -rf /var/lib/apt/lists/* && \
    fc-cache

RUN sed -i '/en_GB.UTF-8/s/^# //g' /etc/locale.gen && locale-gen

RUN mkdir -p /opt/minecraft
COPY --from=glfw /glfw /opt/minecraft/glfw
COPY minecraft-config-diff /usr/bin/minecraft-config-diff
RUN chmod ugo=rx /usr/bin/minecraft-config-diff
COPY minecraft-dynmap-diff /usr/bin/minecraft-dynmap-diff
RUN chmod ugo=rx /usr/bin/minecraft-dynmap-diff

WORKDIR /minecraft

CMD prismlauncher --dir /minecraft/prismlauncher
