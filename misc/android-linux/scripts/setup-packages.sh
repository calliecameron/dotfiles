#!/bin/bash
# This runs as root in the emulator to install things

apt-get update &&
apt-get upgrade &&
apt-get dist-upgrade &&
apt-get install sudo openbox tightvncserver openssh-server xterm &&
adduser ubuntu
