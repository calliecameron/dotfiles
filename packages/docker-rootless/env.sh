# shellcheck shell=sh

DOCKER_HOST="unix:///run/user/$(id -u)/docker.sock"
export DOCKER_HOST
