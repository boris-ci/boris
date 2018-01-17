#!/bin/sh -eux

sudo su -c '(echo boris; date)  >> /etc/packer-run'

sudo apt-get update
sudo sh -c 'DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" upgrade'

sudo curl -o /usr/local/bin/goss -L https://github.com/aelsabbahy/goss/releases/download/v0.3.4/goss-linux-amd64
sudo chmod +x /usr/local/bin/goss

sudo apt-get -y install chrony
sudo su -c 'echo "server 169.254.169.123 prefer iburst" >> /etc/chrony/chrony.conf'
