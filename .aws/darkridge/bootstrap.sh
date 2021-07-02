#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o xtrace

# Update APT with new sources
apt-get update

# Install various packages needed for a booting system
export DEBIAN_FRONTEND=noninteractive

# Install various packages needed for a booting system
apt-get install -y \
    linux-aws \
    zfsutils-linux \
    zfs-initramfs \
    gdisk

if test -d /sys/firmware/efi ; then
    mkdir -p /boot/efi
    mount /dev/nvme1n1p1 /boot/efi
fi

# Install GRUB
sed -ri 's/GRUB_CMDLINE_LINUX=.*/GRUB_CMDLINE_LINUX="boot=zfs \$bootfs"/' /etc/default/grub
grub-probe /
grub-install /dev/nvme1n1

# Configure and update GRUB
mkdir -p /etc/default/grub.d
cat << EOF > /etc/default/grub.d/50-aws-settings.cfg
GRUB_RECORDFAIL_TIMEOUT=0
GRUB_TIMEOUT=0
GRUB_CMDLINE_LINUX_DEFAULT="console=tty1 console=ttyS0 ip=dhcp tsc=reliable net.ifnames=0"
GRUB_TERMINAL=console
EOF

update-grub

locale-gen --purge "en_US.UTF-8"
cat << EOF > /etc/default/locale
LANG="en_US.UTF-8"
LANGUAGE="en_US:en"
EOF

# Install OpenSSH
apt-get install -y --no-install-recommends openssh-server

cat << EOF > /etc/netplan/eth0.yaml
network:
  version: 2
  ethernets:
    eth0:
      dhcp4: true
EOF

# Install standard packages
apt-get install -y \
        ubuntu-standard \
        cloud-init

if test -d /sys/firmware/efi ; then
    umount /boot/efi
fi
