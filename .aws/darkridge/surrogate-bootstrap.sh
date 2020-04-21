#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o xtrace

export DEBIAN_FRONTEND=noninteractive

# Wait for cloudinit on the surrogate to complete before making progress
while [[ ! -f /var/lib/cloud/instance/boot-finished ]]; do
    echo 'Waiting for cloud-init...'
    sleep 1
done

cp /tmp/sources.list /etc/apt/sources.list

# Update apt and install required packages
apt-get update
apt-get install -y gdisk zfsutils-linux debootstrap

# Partition the new root EBS volume
sgdisk -Zg -n1:0:4095 -t1:EF02 -c1:GRUB -n2:0:0 -t2:BF01 -c2:ZFS /dev/xvdf

zpool create rpool /dev/xvdf2 -o altroot=/mnt -o ashift=12 -o cachefile=/etc/zfs/zpool.cache -O canmount=off -O compression=lz4 -O atime=off -O normalization=formD -m none

zfs create rpool/os        -o mountpoint=/ -o canmount=noauto
zfs mount rpool/os

zfs create rpool/home      -o mountpoint=/home -o setuid=off
zfs create rpool/home/root -o mountpoint=/root
zfs create rpool/local     -o mountpoint=/usr/local
zfs create rpool/srv       -o mountpoint=/srv
zfs create rpool/srv/logs  -o mountpoint=/srv/logs
zfs create rpool/var       -o mountpoint=/var -o setuid=off -o overlay=on
zfs create rpool/var/cache -o mountpoint=/var/cache
zfs create rpool/var/tmp   -o mountpoint=/var/tmp
zfs create rpool/var/spool -o mountpoint=/var/spool
zfs create rpool/var/lib   -o mountpoint=/var/lib -o exec=on

# Display ZFS output for debugging purposes
zpool status
zfs list

# Bootstrap Ubuntu Yakkety into /mnt
debootstrap --arch amd64 focal /mnt
cp /tmp/sources.list /mnt/etc/apt/sources.list

# Copy the zpool cache
mkdir -p /mnt/etc/zfs
cp -p /etc/zfs/zpool.cache /mnt/etc/zfs/zpool.cache

# Create mount points and mount the filesystem
mkdir -p /mnt/{dev,proc,sys}
mount --rbind /dev /mnt/dev
mount --rbind /proc /mnt/proc
mount --rbind /sys /mnt/sys

# Scripts need access to SSH agent
mount --rbind /tmp /mnt/tmp

# Copy the bootstrap script into place and execute inside chroot
for file in chroot-bootstrap.sh post-install.sh; do
    chroot /mnt /tmp/$file
done

umount -l /mnt/tmp

# Remove temporary sources list - CloudInit regenerates it
rm -f /mnt/etc/apt/sources.list

# Unmount bind mounts
umount -l /mnt/dev
umount -l /mnt/proc
umount -l /mnt/sys

sleep 5

# Export the zpool
zpool export rpool
