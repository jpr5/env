#!/usr/bin/env bash
#
# Adapted from github:jen20/packer-ubuntu-zfs
#
# Notes:
#
#  - can't put a partition on /etc - ubuntu doesn't mount it (besides /) when booting = no sshd
#  - can't put a partition on /usr - update-grub automation won't find zfs partitions (bizarre!)

set -o errexit
set -o pipefail
set -o xtrace

export DEBIAN_FRONTEND=noninteractive

# Wait for cloudinit on the surrogate to complete before making progress
while [[ ! -f /var/lib/cloud/instance/boot-finished ]]; do
    echo 'Waiting for cloud-init...'
    sleep 1
done

test -f /tmp/sources.list && cp -f /tmp/sources.list /etc/apt/sources.list

apt-get update
apt-get install -y gdisk zfsutils-linux dosfstools debootstrap

if test -d /sys/firmware/efi ; then
    # Partition the new root EBS volume
    sgdisk -Zg -n1:0:+10M -t1:EF00 -c1:GRUB -n2:0:0 -t2:BF01 -c2:ZFS /dev/nvme1n1
    mkfs.fat -v /dev/nvme1n1p1
else
    sgdisk -Zg -n1:0:4095 -t1:EF02 -c1:GRUB -n2:0:0 -t2:BF01 -c2:ZFS /dev/nvme1n1
fi

fdisk -l

# ZFS
zpool create sys /dev/nvme1n1p2 -o altroot=/mnt -o ashift=12 -o cachefile=/etc/zfs/zpool.cache -o autoexpand=on -O canmount=off -O compression=lz4 -O atime=off -O normalization=formD -m none

zfs create sys/os        -o mountpoint=/ -o canmount=noauto
zfs mount sys/os

# Make distinct locations we want to backup (home, srv, local) zfs mount points/snapshottable
zfs create sys/home      -o mountpoint=/home -o setuid=off
zfs create sys/home/root -o mountpoint=/root
zfs create sys/local     -o mountpoint=/usr/local
zfs create sys/srv       -o mountpoint=/srv
zfs create sys/srv/logs  -o mountpoint=/srv/logs
zfs create sys/var       -o mountpoint=/var -o setuid=off -o overlay=on
zfs create sys/var/cache -o mountpoint=/var/cache
zfs create sys/var/tmp   -o mountpoint=/var/tmp
zfs create sys/var/spool -o mountpoint=/var/spool
zfs create sys/var/lib   -o mountpoint=/var/lib -o exec=on

# Display ZFS output for debugging purposes
zpool status
zfs list

# Bootstrap Ubuntu into /mnt
debootstrap --arch $CPUARCH focal /mnt

cp /etc/apt/sources.list /mnt/etc/apt/sources.list

# Copy the zpool cache
mkdir -p /mnt/etc/zfs
cp -p /etc/zfs/zpool.cache /mnt/etc/zfs/zpool.cache

# Create mount points and mount the filesystem
mkdir -p /mnt/{dev,proc,sys}
mount --rbind /dev /mnt/dev
mount --rbind /proc /mnt/proc
mount --rbind /sys /mnt/sys
mount --rbind /tmp /mnt/tmp # Scripts need access to SSH agent

# Copy the bootstrap script into place and execute inside chroot
for file in bootstrap.sh setup.sh migrate.sh; do
    test -f /tmp/$file && chroot /mnt /tmp/$file
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
zpool export sys
