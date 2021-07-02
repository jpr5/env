#!/usr/bin/env bash
####
#### NOTES
####

#SRCHOST=balls.darkridge.com # Set from packer template

set -o errexit
set -o pipefail
set -o xtrace

# known_hosts setup for rsync'ing
mkdir -p ~/.ssh
chmod 711 ~/.ssh
ssh-keyscan `host $SRCHOST` github.com >> ~/.ssh/known_hosts

# /srv setup
mkdir -p /srv/logs
chown root:adm /srv
chown root:syslog /srv/logs
chmod 775 /srv/logs # somehow not sticking, so we add to cron:
cat << EOF >> /var/spool/cron/crontabs/root
@reboot chmod g+w /srv/logs >> /var/spool/cron/crontabs/root
EOF
chown root:crontab /var/spool/cron/crontabs/root
chmod 600 /var/spool/cron/crontabs/root
cd /var
mv log log.old
ln -s /srv/logs ./log
sed -i -e 's/var\/log/srv\/logs/g' /etc/rsyslog.d/50-default.conf
sed -i -e 's/auth,authpriv.none/auth,authpriv,local0,local1,local7.none/' /etc/rsyslog.d/50-default.conf
service rsyslog stop

# wtmp logrotation
sed -i 's/monthly/yearly/' /etc/logrotate.d/wtmp

# install our packages
apt-get install -y \
        net-tools ngrep tcpdump wget curl telnet netcat whois \
        bc screen psmisc strace lsof file rsync parallel \
        unzip zip bzip2 xz-utils \
        gnupg \
        vim emacs-nox autoconf git git-lfs \
        build-essential \
        libssl-dev libreadline-dev zlib1g-dev \
        libmysqlclient-dev \
        zsh \
        awscli

apt-get install -y \
        nginx ssl-cert certbot

# nginx
groupadd -og 33 www
useradd -ou 33 -g 33 www
mkdir -p /srv/logs/nginx
chown www:syslog /srv/logs/nginx
chmod 775 /srv/logs/nginx
chmod g+s /srv/logs/nginx
sed -i.bak -e 's/var\/log\/nginx/srv\/logs\/nginx/' /etc/nginx/nginx.conf
sed -i.bak -e 's/0640/0660/' -e 's/adm/syslog/' /etc/logrotate.d/nginx

# git lfs
git lfs install --system --skip-smudge

# FFR: support the other AWS zones
# TIME
# tzselect #  2 <enter> 49 <enter> 21 <enter> 1 <enter>
rm /etc/localtime && ln -s /usr/share/zoneinfo/US/Pacific /etc/localtime

# SSHD

sed -ie 's/X11Forwarding yes/X11Forwarding no/g' /etc/ssh/sshd_config
# Sudo should keep the ssh agent connection
echo Defaults env_keep = \"SSH_AUTH_SOCK\" > /etc/sudoers.d/keep_auth_sock

# post-setup jpr5
PASSWORD=`echo foobar | mkpasswd --stdin -m sha-512`
useradd -m -s /bin/bash -p "$PASSWORD" jpr5
usermod -a -G users,staff,adm,root,sudo,ssh jpr5
cd /home/jpr5
mkdir .ssh
cd .ssh
rsync -Pavz root@$SRCHOST:/home/jpr5/.ssh/authorized_keys .
chmod 600 authorized_keys
chmod 711 .
cd ..
git clone https://github.com/jpr5/env
cd env
cp -ra .[a-zA-Z]* ..
cd ..
rm -rf env
chown -R jpr5:jpr5 .

# Add jpr5 bashrc and authorized_keys for root
cd /root
ln -fs ~jpr5/.bashrc
ln -fs ~jpr5/.zshrc
mkdir -p .ssh
cp ~jpr5/.ssh/authorized_keys .ssh/
chmod 600 .ssh/authorized_keys
chmod 711 .ssh
chown -R root:root .

# Source setup

mkdir -p /srv/src
chgrp staff /srv/src
chmod 1777 /srv/src
chmod g+s /srv/src

# Setup  global RBENV / Ruby support

#cd /usr/local
#git clone https://github.com/sstephenson/rbenv.git
#cd rbenv
#mkdir plugins
#cd plugins
#git clone https://github.com/sstephenson/ruby-build.git
#cd ruby-build
#./install.sh
#cd /usr/local
#chgrp -R staff rbenv
#chmod -R g+rw rbenv
#find rbenv -type d -print0 | xargs -0 chmod g+sx
#cd /usr/local/bin
#ln -s ../rbenv/bin/rbenv
#echo 'export RBENV_ROOT=/usr/local/rbenv' > /etc/profile.d/rbenv.sh
#echo 'eval "$(rbenv init -)"' >>  /etc/profile.d/rbenv.sh
#chmod a+x /etc/profile.d/rbenv.sh
#echo source /etc/profile.d/rbenv.sh >> /etc/zsh/zshenv

ARCH=`arch`
BUCKET=https://augmentco-build.s3-us-west-2.amazonaws.com

cd /usr/local
VER=`curl $BUCKET/rbenv-$ARCH-latest.txt`
curl "$BUCKET/rbenv-$VER-$ARCH.tar.xz" | tar Jxv

chgrp -R staff rbenv
chmod -R g+rw rbenv
find rbenv -type d -print0 | xargs -0 chmod g+sx

echo 'export RBENV_ROOT=/usr/local/rbenv' > /etc/profile.d/rbenv.sh
echo 'export PATH=$RBENV_ROOT/bin:$PATH' >> /etc/profile.d/rbenv.sh
echo 'eval "$(rbenv init -)"' >>  /etc/profile.d/rbenv.sh
chmod a+x /etc/profile.d/rbenv.sh
echo source /etc/profile.d/rbenv.sh >> /etc/zsh/zshenv

# Up-to-date the OS

apt-get upgrade -y
