#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o xtrace

SRCHOST=brainfart.darkridge.com
TARGETHOST=balls

# known_hosts setup for rsync'ing
mkdir -p ~/.ssh
chmod 711 ~/.ssh
ssh-keyscan `host $SRCHOST` github.com >> ~/.ssh/known_hosts

# /srv setup
chown root:adm /srv

chown root:syslog /srv/logs
chmod 775 /srv/logs # somehow not sticking, so we add to cron:
cat << EOF >> /var/spool/cron/crontabs/root
MAILTO=jpr5
@reboot chmod g+w /srv/logs >> /var/spool/cron/crontabs/root
EOF
chown root:crontab /var/spool/cron/crontabs/root
chmod 600 /var/spool/cron/crontabs/root
cd /var
mv log log.old
ln -s /srv/logs ./log
sed -i 's/var\/log/srv\/logs/g' /etc/rsyslog.d/50-default.conf
service rsyslog stop

# wtmp logrotation
sed -i 's/monthly/yearly/' /etc/logrotate.d/wtmp

# install our packages
apt-get install -y \
        zsh \
        net-tools ngrep wget curl whois telnet tcpdump \
        bc screen psmisc strace lsof file rsync \
        unzip zip bzip2 xz-utils \
        gnupg \
        vim emacs-nox autoconf git git-lfs \
        build-essential \
        libssl-dev libreadline-dev zlib1g-dev \
        php-cgi php-fpm

# has to separately follow php-cgi || php-fpm in order to avoid apache
apt-get install -y php

apt-get install -y \
        dovecot-core dovecot-imapd postfix nginx \
        opendkim opendkim-tools \
        certbot

# global RBENV / Ruby
cd /usr/local
git clone https://github.com/sstephenson/rbenv.git
cd rbenv
mkdir plugins
cd plugins
git clone https://github.com/sstephenson/ruby-build.git
cd ruby-build
./install.sh
cd /usr/local
chgrp -R staff rbenv
chmod -R g+rw rbenv
find rbenv -type d -print0 | xargs -0 chmod g+sx
cd /usr/local/bin
ln -s ../rbenv/bin/rbenv
echo 'export RBENV_ROOT=/usr/local/rbenv' > /etc/profile.d/rbenv.sh
echo 'eval "$(rbenv init -)"' >>  /etc/profile.d/rbenv.sh
chmod a+x /etc/profile.d/rbenv.sh
echo source /etc/profile.d/rbenv.sh >> /etc/zsh/zshenv

# git lfs
git lfs install --system

# hostname
hostname $TARGETHOST
echo $TARGETHOST > /etc/hostname

# TIME
# tzselect #  2 <enter> 49 <enter> 21 <enter> 1 <enter>
rm /etc/localtime && ln -s /usr/share/zoneinfo/US/Pacific /etc/localtime

# Setup jpr5
PASSWORD=`echo foobar | mkpasswd --stdin -m sha-512`
useradd -m -s /bin/zsh -p "$PASSWORD" jpr5
usermod -a -G users,staff,adm,root,sudo,ssh jpr5
cd /home/jpr5
mkdir .ssh
cd .ssh
rsync -Pavz root@$SRCHOST:/home/jpr5/.ssh/authorized_keys .
chmod 600 authorized_keys
chmod 711 .
cd ..
mkdir .rbenv
git clone https://github.com/jpr5/env
cd env
cp -ra .[a-zA-Z]* ..
cd ..
rm -rf env
chown -R jpr5:jpr5 .

# Add jpr5 zshrc and authorized_keys for root
usermod -s /bin/zsh root
cd /root
ln -s ~jpr5/.zshrc
mkdir -p .ssh
cp ~jpr5/.ssh/authorized_keys .ssh/
chmod 600 .ssh/authorized_keys
chmod 711 .ssh
chown -R root:root .

# Migrate old sshd config
rsync -Pavz root@$SRCHOST:/etc/ssh/sshd_config /etc/ssh/sshd_config.d/sshd_config.$SRCHOST

# Sudo should keep the ssh agent connection
echo Defaults env_keep = \"SSH_AUTH_SOCK\" > /etc/sudoers.d/keep_auth_sock

service opendkim stop
service postfix stop
service dovecot stop
service nginx stop

# SSL CERT SYMLINKS
rsync -Pavz root@$SRCHOST:/etc/letsencrypt/ /etc/letsencrypt/
rsync -Pavz "root@$SRCHOST:/etc/ssl/{old_certs,private,darkridge.pem*}" /etc/ssl/
#certbot --standalone certonly --expand -d darkridge.com,www.darkridge.com,mail.darkridge.com,darkrid.ge,www.darkrid.ge,darkridge.digital,www.darkridge.digital,$SRCHOST,$TARGETHOST.darkridge.com
#certbot renew --renew-by-default && certbot certificates # && service nginx restart && service dovecot restart

# postfix
cd /etc/postfix
rsync -Pavz root@$SRCHOST:/etc/postfix/{localhosts,Makefile,main.cf,master.cf,virtuals} .
rsync -Pavz root@$SRCHOST:/etc/aliases /etc
echo $TARGETHOST.darkridge.com >> localhosts
echo $TARGETHOST.darkridge.com > /etc/mailname
make
sed -i.bak -e "s/$SRCHOST/$TARGETHOST.darkridge.com/" /etc/postfix/main.cf
# REMOVE FOLLOWING AFTER FIRST SUCCESSFUL USAGE
sed -i.bak -e "s/queue_directory/data_directory/" /etc/postfix/main.cf
cat << EOF >> /etc/postfix/main.cf
data_directory = /var/lib/postfix
queue_directory = /var/spool/postfix
EOF
#service postfix start

# opendkim
cd /etc
rsync -Pavz "root@$SRCHOST:/etc/opendkim*" /etc/


# dovecot
cd /etc/dovecot
rsync -Pavz root@$SRCHOST:/etc/dovecot/local.conf .
sed -i.bak -e "s/$SRCHOST/$TARGETHOST.darkridge.com/" /etc/dovecot/local.conf
#service dovecot start

# nginx
groupadd -og 33 www
useradd -ou 33 -g 33 www
cd /etc/nginx
mv nginx.conf nginx.conf.orig
rsync -Pavzb root@$SRCHOST:/etc/nginx/{nginx.conf,fastcgi.conf,conf.d} .
mkdir -p /srv/logs/nginx
chown root:www /srv/logs/nginx
chmod 775 /srv/logs/nginx
chmod g+s /srv/logs/nginx
sed -i.bak -e 's/srv\/http\/logs/srv\/logs\/nginx/' /etc/nginx/nginx.conf


###
### Final Setup (after new AMI bootup)
###

#passwd jpr5
#echo $TARGETHOST.darkridge.com > /etc/hostname # Amazon overrides w/ new instance launch
#hostname $TARGETHOST
#userdel -r ubuntu
## migrate user accounts
#touch /etc/securetty
#rm -rf /var/log.old
#rsync -Pavz $SRCHOST:/srv/{http,https} /srv/
#rsync -Pavz root@$SRCHOST:/home/ /home/
#rbenv install 2.6.4
#rbenv global 2.6.4
#gem install aws-sdk-s3
## install ~jpr5/.aws/darkridge/backup.env
#echo @weekly ruby ~jpr5/.aws/darkridge/backup.rb >> /var/spool/cron/crontabs/root
## Update certs to include new hostname
#certbot --standalone certonly --expand -d darkridge.com,www.darkridge.com,mail.darkridge.com,darkrid.ge,www.darkrid.ge,darkridge.digital,www.darkridge.digital,$SRCHOST,$TARGETHOST.darkridge.com
