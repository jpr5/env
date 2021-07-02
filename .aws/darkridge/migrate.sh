#!/usr/bin/env bash

apt-get install -y php-cgi php-fpm
apt-get install -y php # has to separately follow php-cgi || php-fpm in order to avoid apache
apt-get install -y \
        dovecot-core dovecot-imapd postfix nginx \
        opendkim opendkim-tools


service opendkim stop
service postfix stop
service dovecot stop
service nginx stop


# SSL CERT SYMLINKS
rsync -Pavz root@$SRCHOST:/etc/letsencrypt/ /etc/letsencrypt/
rsync -Pavz "root@$SRCHOST:/etc/ssl/{old_certs,private,darkridge.pem*}" /etc/ssl/
#certbot --standalone certonly --expand -d darkridge.com,www.darkridge.com,mail.darkridge.com,darkrid.ge,www.darkrid.ge,darkridge.digital,www.darkridge.digital,$SRCHOST,$TARGETHOST.darkridge.com
#certbot renew --renew-by-default && certbot certificates # && service nginx restart && service dovecot restart


# nginx
cd /etc/nginx
mv nginx.conf nginx.conf.orig
rsync -Pavzb root@$SRCHOST:/etc/nginx/{nginx.conf,fastcgi.conf,conf.d} .


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


###
### Final Setup (after new AMI bootup)
###

## user accounts
#passwd jpr5
#rsync -Pavz root@$SRCHOST:/home/ /home/
# migrate passwd/shadow/group entries

## cleanup
#hostnamectl set-hostname $TARGETHOST
#userdel -r ubuntu
#touch /etc/securetty
#rm -rf /var/log.old
#rsync -Pavz $SRCHOST:/srv/{http,https} /srv/
#rbenv install 2.6.4
#rbenv global 2.6.4
#gem install aws-sdk-s3

## backup: install ~jpr5/.aws/darkridge/backup.env
#echo @weekly ruby ~jpr5/.aws/darkridge/backup.rb >> /var/spool/cron/crontabs/root

## Update certs to include new hostname
#certbot --standalone certonly --expand -d darkridge.com,www.darkridge.com,mail.darkridge.com,darkrid.ge,www.darkrid.ge,darkridge.digital,www.darkridge.digital,$SRCHOST,$TARGETHOST.darkridge.com
