#!/usr/bin/env ruby
#
# Backup script (ZFS -> S3)
# Author: Jordan Ritter <jpr5@darkridge.com>

require 'aws-sdk-s3'


#########
# SETUP #
#########

POOL    = 'rpool'
TARGETS = [ 'home', 'local', 'srv' ]

WORKDIR  = "/tmp"
BUCKET   = "jpr5-backups"
CREDS    = "/home/jpr5/.aws/darkridge/backup.env"

File.readlines(CREDS).map(&:chomp).each do |line|
  values = line.split("=")
  ENV[values[0]] = values[1]
end

HOSTNAME = `hostname`.chomp
DATETIME = `date +%Y%m%d%H%M%S`.chomp

$KEEP_XZ = false


########
# MAIN #
########

puts "Beginning backups: #{TARGETS.join(", ")} (#{DATETIME})"

Aws.config.update(
    credentials: Aws::Credentials.new(ENV['AWS_ACCESS_KEY_ID'], ENV['AWS_SECRET_ACCESS_KEY']),
    region: 'us-west-2',
)

$bucket = Aws::S3::Resource.new.bucket(BUCKET)

TARGETS.each do |fs|
    snapshot = "#{POOL}/#{fs}@#{DATETIME}"
    snapxz   = "#{WORKDIR}/#{fs}-#{DATETIME}.zfs.xz"
    key      = "#{HOSTNAME}/#{POOL}/#{fs}-#{DATETIME}.zfs.xz"

    puts "\nSnapshot: #{snapshot}"
    system("zfs snapshot #{snapshot}")

    puts "Compressing: #{snapxz}"
    system("zfs send #{snapshot} | xz -c > #{snapxz}")

    puts "Uploading: #{snapxz} -> #{key}"
    $bucket.object(key).upload_file(snapxz)

    system("rm #{snapxz}") unless $KEEP_XZ
    system("zfs destroy #{snapshot}")
end

puts "\nBackup complete."
