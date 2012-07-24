require 'irb/completion'
require 'irb/ext/save-history'

require 'rubygems'
require 'pp'

ARGV.concat [ "--readline", "--prompt-mode", "simple" ]
IRB.conf[:VERBOSE] = true
IRB.conf[:SAVE_HISTORY] = 10000
IRB.conf[:USE_READLINE] = true
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.ruby_history"

module IRB
  def self.start_session(binding)
    IRB.setup(nil)

    workspace = WorkSpace.new(binding)

    if @CONF[:SCRIPT]
      irb = Irb.new(workspace, @CONF[:SCRIPT])
    else
      irb = Irb.new(workspace)
    end

    @CONF[:IRB_RC].call(irb.context) if @CONF[:IRB_RC]
    @CONF[:MAIN_CONTEXT] = irb.context

    trap("SIGINT") do
      irb.signal_handle
    end

    catch(:IRB_EXIT) do
      irb.eval_input
    end
  end
end

module Kernel
    def self.irb
        IRB.start_session(Kernel.binding)
    end
end

module Balls
    def remote_execute(host, *commands)
        commands = commands.flatten.map(&:chomp)

        ssh      = Net::SSH.start(host, `whoami`.chomp)
        channel  = ssh.open_channel do |channel|
            channel.exec("bash -l") do |ch, success|
                ch.on_data          { |_, data| STDOUT.print data }
                ch.on_extended_data { |_, _, data| STDERR.print data }
                ch.on_close         { STDOUT.print "connection closed." }

                commands.each do |c|
                    STDOUT.print "#{c}\n"
                    ch.send_data "#{c}\n"
                end

                ch.send_data("exit\n")
            end
        end

        channel.wait

        return true
    end
end

# Give ourselves some balls.
class << self
    include Balls
end
