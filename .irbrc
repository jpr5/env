require 'irb/completion'
require 'irb/ext/save-history'
ARGV.concat [ "--readline", "--prompt-mode", "simple" ]
IRB.conf[:VERBOSE] = true
IRB.conf[:SAVE_HISTORY] = 100000
IRB.conf[:USE_READLINE] = true
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.ruby_history"
