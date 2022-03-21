#! /usr/bin/env ruby

pattern = eval ARGV.shift rescue nil

unless pattern.is_a? Regexp
  puts "usage:"
  puts "  #{$0} [Regular Expression] [File(s)..]"
  puts
  exit
end

# perform unstable sort
ARGF.read.lines.sort_by do
  pattern.match(_1) ? $& : _1
end.each &:display

