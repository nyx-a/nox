#! /usr/bin/env ruby

pattern = eval ARGV.shift rescue nil

if pattern.is_a? Regexp
  # unstable sort
  ARGF.read.lines.sort_by{ pattern.match(_1) ? $& : _1 }.each &:display
else
  puts "usage:"
  puts "  #{$0} [Regular Expression] [File(s)..]"
  puts
end

