require 'rubygems'
require 'rake'

task :default do
  sh "mzc -z -d lib/ src/*.ss"
end

task :console do
  puts "Leetserver Console - All Modules Loaded\n"
  exec "mzscheme -m -S ./lib/ -t lib/*.zo"
end

task :clean do
  sh "git clean -xf"
end
