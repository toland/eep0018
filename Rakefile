require 'erlbox'
require 'erlbox/driver'

YAJL_SRCS = FileList["yajl/src/*.c"]
YAJL_OBJS = YAJL_SRCS.pathmap("%X.o")

C_OBJS.include YAJL_OBJS
CLEAN.include YAJL_OBJS

LINK = "#{SRC_DIR}/yajl"
file LINK do
  sh "ln -sf #{PWD}/yajl/src/api #{LINK}"
end

CLOBBER.include LINK

file DRV_DIR => LINK
file DRIVER => YAJL_OBJS

task 'erlbox:prepare' do
  FileUtils.rm_rf "yajl" if File.exists?("yajl")
  sh "git submodule update --init"
end
