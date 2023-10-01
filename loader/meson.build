loader_srcs = [
  'loader.cpp',
  'gamedll.cpp',
  'serverplugin.cpp',
  'utility.cpp',
]

loader_cpp_args = []
loader_include_directories = [
  SourcehookIncludes
]
loader_link_args = ['-lgcc_eh']
if target_machine.system() == 'linux'
  if target_machine.cpu_family() == 'x86_64'
    shared_module('libserver', loader_srcs,
                   include_directories: loader_include_directories,
                   link_args: loader_link_args,
                   name_prefix: '',
                   cpp_args: ['-DLIB_PREFIX="lib"', '-DLIB_SUFFIX=".so"'])
  elif target_machine.cpu_family() == 'x86'
    shared_module('server_i486', loader_srcs,
                   include_directories: loader_include_directories,
                   link_args: loader_link_args,
                   name_prefix: '',
                   cpp_args: ['-DLIB_PREFIX=""', '-DLIB_SUFFIX="_i486.so"'])
  endif
endif

shared_module('server', loader_srcs,
               include_directories: loader_include_directories,
               link_args: loader_link_args,
               name_prefix: '',
               cpp_args: ['-DLIB_PREFIX="lib"', '-DLIB_SUFFIX=".so"'])
