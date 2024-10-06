project('metamod', 'c', 'cpp', default_options: [
  'warning_level=1',
  'werror=true',
  'cpp_std=c++14',
])

compiler = meson.get_compiler('cpp')
if compiler.get_id() == 'clang' or compiler.get_id() == 'gcc'
  # C/C++ Defines
  add_project_arguments([
    '-Dstricmp=strcasecmp',
    '-D_stricmp=strcasecmp',
    '-D_snprintf=snprintf',
    '-D_vsnprintf=vsnprintf',
    '-DHAVE_STDINT_H',
    '-DGNUC',
  ], language: ['c', 'cpp'])

  # C/C++ Arguments
  add_project_arguments([
    '-pipe',
    '-fno-strict-aliasing',
    '-Wno-uninitialized',
    '-Wno-unused',
    '-Wno-switch',
    '-msse',
    '-fPIC',
    '-fno-omit-frame-pointer',
  ], language: ['c', 'cpp'])

  # C++ Arguments
  add_project_arguments([
    '-fno-exceptions',
    '-fno-rtti',
    '-fno-threadsafe-statics',
    '-Wno-non-virtual-dtor',
    '-Wno-overloaded-virtual',
  ], language: 'cpp')

  # Optional C++ Arguments
  optional_arguments = [
    '-Wno-delete-non-virtual-dtor',
    '-mfpmath=sse',
    '-Wno-implicit-exception-spec-mismatch',
    '-Wno-expansion-to-defined',
    '-Wno-inconsistent-missing-override',
    '-Wno-deprecated-register',
    '-Wno-deprecated',
    '-Wno-implicit-int-float-conversion',
    '-Wno-tautological-overlap-compare',
  ]
  foreach arg : optional_arguments
    if compiler.has_argument(arg)
      add_project_arguments(arg, language: 'cpp')
    endif
  endforeach

  # Platform Arguments
  add_project_arguments('-D_LINUX', language: 'cpp')
  add_project_arguments('-DPOSIX', language: 'cpp')
  add_project_arguments('-D_FILE_OFFSET_BITS=64', language: 'cpp')
endif

public_inc = include_directories('public')
core_inc = include_directories('core')
sourcehook_inc = include_directories('core/sourcehook')

subdir('versionlib')
subdir('loader')