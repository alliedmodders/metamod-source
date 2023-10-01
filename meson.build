project('metamod', 'c', 'cpp')

fs = import('fs')
compiler = meson.get_compiler('cpp')

if compiler.get_id() == 'clang' or compiler.get_id() == 'gcc'
  global_defines = [
    '-Dstricmp=strcasecmp',
    '-D_stricmp=strcasecmp',
    '-D_snprintf=snprintf',
    '-D_vsnprintf=vsnprintf',
    '-DHAVE_STDINT_H',
    '-DGNUC',
  ]
  add_project_arguments(global_defines, language: ['c', 'cpp'])

  global_arguments = [
    '-pipe',
    '-fno-strict-aliasing',
    '-Wall',
    '-Werror',
    '-Wno-uninitialized',
    '-Wno-unused',
    '-Wno-switch',
    '-msse',
    '-fPIC',
    '-fno-omit-frame-pointer',
  ]
  add_project_arguments(global_arguments, language: ['c', 'cpp'])

  global_cxx_arguments = [
    '-std=c++14',
    '-fno-exceptions',
    '-fno-rtti',
    '-fno-threadsafe-statics',
    '-Wno-non-virtual-dtor',
    '-Wno-overloaded-virtual',
  ]
  add_project_arguments(global_cxx_arguments, language: ['cpp'])

  if compiler.has_argument('-Wno-delete-non-virtual-dtor')
    add_project_arguments('-Wno-delete-non-virtual-dtor', language: 'cpp')
  endif
  if compiler.has_argument('-mfpmath')
    add_project_arguments('-mfpmath=sse', language: 'cpp')
  endif
  if compiler.has_argument('-Wno-implicit-exception-spec-mismatch')
    add_project_arguments('-Wno-implicit-exception-spec-mismatch', language: 'cpp')
  endif
  if compiler.has_argument('-Wno-expansion-to-defined')
    add_project_arguments('-Wno-expansion-to-defined', language: 'cpp')
  endif
  if compiler.has_argument('-Wno-inconsistent-missing-override')
    add_project_arguments('-Wno-inconsistent-missing-override', language: 'cpp')
  endif
  if compiler.has_argument('-Wno-deprecated-register')
    add_project_arguments('-Wno-deprecated-register', language: 'cpp')
  elif compiler.has_argument('-Wno-deprecated')
    add_project_arguments('-Wno-deprecated', language: 'cpp')
  endif
  if compiler.has_argument('-Wno-implicit-int-float-conversion')
    add_project_arguments('-Wno-implicit-int-float-conversion', language: 'cpp')
  endif
  if compiler.has_argument('')
    add_project_arguments('-Wno-tautological-overlap-compare', language: 'cpp')
  endif

  add_project_arguments('-D_LINUX', language: 'cpp')
  add_project_arguments('-DPOSIX', language: 'cpp')
  add_project_arguments('-D_FILE_OFFSET_BITS=64', language: 'cpp')
endif

TargetPlatform = target_machine.system() + '-' + target_machine.cpu_family()
SourcehookIncludes = include_directories('core/sourcehook')
AMTL = include_directories('third_party/amtl')

PossibleSDKs = {
  'l4d': {
    'sdk_def': 'LEFT4DEAD',
    'sdk_code': '13',
    'platforms': ['windows-x86', 'linux-x86'],
    'linux-x86': {
      'dynamic_libs': ['libtier0.so', 'libvstdlib.so'],
    }
  },
  'l4d2': {
    'sdk_def': 'LEFT4DEAD2',
    'sdk_code': '16',
    'platforms': ['windows-x86', 'linux-x86'],
    'linux-x86': {
      'dynamic_libs': ['libtier0_srv.so', 'libvstdlib_srv.so'],
      'flags': ['-DNO_HOOK_MALLOC', '-DNO_MALLOC_OVERRIDE'],
    }
  },
  'csgo': {
    'sdk_def': 'CSGO',
    'sdk_code': '23',
    'platforms': ['windows-x86', 'linux-x86', 'linux-x86_64'],
    'linux-x86': {
      'interfaces': 'interfaces.a',
      'dynamic_libs': ['libtier0.so', 'libvstdlib.so'],
      'flags': ['-D_GLIBCXX_USE_CXX11_ABI=0'],
      'link_flags': ['-lstdc++'],
    },
    'linux-x86_64': {
      'dynamic_libs': ['libtier0_client.so', 'libvstdlib_client.so'],
      'flags': ['-D_GLIBCXX_USE_CXX11_ABI=0'],
      'link_flags': ['-lstdc++'],
    },
  },
  'mock': {
    'sdk_def': 'MOCK',
    'sdk_code': '999',
    'platforms': ['windows-x86', 'windows-x86_64', 'linux-x86', 'linux-x86_64'],
    'linux-x86': {
      'dynamic_libs': ['tier0_i486.so', 'vstdlib_i486.so'],
    },
    'linux-x86_64': {
      'dynamic_libs': ['libtier0_client.so', 'libvstdlib_client.so'],
    },
  }
}

subdir('versionlib')

SDKDefines = []
foreach sdk_name, sdk : PossibleSDKs
  SDKDefines += ['-DSE_' + sdk['sdk_def'] + '=' + sdk['sdk_code']]
endforeach

SDKs = []
foreach sdk_name, sdk : PossibleSDKs
  if TargetPlatform not in sdk['platforms']
    continue
  endif

  sdk_dir_name = sdk_name
  if 'dir_name' in sdk
    sdk_dir_name = sdk['dir_name']
  endif
  sdk_path = join_paths(fs.parent(meson.source_root()), 'hl2sdk-' + sdk_dir_name)
  if not fs.is_dir(sdk_path)
    message(sdk_path)
    continue
  endif

  sdk += {
    'path': sdk_path,
    'name': sdk_name,
  }

  platform_rules = {}
  if TargetPlatform in sdk
    platform_rules += sdk[TargetPlatform]
  endif

  if 'lib_folder' not in platform_rules
    if target_machine.system() == 'linux'
      if target_machine.cpu_family() == 'x86_64'
        platform_rules += { 'lib_folder': join_paths('lib', 'linux64') }
      elif target_machine.cpu_family() == 'x86'
        platform_rules += { 'lib_folder': join_paths('lib', 'linux') }
      endif
    endif
  endif

  if 'tier1_name' not in platform_rules
    if target_machine.system() == 'linux'
      if target_machine.cpu_family() == 'x86_64'
        platform_rules += { 'tier1': 'tier1.a' }
      elif target_machine.cpu_family() == 'x86'
        platform_rules += { 'tier1': 'tier1_i486.a' }
      endif
    endif
  endif

  link_flags = []
  if target_machine.system() == 'linux'
    if sdk_name == 'mock'
      link_flags += ['-Wl,-z,-origin']
    endif
    link_flags += [join_paths(sdk_path, platform_rules['lib_folder'], platform_rules['tier1'])]
  endif

  if 'interfaces' in platform_rules
    link_flags += [join_paths(sdk_path, platform_rules['lib_folder'], platform_rules['interfaces'])]
  endif
  if 'mathlib' in platform_rules
    link_flags += [join_paths(sdk_path, platform_rules['lib_folder'], platform_rules['mathlib'])]
  endif

  if target_machine.system() == 'linux'
    link_flags = ['-lm'] + link_flags
  endif
  if 'link_flags' in platform_rules
    link_flags += platform_rules['link_flags']
  endif

  include_dirs = [
    AMTL,
    SourcehookIncludes,
    include_directories('loader'),
    include_directories('public'),
  ]

  paths = [
    'common',
    'public',
    join_paths('game', 'shared'),
    join_paths('game_shared'),
    join_paths('public', 'dlls'),
    join_paths('public', 'engine'),
    join_paths('public', 'game', 'server'),
    join_paths('public', 'mathlib'),
    join_paths('public', 'tier0'),
    join_paths('public', 'tier1'),
    join_paths('public', 'vstdlib'),
  ]
  foreach path : paths
    full_path = join_paths(sdk_path, path)
    if fs.is_dir(full_path)
      include_dirs += [include_directories(full_path)]
    endif
  endforeach

  flags = []
  flags += SDKDefines
  flags += ['-DSOURCE_ENGINE=' + sdk['sdk_code']]

  if compiler.get_id() in ['gcc', 'clang']
    flags += ['-DCOMPILER_GCC']
  endif
  if target_machine.cpu_family() == 'x86_64'
    flags += ['-DX64BITS', '-DPLATFORM_64BITS']
  endif

  if 'flags' in platform_rules
    flags += platform_rules['flags']
  endif

  platform_rules += {
    'cpp_args': flags,
    'link_args': link_flags,
    'include_directories': include_dirs,
  }
  sdk += { 'rules': platform_rules }

  SDKs += [sdk]
endforeach

subdir('loader')
subdir('core')
