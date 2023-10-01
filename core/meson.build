
core_builds = []
core_sources = [
  'metamod.cpp',
  'metamod_console.cpp',
  'metamod_oslink.cpp',
  'metamod_plugins.cpp',
  'metamod_util.cpp',
  'provider/provider_base.cpp',
  'sourcehook/sourcehook.cpp',
  'sourcehook/sourcehook_impl_chookidman.cpp',
  'sourcehook/sourcehook_impl_chookmaninfo.cpp',
  'sourcehook/sourcehook_impl_cproto.cpp',
  'sourcehook/sourcehook_impl_cvfnptr.cpp',
  'gamedll_bridge.cpp',
  'vsp_bridge.cpp'
]

if target_machine.cpu_family() == 'x86'
  core_sources += [
    'sourcehook/sourcehook_hookmangen.cpp',
  ]
endif

foreach sdk : SDKs
  core_binary_name = 'metamod.2.' + sdk['name']

  sdk_core_sources = core_sources
  if sdk['name'] == 'cs2' or sdk['name'] == 'dota'
    sdk_core_sources += [
      'provider/source2/provider_source2.cpp',
    ]
  else
    sdk_core_sources += [
      'provider/source/provider_source.cpp',
      'provider/source/provider_source_console.cpp',
    ]
  endif

  shared_module(core_binary_name,
                sdk_core_sources,
                versionlib_header,
                name_prefix: '',
                cpp_args: sdk['rules']['cpp_args'],
                link_args: sdk['rules']['link_args'],
                include_directories: sdk['rules']['include_directories'])
endforeach
