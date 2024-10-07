core_src = [
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
  'sourcehook/sourcehook_hookmangen.cpp',
  'gamedll_bridge.cpp',
]

foreach sdk : get_option('sdks')
  sdk_proj = subproject('hl2sdk-' + sdk)

  sdk_platforms = sdk_proj.get_variable('sdk_platforms')
  if target_machine.system() not in sdk_platforms
    error(sdk, 'not supported for', target_machine.system())
  elif target_machine.cpu_family() not in sdk_platforms[target_machine.system()]
    error(sdk, 'not supported for', target_machine.system(), target_machine.cpu_family())
  endif

  sdk_suffix = sdk_proj.get_variable('sdk_suffix')
  sdk_source2 = sdk_proj.get_variable('sdk_source2', false)
  sdk_src = []
  sdk_inc = sdk_proj.get_variable('sdk_inc')
  sdk_libs = sdk_proj.get_variable('sdk_libs')
  sdk_deps = sdk_proj.get_variable('sdk_deps')
  sdk_cpp_args = sdk_proj.get_variable('sdk_cpp_args')
  sdk_link_args = []
  sdk_proto = sdk_proj.get_variable('sdk_proto')
  sdk_proto_lib = sdk_proj.get_variable('sdk_proto_lib')

  if sdk_source2
    sdk_src += [
      'provider/source2/provider_source2.cpp',
    ]
    sdk_cpp_args += [
      '-DMETA_IS_SOURCE2',
    ]
  else
    sdk_src += [
      'provider/source/provider_source.cpp',
      'provider/source/provider_source_console.cpp',
      'vsp_bridge.cpp',
    ]
  endif

  if target_machine.system() == 'linux'
    if compiler.get_id() == 'gcc'
      sdk_link_args += [
        '-static-libgcc',
      ]
    elif compiler.get_id() == 'clang'
      sdk_link_args += [
        '-lgcc_eh',
      ]
    endif
    sdk_link_args += [
      '-static-libstdc++',
    ]
  elif target_machine.system() == 'mac'
    sdk_link_args += ['-liconv']
  endif

  if target_machine.cpu_family() == 'x86_64' and target_machine.system() != 'linux'
    sdk_src += ['sourcehook/sourcehook_hookmangen_x86_64.cpp']
  elif target_machine.cpu_family() == 'x86'
    sdk_src += ['sourcehook/sourcehook_hookmangen_x86.cpp']
  endif

  build_target('metamod.' + sdk_suffix,
    name_prefix: '',
    target_type: 'shared_library',
    override_options: 'b_lundef=false',
    sources: [
      core_src,
      sdk_src,
      sdk_proto,
    ],
    include_directories: [
      core_inc,
      sdk_inc,
      loader_inc,
      sourcehook_inc,
      amtl_inc,
      public_inc,
    ],
    cpp_args: [
      sdk_cpp_args,
    ],
    link_args: [
      sdk_link_args,
    ],
    dependencies: [
      sdk_deps,
    ],
    link_with: [
      versionlib,
      sdk_libs,
    ]
  )
endforeach