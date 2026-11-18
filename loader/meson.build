loader_src = [
  'loader.cpp',
  'gamedll.cpp',
  'serverplugin.cpp',
  'utility.cpp',
]

if target_machine.system() == 'linux'
  build_target('server',
    target_type: 'shared_library',
    override_options: 'b_lundef=false',
    sources: loader_src,
    include_directories: [
      sourcehook_inc,
    ],
    name_prefix: '',
    cpp_args: [
      '-DLIB_PREFIX=""',
      '-DLIB_SUFFIX=".so"',
    ]
  )

  if target_machine.cpu_family() == 'x86_64'
    build_target('libserver',
      target_type: 'shared_library',
      override_options: 'b_lundef=false',
      sources: loader_src,
      include_directories: [
        sourcehook_inc,
      ],
      name_prefix: '',
      cpp_args: [
        '-DLIB_PREFIX="lib"',
        '-DLIB_SUFFIX=".so"',
      ]
    )
  elif target_machine.cpu_family() == 'x86'
    build_target('server_i486',
      target_type: 'shared_library',
      override_options: 'b_lundef=false',
      sources: loader_src,
      include_directories: [
        sourcehook_inc,
      ],
      cpp_args: [
        '-DLIB_PREFIX=""',
        '-DLIB_SUFFIX="_i486.so"',
      ]
    )
  endif
else
  build_target('server',
    target_type: 'shared_library',
    override_options: 'b_lundef=false',
    sources: loader_src,
    include_directories: [
      sourcehook_inc,
    ],
  )
endif