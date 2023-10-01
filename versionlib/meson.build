python3 = find_program('python3')

versionlib_inputs = [
  join_paths('..', 'support', 'buildbot', 'generate_headers.py'),
  join_paths('..', 'product.version'),
]
versionlib_cmd = [
  python3,
  versionlib_inputs[0],
  meson.source_root(),
  '@OUTDIR@',
]

versionlib_header_tgt = custom_target('versionlib_header',
                                      output: 'metamod_version_auto.h',
                                      input: versionlib_inputs,
                                      command: versionlib_cmd)
versionlib_header = versionlib_header_tgt[0]

versionlib_includes = [
  include_directories(join_paths('..', 'public')),
]

versionlib = static_library('version',
                            ['versionlib.cpp'],
                            versionlib_header,
                            include_directories: versionlib_includes)
