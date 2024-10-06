fs = import('fs')
metamod_version_string = fs.read('../product.version').strip()

version_tag = metamod_version_string.split('-')

version = version_tag[0].split('.')
major = version[0]
minor = version[1]
release = version[2]

tag = version_tag[1]

revision = run_command('git', 'rev-list', '--count', 'HEAD', check: true).stdout().strip()
shorthash = run_command('git', 'log', '--pretty=format:%h', '-n', '1', check: true).stdout().strip()
product = '.'.join(major, minor, release)

if tag != ''
  product += '-' + tag
  if tag == 'dev'
    product += '+' + revision
  endif
endif

metamod_version_config = configuration_data({
  'tag': tag,
  'shorthash': shorthash,
  'major': major,
  'minor': minor,
  'release': release,
  'product': product,
  'revision': revision,
})

configure_file(
  input: 'metamod_version_auto.h.in',
  output: 'metamod_version_auto.h',
  configuration: metamod_version_config
)

versionlib = static_library(
  'version',
  'versionlib.cpp',
  include_directories: [
    public_inc,
  ]
)