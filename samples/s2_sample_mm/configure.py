# vim: set sts=2 ts=8 sw=2 tw=99 et:
import sys
try:
  from ambuild2 import run, util
except:
  try:
    import ambuild
    sys.stderr.write('It looks like you have AMBuild 1 installed, but this project uses AMBuild 2.\n')
    sys.stderr.write('Upgrade to the latest version of AMBuild to continue.\n')
  except:
    sys.stderr.write('AMBuild must be installed to build this project.\n')
    sys.stderr.write('http://www.alliedmods.net/ambuild\n')
  sys.exit(1)

# Hack to show a decent upgrade message, which wasn't done until 2.2.
ambuild_version = getattr(run, 'CURRENT_API', '2.1')
if ambuild_version.startswith('2.1'):
  sys.stderr.write("AMBuild 2.2 or higher is required; please update\n")
  sys.exit(1)

parser = run.BuildParser(sourcePath=sys.path[0], api='2.2')
parser.options.add_argument('-n', '--plugin-name', type=str, dest='plugin_name', default=None,
                       help='Plugin name')
parser.options.add_argument('-a', '--plugin-alias', type=str, dest='plugin_alias', default=None,
                       help='Plugin alias')
parser.options.add_argument('--hl2sdk-root', type=str, dest='hl2sdk_root', default=None,
                       help='Root search folder for HL2SDKs')
parser.options.add_argument('--hl2sdk-manifests', type=str, dest='hl2sdk_manifests', default=None,
                       help='HL2SDK manifests source tree folder')
parser.options.add_argument('--mms_path', type=str, dest='mms_path', default=None,
                       help='Metamod:Source source tree folder')
parser.options.add_argument('--enable-debug', action='store_const', const='1', dest='debug',
                       help='Enable debugging symbols')
parser.options.add_argument('--enable-optimize', action='store_const', const='1', dest='opt',
                       help='Enable optimization')
parser.options.add_argument('-s', '--sdks', default='all', dest='sdks',
                       help='Build against specified SDKs; valid args are "all", "present", or '
                            'comma-delimited list of engine names (default: "all")')
parser.options.add_argument('--targets', type=str, dest='targets', default=None,
                            help="Override the target architecture (use commas to separate multiple targets).")
parser.Configure()
