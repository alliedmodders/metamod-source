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

def make_objdir_name(p):
    return 'obj-' + util.Platform() + '-' + p.target_arch

parser = run.BuildParser(sourcePath=sys.path[0], api='2.1')
parser.default_arch = 'x86'
parser.default_build_folder = make_objdir_name
parser.options.add_option('--hl2sdk-root', type=str, dest='hl2sdk_root', default=None,
                       help='Root search folder for HL2SDKs')
parser.options.add_option('--mms_path', type=str, dest='mms_path', default=None,
                       help='Metamod:Source source tree folder')
parser.options.add_option('--enable-debug', action='store_const', const='1', dest='debug',
                       help='Enable debugging symbols')
parser.options.add_option('--enable-optimize', action='store_const', const='1', dest='opt',
                       help='Enable optimization')
parser.options.add_option('-s', '--sdks', default='all', dest='sdks',
                       help='Build against specified SDKs; valid args are "all", "present", or '
                            'comma-delimited list of engine names (default: %default)')
parser.options.add_option('--enable-tests', default=False, dest='enable_tests', action='store_true',
                       help='Build tests.')
parser.Configure()
