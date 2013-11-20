# vim: set ts=8 sts=2 sw=2 tw=99 et:
import re
import os, sys
import subprocess

argv = sys.argv[1:]
if len(argv) < 2:
  sys.stderr.write('Usage: generate_headers.py <source_path> <output_folder>\n')
  sys.exit(1)

SourceFolder = os.path.abspath(os.path.normpath(argv[0]))
OutputFolder = os.path.normpath(argv[1])

def get_hg_version():
  argv = ['hg', 'parent', '-R', SourceFolder]

  # Python 2.6 doesn't have check_output.
  if hasattr(subprocess, 'check_output'):
    text = subprocess.check_output(argv)
    if str != bytes:
      text = str(text, 'utf-8')
  else:
    p = subprocess.Popen(argv, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output, ignored = p.communicate()
    rval = p.poll()
    if rval:
      raise subprocess.CalledProcessError(rval, argv)
    text = output.decode('utf8')

  m = re.match('changeset:\s+(\d+):(.+)', text)
  if m == None:
    raise Exception('Could not determine repository version')
  return m.groups()

def output_version_header():
  rev, cset = get_hg_version()
  with open(os.path.join(SourceFolder, 'product.version')) as fp:
    productContents = fp.read()
  m = re.match('(\d+)\.(\d+)\.(\d+)(.*)', productContents)
  if m == None:
    raise Exception('Could not detremine product version')
  major, minor, release, tag = m.groups()

  with open(os.path.join(OutputFolder, 'metamod_version_auto.h'), 'w') as fp:
    fp.write("""#ifndef _METAMOD_AUTO_VERSION_INFORMATION_H_
#define _METAMOD_AUTO_VERSION_INFORMATION_H_

#define MMS_BUILD_STRING     \"{0}\"
#define MMS_BUILD_UNIQUEID    \"{1}:{2}\" MMS_BUILD_STRING
#define MMS_FULL_VERSION    \"{3}.{4}.{5}\" MMS_BUILD_STRING
#define MMS_FILE_VERSION    {6},{7},{8},0

#endif /* _METAMOD_AUTO_VERSION_INFORMATION_H_ */

""".format(tag, rev, cset, major, minor, release, major, minor, release))

output_version_header()

