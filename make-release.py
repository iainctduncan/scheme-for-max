import os

# this script assumes you have checked out and built the max 8.3 SDK and checked out scheme-for-max at
# Max 8/Packages/max-sdk/source/scheme-for-max

# its job is to copy the package, including the binary assets and scm files, into the 
# dist directory, ready for zipping up for release

version = "0.4.0"     # appended to tarball name
dry_run = False
do_install = True

externals_src = "../../externals"
externals_dest = "dist/Scheme-For-Max/extensions"
scm_src = "s4m/scm"
scm_dest = "dist/Scheme-For-Max/extras"

# list of scheme files required, will be copied into patcher dir in package
# tuple of source/dest
scm_files = [
    ("s4m.scm", "s4m.scm"),
    ("s74.scm", "s74.scm"),
    ("stuff.scm", "stuff.scm"),
    ("loop.scm", "loop.scm"),
    ("utilities.scm", "utilities.scm"),
    ("schedule.scm", "schedule.scm"),
    ("live-api.scm", "live-api.scm"),
]

def do(command):
    "wrapper to allow executing dry runs"
    print(command)
    if not dry_run:
        os.system(command)


def package_release():
    print("\n... Emptying dist directory")
    do("rm -fr dist/*")

    print("\n... Copying the package to the dist directory")
    do("cp -rp Scheme-For-Max dist/")

    print("\n... Copying the built external")
    do("cp -rp %s/s4m.mxo %s" % (externals_src, externals_dest))
    do("cp -rp %s/s4m.mxe %s" % (externals_src, externals_dest))
    do("cp -rp %s/s4m.mxe64 %s" % (externals_src, externals_dest))

    print("\n... Copying the scm files")
    for f in scm_files:
        do("cp %s/%s %s/%s" % (scm_src, f[0], scm_dest, f[1])) 

    print("\n... Removing any swap files")
    do("find dist -type f -name \"*.sw[klmnop]\" -delete")

    print("\n... Creating zip file")
    os.chdir("dist") 
    do("tar cvf Scheme-For-Max-%s.tar Scheme-For-Max" % version)
    os.chdir("..") 


if __name__=="__main__":
    print ("\nmake-release.py: package up a scheme-for-max release")

    if dry_run:
        print("... Dry run, commands only printed")
    package_release()
   
    print("DONE. ready for release") 
   
    # helper for setting up testing packages
    # moves the SDK so Max can't see it and puts the dist package into Max view
    # so that we are running the released package 
    if do_install:
        print("\n...Installing to Max")
        do("mv ~/Documents/Max\ 8/Packages/max-sdk ~/Documents/Max\ 8/max-sdk")
        # remove the symlink
        do("rm ~/Documents/Max\ 8/Packages/Scheme-For-Max")
        do("cp -rp dist/Scheme-For-Max ~/Documents/Max\ 8/Packages/")
        print("\n\nINSTALL DONE, ready to test ")
