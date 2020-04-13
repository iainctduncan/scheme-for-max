import os

# this script assumes you have installed the max 8 SDK and checked out scheme-for-max at
# Max 8/Packages/max-sdk-8.0.3/source/scheme4max

# its job is to copy the package, including the binary assets and scm files, into the 
# dist directory, ready for zipping up for release

version = "0.1"     # goes in the tarball
dry_run = False

externals_src = "../../externals"
externals_dest = "dist/Scheme-For-Max/externals"
scm_src = "s4m.scm/scm"
scm_dest = "dist/Scheme-For-Max/patchers"
patcher_src = "s4m.scm/patchers"
patcher_dest = "dist/Scheme-For-Max/patchers"

scm_files = [
    "scm4max.scm",
    "stuff.scm"
]
patcher_files = [
    "s4m.repl.maxpat",
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
    do("cp -rp %s/s4m.scm.mxo %s" % (externals_src, externals_dest))

    print("\n... Copying the scm files")
    for f in scm_files:
        do("cp %s/%s %s/%s" % (scm_src, f, scm_dest, f)) 

    print("\n... Copying the max patcher files")
    for f in patcher_files:
        do("cp %s/%s %s/%s" % (patcher_src, f, patcher_dest, f)) 

    print("\n... Removing any swap files")
    do("find dist -type f -name \"*.sw[klmnop]\" -delete")

    #print("\n... Creating zip file")
    #do("tar cvzf Scheme-For-Max-%s.zip Scheme-For-Max" % version)

if __name__=="__main__":
    print ("\nmake-release.py: package up a scheme-for-max release")

    dry_run = False
    if dry_run:
        print("... Dry run, commands only printed")
    package_release()
   
    print("DONE. ready for release") 