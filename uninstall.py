# put s4m back in dev mode
import os
dry_run = False

def do(command):
    "wrapper to allow executing dry runs"
    print(command)
    if not dry_run:
        os.system(command)


if __name__=="__main__":
    print("removing package and replacing sdk")
    do("mv ~/Documents/Max\ 8/max-sdk-8.0.3 ~/Documents/Max\ 8/Packages/max-sdk-8.0.3")
    # remove installed package
    do("rm -fr ~/Documents/Max\ 8/Packages/Scheme-For-Max")  
    # recreate symlink
    do("ln -s ~/Documents/Max\ 8/Packages/max-sdk-8.0.3/source/scheme4max/Scheme-For-Max "
        "~/Documents/Max\ 8/Packages/Scheme-For-Max")
    print("DONE")
