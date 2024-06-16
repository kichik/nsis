from os import listdir, path
import os
import re
import zipfile
import shutil

def build_package(artifacts, output):
    """Merge together x86 and amd64 build packages to form the final NSIS package.

    NSIS packages (x86 and amd64) are built from the `ubuntu` packages, with
    some extra binaries from `windows` packages.

    Arguments:
    artifacts: Input directory with build artifacts
    output: Output directory where final packages are put
    """
    windows_x86_dir   = path.join(output, '.instdist-windows-x86')
    windows_amd64_dir = path.join(output, '.instdist-windows-amd64')
    ubuntu_x86_dir    = path.join(output, '.instdist-ubuntu-x86')
    ubuntu_amd64_dir  = path.join(output, '.instdist-ubuntu-amd64')

    # unzip all files in `artifacts`
    # this step does nothing when running in GitHub workflow, since artifacts are already unzipped
    for file in listdir(artifacts):
        zippath = path.join(artifacts, file)
        if path.splitext(zippath)[1] == '.zip' and path.isfile(zippath):
            unzipdir = path.join(artifacts, path.splitext(zippath)[0])
            if not path.exists(unzipdir):
                print(f"extract( {zippath} --> {unzipdir} )")
                with zipfile.ZipFile(zippath) as zip:
                    zip.extractall(unzipdir)

    # extract the root directory from the inner .zip file (i.e. dist-x86_ubuntu-latest.zip => nsis-0.0.0.0-x86.zip => .\nsis-0.0.0.0 )
    count = 0
    for dir in listdir(artifacts):
        if path.isdir(path.join(artifacts, dir)):

            outdir = None
            for srcre, dstdir in [
                [r'^.*-x86_windows-latest$', windows_x86_dir], [r'^.*-amd64_windows-latest$', windows_amd64_dir],
                [r'^.*-x86_ubuntu-latest$', ubuntu_x86_dir],   [r'^.*-amd64_ubuntu-latest$', ubuntu_amd64_dir]]:
                if (re.match(srcre, dir)):
                    outdir = dstdir
            if outdir == None:
                continue

            for file in listdir(path.join(artifacts, dir)):
                if re.match(r'^nsis-.+\.zip$', file) != None:
                    count += 1
                    print(f"extract( {path.join(artifacts, dir, file)} --> {artifacts} )")
                    with zipfile.ZipFile(path.join(artifacts, dir, file)) as zip:
                        rootdir = zip.filelist[0].filename.split("/")[0]
                        zip.extractall(artifacts)
                        if path.exists(outdir):
                            shutil.rmtree(outdir)
                        print(f"rename( {path.join(artifacts, rootdir)} --> {outdir} )")
                        os.rename(path.join(artifacts, rootdir), outdir)

    if count < 4:
        raise Exception(f"found {count}/4 distribution directories")

    # # remove files
    # for dir, filere in [
    #     # remove ELF files
    #     [ubuntu_x86_dir, r'^makensis$'],   [ubuntu_x86_dir, r'^GenPat$'],
    #     [ubuntu_amd64_dir, r'^makensis$'], [ubuntu_amd64_dir, r'^GenPat$'],
    #     ]:
    #     for file in listdir(dir):
    #         filepath = path.join(dir, file)
    #         if re.match(filere, file) != None and path.isfile(filepath):
    #             print(f"remove( {filepath} )")
    #             os.remove(filepath)

    # copy files
    for srcdir, srcre, dstdir in [
        # copy Bin\makensisw.exe to root
        [path.join(ubuntu_x86_dir, 'Bin'), r'makensisw\.exe', ubuntu_x86_dir],
        [path.join(ubuntu_amd64_dir, 'Bin'), r'makensisw\.exe', ubuntu_amd64_dir],
        # copy missing root *.exe and *.chm from windows to ubuntu
        [windows_x86_dir, r'^.*\.exe$', ubuntu_x86_dir],
        [windows_x86_dir, r'^.*\.chm$', ubuntu_x86_dir],
        [windows_amd64_dir, r'^.*\.exe$', ubuntu_amd64_dir],
        [windows_amd64_dir, r'^.*\.chm$', ubuntu_amd64_dir],
        # copy missing Bin\*.exe from windows to ubuntu
        [path.join(windows_x86_dir, 'Bin'), r'^.*\.exe$', path.join(ubuntu_x86_dir, 'Bin')],
        [path.join(windows_amd64_dir, 'Bin'), r'^.*\.exe$', path.join(ubuntu_amd64_dir, 'Bin')],
        # merge x86 and amd64 plugins
        [path.join(ubuntu_x86_dir, 'Plugins', 'x86-ansi'), r'.*', path.join(ubuntu_amd64_dir, 'Plugins', 'x86-ansi')],
        [path.join(ubuntu_x86_dir, 'Plugins', 'x86-unicode'), r'.*', path.join(ubuntu_amd64_dir, 'Plugins', 'x86-unicode')],
        [path.join(ubuntu_amd64_dir, 'Plugins', 'amd64-unicode'), r'.*', path.join(ubuntu_x86_dir, 'Plugins', 'amd64-unicode')],
        # merge x86 and amd64 stubs
        [path.join(ubuntu_x86_dir, 'Stubs'), r'^.+-x86-ansi$', path.join(ubuntu_amd64_dir, 'Stubs')],
        [path.join(ubuntu_x86_dir, 'Stubs'), r'^.+-x86-unicode$', path.join(ubuntu_amd64_dir, 'Stubs')],
        [path.join(ubuntu_amd64_dir, 'Stubs'), r'^.+-amd64-unicode$', path.join(ubuntu_x86_dir, 'Stubs')],
        # merge x86 and amd64 Bin\RegTool-*.bin
        [path.join(ubuntu_x86_dir, 'Bin'), r'RegTool-x86\.bin', path.join(ubuntu_amd64_dir, 'Bin')],
        [path.join(ubuntu_amd64_dir, 'Bin'), r'RegTool-amd64\.bin', path.join(ubuntu_x86_dir, 'Bin')],
        ]:
        for file in listdir(srcdir):
            srcfile = path.join(srcdir, file)
            dstfile = path.join(dstdir, file)
            if re.match(srcre, file) != None and path.isfile(srcfile):
                if not path.exists(dstfile):
                    os.makedirs(dstdir, exist_ok=True)
                    print(f"copy2( {srcfile} --> {dstfile} )")
                    shutil.copy2(srcfile, dstfile)

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-a", "--artifacts-dir", type=str, default=path.join(path.dirname(__file__), 'artifacts'))
    parser.add_argument("-o", "--output-dir", type=str, default=path.dirname(__file__))
    args = parser.parse_args()
    build_package(args.artifacts_dir,  args.output_dir)
