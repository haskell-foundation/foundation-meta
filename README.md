# Disclaimer

This repository is still **Work In Progress**. The aim being setup
automatic generation of the Foundation's meta data. Because this is still
a **WIP**, it is possible that statements made in this README.md are not
yet implemented, see [TODO](#todo) for more information.

# Foundation META

Foundation META contains all the meta data associated to the foundation
libraries:

* the [Foundation Web Site](https://haskell-foundation.github.io)'s Hakyll
  sources;
* the benchmark results of: unstable, stable and past releases;
* the documentation of: unstable, stable and the past releases.

This repository also provides a useful development tool base on
[Shake](http://shakebuild.com) to run the tests and benchmark of Foundation as
you develop it.

## Installation:

Requirements:

* using [stack](https://haskellstack.org) (recommended);
* install [Shake](http://shakebuild.com) (`stack install shake`);
* install [open-browser](http://hackage.haskell.org/package/open-browser)
  (`stack install open-browser`)

Compile the `ShakeFile`:

```bash
stack ghc -- ShakeFile.hs
```

## Usage

The [ShakeFile](ShakeFile.hs) provides two different modes.

* `--foundation path/to/foundation`: set the source directory to the foundation
  sources. If not set, the repository will be cloned;
* `--release`: run the build on release mode.
* `--open`: open the generated meta in browser.

### Development

This is the default mode of the [ShakeFile](ShakeFile.hs). Simply provides the
path to the **foundation sources** you are working on (otherwise,
[ShakeFile](ShakeFile.hs) will clone it for you in the `shake_build` directory.)

One of the most common use case is to generate the benchs of the source you are
working on:

```bash
# build the ShakeFile
stack ghc -- ShakeFile.hs
# run the benchs and open the result in the browser
./ShakeFile --foundation <path/to/source> --open benchs
```

### Release (--release)

To only use for publishing. **WIP**

# TODO

* [ ] automatic publication of the Foundation web site by Travis;
* [ ] generation and publication of the documentation:
  * [ ] the documentation is not yet well generated;
  * [ ] Hakyll needs to be aware of the documentations tree (different versions);
* [ ] generation and publication of the benchmark:
  * [ ] being able to compare 2 different version of foundation;
  * [ ] publication on the Hakyll website.
* [ ] in devel mode, give option to open the html result in browser.
