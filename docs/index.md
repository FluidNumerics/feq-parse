# FEQParse


## Quickstart

### Install with Fortran Package Manager (fpm)

1. [Download and install Fortran Package Manager (fpm)](https://fpm.fortran-lang.org/install/index.html)
2. Clone `feq-parse`
```
git clone https://github.com/fluidnumerics/feq-parse ~/feq-parse
```
3. Install
```
cd ~/feq-parse
fpm build --profile release
fpm test --profile release
```

### Install with Spack

1. Download and set up the [Spack package manager](https://spack.io)
```
git clone https://github.com/spack/spack ~/spack
source ~/spack/share/spack/setup-env.sh
spack compiler find
```
2. Install `feq-parse`
```
spack install feq-parse
```


## Learn more

* [Building your applications with feq-parse](./build-with-feqparse.md)
* [Learn more about the feq-parse algorithms](./parsing-equations.md)
