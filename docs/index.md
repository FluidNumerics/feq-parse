# FEQParse


## Quickstart

<details markdown="1"><summary>Install with Fortran Package Manager (fpm)</summary>

* [Download and install Fortran Package Manager (fpm)](https://fpm.fortran-lang.org/install/index.html)

* Clone `feq-parse`
```
git clone https://github.com/fluidnumerics/feq-parse ~/feq-parse
```

* Install
```
cd ~/feq-parse
fpm build --profile release
fpm test --profile release
```
</details>

<details markdown="1"><summary>Install with Spack</summary>

* Download and set up the [Spack package manager](https://spack.io)
```
git clone https://github.com/spack/spack ~/spack
source ~/spack/share/spack/setup-env.sh
spack compiler find
```

* Install `feq-parse`
```
spack install feq-parse
```
</details>

<details markdown="1"><summary>Install with CMake</summary>

* Clone `feq-parse`
```
git clone https://github.com/fluidnumerics/feq-parse ~/feq-parse
```

* Install
```
mkdir ~/feq-parse/build
cd ~/feq-parse/build
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${HOME}/apps/feq-parse ../
make
make install
```
</details>
## Learn more

* [Building your applications with feq-parse](./build-with-feqparse.md)
* [Learn more about the feq-parse algorithms](./parsing-equations.md)
