## Development

First of all, you need to create a directory in project root directory to handle the building stuffs:

```bash
mkdir build
```

### Setup vcpkg

First, install vcpkg by following its [official tutorial](https://vcpkg.io/en/getting-started.html)

then run below command to integrate vcpkg with cmake:

```bash
cmake -B build -S . -G Ninja -D CMAKE_TOOLCHAIN_FILE=$(dirname `which vcpkg`)/scripts/buildsystems/vcpkg.cmake
```

### Building

Before you perform the actual build it's better to glance over the available options:

```bash
cmake -B build -LH
```

> - `-B` stands for the building directory
> - `-L` stands for listing all the options
> - `-H` stands for printing the help messages along with the options

above command will print the available options and their help message, use them like this:

```bash
cmake -B build -S . -G Ninja -D QJS_DUMP_BYTECODE=1
```

> - `-S` stands for the source directory
> - `-D` stands for defining the options in a `key=value` pattern

then choose one of below sections to run in project root directory

#### Debug build

```bash
cmake -B build -S . -G Ninja
cmake --build build
```

#### Release build

```bash
cmake -B build -S . -G Ninja -D CMAKE_BUILD_TYPE=Release
cmake --build build
```

#### Tests

```bash
cmake -B build -S . -G Ninja
cmake --build build --target tests
ctest --test-dir build/tests
```

#### Microbench

```bash
cmake -B build -S . -G Ninja -D CMAKE_BUILD_TYPE=Release
cmake --build build --target microbench
```

#### Test262

```bash
cmake -B build -S . -G Ninja -D CMAKE_BUILD_TYPE=Release
cmake --build build --target run-test262-test2

# Result: 302/75790 errors, 1396 excluded, 7712 skipped, 302 new
```
