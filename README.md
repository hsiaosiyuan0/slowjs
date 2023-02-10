## Development

### Building

First, in project root directory create a directory to handle the building stuffs:

```bash
mkdir build
```

then keep in project root directory and choose one of below sections to run.

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
