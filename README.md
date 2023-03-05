## SlowJS

Learning the awesome [QuickJS](https://github.com/bellard/quickjs), QuickJS is quick but I'll make it slow

## Development

It's better to glance over the available options before you perform the actual build:

```bash
cmake -B build -LH
```

> - `-B` stands for the building directory
> - `-L` stands for listing all the options
> - `-H` stands for printing the help messages along with the options

above command will print the available options and their help messages, use them like this:

```bash
cmake -B build -S . -G Ninja -D QJS_DUMP_BYTECODE=1
```

> - `-S` stands for the source directory
> - `-D` stands for specifying an options in a `key=value` pattern

then choose one of below sections to run in project root directory

#### Debug build

```bash
cmake -S . --preset=default
cmake --build --preset=qjs
```

#### Release build

```bash
cmake -S . --preset=default -D CMAKE_BUILD_TYPE=Release
cmake --build --preset=qjs
```

#### Tests

```bash
cmake -S . --preset=default
cmake --build --preset=run-tests
```

#### Microbench

```bash
cmake -S . --preset=default -D CMAKE_BUILD_TYPE=Release
cmake --build --preset=run-microbench
```

#### Test262

```bash
cmake -S . --preset=default -D CMAKE_BUILD_TYPE=Release
cmake --build --preset=run-test262

# Result: 302/75790 errors, 1396 excluded, 7712 skipped, 302 new
```

### Presets

You can also choose the presets listed in `CMakePresets.json` to run:

```bash
# Use a config preset
cmake -S . --preset=default

# Use a build preset
cmake --build --preset=run-tests
```
