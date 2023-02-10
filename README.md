## Development

### Building

First, in project root directory create a directory to handle the building stuffs:

```bash
mkdir build
```

then in the `build` directory, choose one of the command groups listed in below sections to run.

Before you perform the actual build it's better to glance over the available options:

```bash
cd build && cmake -LH ..
```

above command will print the available options and their help message, use them like this:

```bash
cmake .. --fresh -G Ninja -DQJS_DUMP_BYTECODE=1
```

#### Debug build

```bash
cmake .. --fresh -G Ninja
ninja qjs
```

#### Release build

```bash
cmake .. --fresh -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja tests
```

#### Tests

```bash
cmake .. --fresh -G Ninja
ninja tests
ctest --test-dir tests
```

#### Microbench

```bash
cmake .. --fresh -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja microbench
```

#### Test262

```bash
cmake .. --fresh -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja run-test262-test2

# Result: 302/75790 errors, 1396 excluded, 7712 skipped, 302 new
```
