## Development

### Building

First, in project root directory create a directory to handle the building stuffs:

```bash
mkdir build
```

then choose one of below command groups to run in the `build` directory

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
# test the release
# cmake .. --fresh -G Ninja -DCMAKE_BUILD_TYPE=Release
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
