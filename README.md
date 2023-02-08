## Development

Debug build:

```bash
mkdir build
cd build
cmake .. --fresh -G Ninja
ninja qjs
```

Release build:

```bash
mkdir build
cd build
cmake .. --fresh -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja tests
```

Tests:

```bash
mkdir build
cd build
# test the release
# cmake .. --fresh -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake .. --fresh -G Ninja
ninja tests
ctest --test-dir tests
```

Microbench:

```bash
mkdir build
cd build
cmake .. --fresh -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja microbench
```
