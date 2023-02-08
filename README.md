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
cmake .. --fresh -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja tests
ctest --test-dir tests
```
