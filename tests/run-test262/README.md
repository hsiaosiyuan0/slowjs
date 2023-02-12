1. Clone test262:

```bash
# under project root directory
./tests/run-test262/setup_test262.sh
```

2. Make a build directory to handle the building stuffs

```bash
# under project root directory
mkdir build
```

3. Generate the config used by the building tools

```bash
# under directory `build` created by step 2
cmake -B build -S . -G Ninja
```

4. Run the test262

```bash
# under directory `build` created by step 2
cmake --build build --target run-test262-test2-default
```

5. Or run the test262 in strict and nostrict modes

```bash
# under directory `build` created by step 2
cmake --build build --target run-test262-test2
```