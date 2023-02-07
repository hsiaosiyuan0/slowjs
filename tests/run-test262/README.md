1. clone test262:

```bash
# under project root directory
./tests/run-test262/setup_test262.sh
```

2. make a build directory to handle the building stuffs

```bash
# under project root directory
mkdir build
```

3. generate the config used by the building tools

```bash
# under directory `build` created by step 2
cmake .. --fresh -G Ninja
```

4. run the test262

```bash
# under directory `build` created by step 2
ninja run-test262-test2-default
```

5. run the test262 in strict and nostrict modes

```bash
# under directory `build` created by step 2
ninja run-test262-test2
```