# GSI-Utils
GSI Utility Tools

These are GSI utilities for various functions.

## Installation (see instructions [here](./INSTALL.md))

### Environment: 

Use the build_hera_intel.env (or build_jet_intel.env) from RRFS (ufs-srweather-app/env/build_hera_intel.env).
Also note that you may have to load the modules from ufs-srweather-app/env/srw_common separately. This
environment uses the `rrfs-env` alias on Jet and Hera.

### Compilation Steps:

- Clone this repository and cd into GSI-utils. 

- Create a new directory called "build" and cd into that directory.

- Load the environment described above.

- Run CMake with the following command: `cmake -DCMAKE_INSTALL_PREFIX=/path/to/GSI-utils /path/to/GSI-utils`

- Run the command `make`

- Run the command `make install`

- There should now be a bunch of executables (*.x) files in GSI-utils/bin
