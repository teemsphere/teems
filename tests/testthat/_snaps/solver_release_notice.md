# a newer release of the same major version points to the rebuild

    Code
      newer <- .solver_release_notice("teems:latest", "1.1.0", call = NULL)
    Message
      i teems-solver 1.2.0 is available at <https://github.com/teemsphere/teems-solver/releases/tag/v1.2.0>; the "teems:latest" image carries 1.1.0.
      i Rebuild the image from the current teems-solver repository: `git pull`, then the expedited `docker build --pull` with your libHSL tarball (<https://github.com/teemsphere/teems-solver#updating-an-existing-installation>). Then restart R.

# a newer release of the next major version asks for the package first

    Code
      newer <- .solver_release_notice("teems:latest", "1.2.0", call = NULL)
    Message
      i teems-solver 2.0.0 is available at <https://github.com/teemsphere/teems-solver/releases>; the "teems:latest" image carries 1.2.0.
      i That release is a 2.x interface and this teems version drives 1.x: update the teems package first, then rebuild the image (<https://github.com/teemsphere/teems-solver#updating-an-existing-installation>).

