# Tests for the hal-bridge install / status / discovery surface in R/bridge.R
#
# These exercise everything that doesn't require a live HTTP bridge: env
# detection, port-file parsing, token-ladder env path, Positron CLI lookup,
# version pinning. The HTTP-streaming side (prompt, /models, /version) is
# covered by the live smoke test in dev/test_vscode_bridge.R.

test_that("BRIDGE_VERSION is a non-empty string", {
  expect_true(nzchar(hal:::BRIDGE_VERSION))
})

test_that("bundled VSIX ships with the package", {
  path <- system.file(
    sprintf("extdata/hal-bridge-%s.vsix", hal:::BRIDGE_VERSION),
    package = "hal"
  )
  expect_true(nzchar(path) && file.exists(path))
})

test_that(".is_positron reflects POSITRON_VERSION", {
  withr::with_envvar(c(POSITRON_VERSION = "1.0"), {
    expect_true(hal:::.is_positron())
  })
  withr::with_envvar(c(POSITRON_VERSION = ""), {
    expect_false(hal:::.is_positron())
  })
})

# Shared helpers .bridge_env() / .write_port_file() live in helper-bridge.R.

test_that(".hal_bridge_port_file returns a path ending in hal-bridge/port.json", {
  path <- hal:::.hal_bridge_port_file()
  expect_type(path, "character")
  expect_match(path, "hal-bridge[/\\\\]port\\.json$")
})

test_that(".hal_bridge_discover errors when port file is missing", {
  td <- withr::local_tempdir()
  withr::with_envvar(.bridge_env(td), {
    expect_error(
      hal:::.hal_bridge_discover(),
      "hal-bridge port file not found"
    )
  })
})

test_that(".hal_bridge_discover parses a valid port file", {
  td <- withr::local_tempdir()
  withr::with_envvar(.bridge_env(td), {
    .write_port_file(list(port = 12345L, pid = 99L, version = "0.1.0",
                          started = "2026-01-01T00:00:00Z"))
    info <- hal:::.hal_bridge_discover()
    expect_equal(info$port, 12345L)
    expect_equal(info$version, "0.1.0")
    expect_equal(info$pid, 99L)
  })
})

test_that(".hal_bridge_discover errors on malformed port file", {
  td <- withr::local_tempdir()
  withr::with_envvar(.bridge_env(td), {
    .write_port_file("this is not json")
    expect_error(hal:::.hal_bridge_discover(), "Failed to parse")
  })
})

test_that(".hal_bridge_discover errors when port field missing", {
  td <- withr::local_tempdir()
  withr::with_envvar(.bridge_env(td), {
    .write_port_file(list(version = "0.1.0"))
    expect_error(hal:::.hal_bridge_discover(), "malformed")
  })
})

test_that("hal_bridge_status returns FALSE silently when port file missing", {
  td <- withr::local_tempdir()
  withr::with_envvar(.bridge_env(td), {
    expect_message(out <- hal_bridge_status(), "not found")
    expect_false(out)
  })
})

test_that("hal_bridge_status returns FALSE on malformed port file", {
  td <- withr::local_tempdir()
  withr::with_envvar(.bridge_env(td), {
    .write_port_file("not json")
    expect_message(out <- hal_bridge_status(), "malformed")
    expect_false(out)
  })
})

test_that(".find_positron_cli honors POSITRON_BIN override", {
  # Use an existing file so the function returns rather than aborts. R's own
  # executable is always present and the function only requires existence.
  fake_positron <- file.path(R.home("bin"),
                              if (.Platform$OS.type == "windows")
                                "R.exe" else "R")
  skip_if_not(file.exists(fake_positron),
              "no R binary to use as fake positron path")
  withr::with_envvar(c(POSITRON_BIN = fake_positron), {
    found <- hal:::.find_positron_cli()
    expect_true(file.exists(found))
  })
})

test_that(".find_positron_cli aborts when nothing is found", {
  # Empty PATH + bogus override -> no positron anywhere.
  withr::with_envvar(c(POSITRON_BIN = "", PATH = ""), {
    # On Windows, Sys.which still inspects WindowsApps; the fallback list of
    # platform paths may still exist on this dev box. Skip if so.
    if (nzchar(Sys.which(if (.Platform$OS.type == "windows")
                          "positron.cmd" else "positron"))) {
      skip("positron is on PATH after env scrub; cannot test miss path")
    }
    pf_exists <- file.exists(file.path(Sys.getenv("ProgramFiles", ""),
                                        "Positron", "bin", "positron.cmd"))
    if (pf_exists) skip("Positron present in ProgramFiles; cannot test miss")
    expect_error(hal:::.find_positron_cli(), "Cannot locate")
  })
})
