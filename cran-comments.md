# cran-comments

## Submission

This is a new submission (hal 0.1.4).

## Test environments

- local: Windows 11, R 4.x (release)
- GitHub Actions: ubuntu-latest (R release), ubuntu-latest (R devel),
  ubuntu-latest (R oldrel-1), windows-latest (R release),
  macos-latest (R release)
- win-builder: R devel (run before submission)

## R CMD check results

0 errors | 0 warnings | 0 notes

(Only the expected "New submission" NOTE on CRAN incoming checks.)

## Notes for reviewers

- **External CLIs are optional runtime dependencies, not build/check
  dependencies.** hal is an interface to locally installed coding-agent
  command line interfaces ('GitHub Copilot', 'Claude Code') and to the
  'Positron' IDE's language-model bridge. None are required to install,
  check, or test the package: the test suite runs fully offline against
  mock CLIs implemented in R (shipped in `inst/mock-cli/`), and tests
  that spawn mock subprocesses are additionally `skip_on_cran()`.
  Examples that would invoke a real CLI or model are wrapped in
  `\dontrun{}` for the same reason (they require third-party tools,
  authentication, and network access, and would incur costs).

- **`inst/extdata/hal-bridge-0.1.4.vsix` (~9 KB)** is a small editor
  extension (zip archive of JavaScript) that ships with the package.
  `hal_install_bridge()` installs it into the 'Positron' IDE **only when
  the user explicitly calls that function** in an interactive setup flow.
  Nothing is installed at package install/load time. It contains no
  compiled code: the extension is plain, commented JavaScript
  (`extension/out/extension.js`) plus its manifest, readme, and license.
  Its source is maintained at <https://github.com/ArcLite-Red/hal-bridge>
  under the MIT license, same copyright holder as this package. The bundled
  file is byte-identical to the v0.1.4 release asset built from that
  source by the repository's release workflow
  (<https://github.com/ArcLite-Red/hal-bridge/releases/tag/v0.1.4>);
  SHA-256 `316cc915cb6ae16a923f1bcf9a0b0f04bfc11478ce1ff36301c03a86aeb661f0`.

- **File writes**: the package writes only to `tempfile()`/`tempdir()`
  during checks and tests. The one runtime exception is an optional
  per-project memory file (`hal.md`) in the working directory, which falls
  under the policy's interactive-session exception: hal **asks the user for
  confirmation** (`utils::askYesNo()`, default "no") and writes only on an
  explicit yes. It never asks or writes in a non-interactive session, asks
  at most once per folder per R session, and the offer can be switched off
  entirely with `options(hal.memory_prompt = FALSE)`. An existing file is
  read but never overwritten.

- **Subprocesses**: runtime operation spawns the user's locally installed
  CLI via 'processx'. On CRAN machines no subprocess is ever spawned
  (guards described above).
