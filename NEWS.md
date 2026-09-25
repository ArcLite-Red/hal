# hal 0.1.4

* **Plot vision: the model can now see your plots.** When `eval_r` code
  draws a plot -- base graphics or a returned ggplot/lattice object -- hal
  captures it as PNG and attaches it to the tool result as an image, so
  the model can describe, critique, and iterate on actual visual output
  ("make this plot publication-ready" now works on what the plot *looks
  like*). Supported on the **vscode** backend (requires the bundled
  hal-bridge 0.1.4) and the **claude** backend (MCP image content blocks).
  The **copilot** backend stays text-only until its CLI's image forwarding
  is verified. Disable with `hal_configure(plot_vision = FALSE)`.
  Notes: returned ggplot objects are also printed to your device (they
  now appear in the Positron plots pane); render errors surface to the
  model as a `## Plot render error` section; plots written to file
  devices the code opens itself are not echoed; one image per eval (the
  final page); images larger than 5 MB are skipped.

* **hal-bridge 0.1.4** (bundled): tool results can carry an image part
  (`LanguageModelDataPart.image`); if the selected model rejects image
  input at runtime, the bridge strips images and retries once text-only;
  request bodies are capped at 20 MB (413 beyond); minimum Positron/VS
  Code engine raised to 1.103 (first stable release with the image API).
  The vscode client also prunes all but the newest 2 images from the
  resent history so long sessions don't re-bill every old plot.

* **`hal_do()` now verifies its transforms.** After a successful pipe-mode
  transform, hal compares input and output and prints a one-line
  structural report -- row deltas, columns added/removed, class changes,
  introduced NAs (`hal_do: 32 -> 14 rows | +1 col (kpl)`). The full
  report is attached as `attr(result, "hal_verify")` (print it for
  details). Suspicious patterns -- output identical to input, or 0-row
  output -- raise a classed `hal_do_warning`. Report-only by design:
  deltas are often intentional ("filter mpg > 20" *should* shrink the
  data), so verification never triggers retries or changes values.
  Disable with `.verify = FALSE` or `hal_configure(verify = FALSE)`.

* **Fixed: the default model on the copilot and vscode backends no longer
  exists.** Both defaulted to `"claude-sonnet-4.6"`, which GitHub Copilot and
  `vscode.lm` have since dropped from their catalogs — neither backend serves
  that id any more. Both now default to `"claude-sonnet-5"`, verified present
  in both live catalogs. The claude backend was unaffected: it passes the
  `"sonnet"` alias, which the CLI resolves server-side and which cannot go
  stale this way.

* **Fixed: stale model metadata.** The Claude backend's alias descriptions
  still named Sonnet 4.6 and Opus 4.8 as the current tiers (now Sonnet 5 and
  Opus 5), and reported a 200K context window for all three aliases — Sonnet
  and Opus are 1M natively, and only Haiku is 200K. Context windows for the
  newer Copilot/`vscode.lm` ids (`claude-opus-4.8`, `claude-opus-5`,
  `claude-sonnet-5`) were missing and reported as `NA`. Documentation examples
  naming retired ids were updated.

* **Missing Copilot? hal now points you at Claude if you have it.** When hal
  falls back to the copilot backend and can't find the Copilot CLI, but Claude
  Code is installed, the error, `hal_status()`, and `hal_setup()` now say so
  and give the one command that gets you working:
  `hal_configure(backend = "claude")`. The install instructions are still
  shown. Default backends are unchanged — this only changes the message in a
  case that already failed. Detection checks the `PATH` and
  `CLAUDE_CLI_PATH` only; it never runs a subprocess.

* **Fixed: slow `eval_r` calls could abort a healthy Claude turn.** Tool
  calls execute synchronously inside the Claude client's response loop, but
  the prompt deadline only reset when the CLI produced output -- so time
  spent running your R code counted as the model stalling. A single slow
  eval (`eval_timeout` defaults to 30s, roughly double that when plot vision
  re-renders) could exhaust the 60s `prompt_timeout` and abort a turn that
  was progressing normally. Time spent servicing tool calls is now credited
  back to the deadline.

* **Fixed: a failed Claude turn wedged the session.** The client marked a
  session as created only after a *successful* turn. If a turn timed out or
  the CLI died mid-stream, the session already existed in Claude Code's
  store but hal did not know it -- so every subsequent prompt re-sent
  `--session-id` with a live uuid and was rejected, until `hal_reset()`.
  The session is now latched as soon as the CLI announces it, so a failed
  turn is simply retried with `--resume`. Timeout messages say so.

* **Fixed: `hal_setup(backend = "claude")` installed the Copilot CLI.** The
  documented `"claude"` value had no branch in `hal_setup()`, so it fell
  through to the Copilot path and either mislabelled Claude as Copilot or
  ran `npm install -g @github/copilot`. There is now a real Claude branch
  that verifies the `claude` binary, reports its version, and points at the
  download plus one-time OAuth step. It deliberately does not auto-install:
  Claude Code's sign-in is interactive, and an npm install produces exactly
  the `.cmd` shim whose stdio processx drops on Windows.

* **Fixed: MCP subprocess crash on R 4.1--4.3.** The generated MCP server
  script used `%||%` without defining it; base R only gained `%||%` in
  4.4.0. The script now defines it, matching the package's declared
  R >= 4.1 support.

* **Fixed: blank destination in the first-run data notice on the vscode
  backend.** The one-time "hal sends prompts and tool results to the ..."
  notice had no vscode branch and rendered an empty slot; it now names
  the model behind your Positron Copilot sign-in.

* **CRAN preparation.**
  - **`hal()` now asks before creating `hal.md`.** Previously it wrote the
    project memory file into your working directory on first use and told
    you afterwards. It now offers — "Create hal.md?", default no — and writes
    only on an explicit yes. It never asks in a non-interactive session,
    asks at most once per folder per R session, and a decline is not undone
    by `hal_reset()`. Turn the offer off with
    `options(hal.memory_prompt = FALSE)`. An existing `hal.md` is still read
    in any context, whatever you answered.
  - Removed the never-shipped `hal_query_map` surface: the
    `hal_configure(max_spawn =)` parameter and `hal.max_spawn` option
    documented a function that does not exist.
  - Mock-CLI tests (which spawn local `Rscript` subprocesses) are
    skipped on CRAN; the rest of the suite runs there.
  - `DESCRIPTION` rewritten per CRAN conventions; stale `LICENSE`
    copyright holder fixed; `.vscode/` excluded from the build.

# hal 0.1.3

* **`excelR()` is now `hal_excel()`.** Clean rename (no alias) to match the
  `hal_` prefix convention before anyone depends on it. The result class is
  `hal_excel_code` and the verification report attribute is
  `attr(x, "hal_excel")`. Functionality is unchanged.

* **New `hal_status()`: one traffic-light diagnostic for the whole stack.**
  Reports the resolved backend (and why), whether its transport is
  reachable, session state, and — when something is wrong — the single next
  step to fix it. Returns a structured list invisibly for programmatic use.
  This is the first thing to run when hal misbehaves, and the thing to
  paste into a bug report.

* **Positron setup no longer requires the GitHub CLI.** `hal_setup()` was
  still gating the vscode path on `gh` being installed and `gh auth status`
  passing — a leftover from before the bridge VSIX was bundled (0.1.1).
  The gate is gone; the only external requirement (a Copilot sign-in
  inside Positron itself) is now surfaced as a reminder in the next-step
  bullets, since hal cannot verify it from R.

* **`hal_do()` now aborts on failure in non-interactive contexts.**
  Previously a failed generation warned and passed `.data` through
  unchanged everywhere — in a script or R Markdown pipeline that means
  silently continuing with untransformed data. Interactive sessions keep
  the forgiving warn-and-passthrough; scripts get an error. Override
  either way with `hal_configure(do_on_fail = "warn"|"abort")`.

* **Default `hal_do()` retries raised from 1 to 2** (`hal.do_retries`),
  matching the default-on edit-in-place behavior: more self-correction
  before giving up. `hal_excel()` inherits the same default.

* **hal failures now signal classed conditions.** `hal()` transport
  failures abort with `hal_transport_error` in non-interactive contexts;
  `hal_do()` signals `hal_do_error` / `hal_do_warning`; `hal_ask()` signals
  `hal_ask_warning`. All inherit from `hal_error` / `hal_warning`, so
  programmatic callers can `tryCatch(..., hal_error = ...)` instead of
  string-matching messages.

* **Environment auto-detect is harder to false-positive.** `hal()`'s
  `use_env = NULL` auto-detection no longer injects your environment when
  a prompt merely contains English function words that collide with object
  names ("show me **all** the columns **on** that table" with objects
  `all`/`on`). Backtick-quoting, `$`/`[` subsetting, or calling the object
  still triggers injection deliberately. Data-science nouns (`data`,
  `model`, `fit`, ...) still match by name — missing real context costs
  more than a small extra snapshot.

* **Website URL fixed.** `DESCRIPTION`, `_pkgdown.yml`, and the README
  pointed at `d-m4rk.github.io/hal` (404); the site deploys at
  `arclite-red.github.io/hal`.

# hal 0.1.2

* **hal-bridge discovery file moved out of the system temp dir (bridge
  0.1.2).** The bridge previously wrote its port + token to
  `%TEMP%\hal-bridge.port`, which the OS garbage-collects (e.g. Windows
  Storage Sense). After a day or two the file was swept while the bridge
  was still listening, so `hal_bridge_status()` reported the bridge as
  gone and users reinstalled needlessly. The discovery file now lives in a
  durable per-user app-data dir — `%LOCALAPPDATA%\hal-bridge\port.json` on
  Windows, `$XDG_RUNTIME_DIR/hal-bridge/port.json` (else
  `~/.cache/hal-bridge/port.json`) on POSIX. Requires the bundled bridge
  0.1.2; older bridges write the old location and won't be found —
  reinstall with `hal_install_bridge(force = TRUE)` and cold-restart
  Positron.

* **`hal_config()$backend` now reports the resolved backend, not the raw
  option.** Previously it read `getOption("hal.backend", "copilot")` with a
  hardcoded literal fallback, so in Positron it lied and said `"copilot"`
  even though `hal()` would actually route to `"vscode"`. Now uses
  `.hal_backend()` — the same resolver `hal()` uses — so the reporter and
  the router agree.

* **Bridge install messaging now tells users to fully quit Positron**,
  not just "Reload Window". On a fresh extension install the extension
  host only loads new extensions on a cold start; "Reload Window" is
  insufficient and leaves users stuck at `hal_bridge_status()` reporting
  the bridge as not installed. Updated in: `hal_install_bridge()` success
  message, `.hal_bridge_discover()` not-found error, `hal_setup()` next-
  step bullets, and the `backends` vignette install snippet. Runtime
  reload prompts (crashed bridge, 401 token rotation) still say "reload
  Positron" since Reload Window is sufficient there.

# hal 0.1.1

* **`hal_install_bridge()` now installs from a bundled VSIX.** The hal-bridge
  extension (7 KB) ships in `inst/extdata/` and is installed directly into
  Positron — no GitHub download, no auth token, no SHA pin. Removes the
  install-time `gh auth login` / `GITHUB_PAT` requirement that broke fresh
  installs on machines without a token. `local_path = "..."` still works for
  testing dev builds; `version` and `verify` arguments are gone (no longer
  meaningful). To bump the bridge, ship a new hal release with the updated
  VSIX in `inst/extdata/`.

* Internal: deleted `.hal_download_bridge_vsix`, `.hal_github_token`,
  `.hal_github_api_get`, `.hal_github_download_asset`, `.hal_sha256_file`,
  and the `BRIDGE_SHA256` / `BRIDGE_REPO` constants. Dropped `digest` and
  `openssl` from `Suggests`.

# hal 0.1.0

Initial public release.

* **New `vscode` backend** -- talks to Positron's built-in `vscode.lm` API
  via a small localhost HTTP bridge (the `hal-bridge` Positron extension).
  Skips the Copilot CLI entirely: no Node.js, no `@github/copilot`
  package, no `--additional-mcp-config` plumbing. Tool calls round-trip
  directly through R, so `eval_r` runs without an MCP subprocess.
  - `hal_install_bridge()` -- downloads the pinned VSIX from the
    `hal-bridge` private GitHub release and installs it into Positron.
    GitHub auth comes from `gh auth token`, git credential helper, or
    `GITHUB_PAT` (in that order); no separate token setup needed for
    most users.
  - `hal_bridge_status()` -- ping/version/port diagnostics.
  - `hal_setup()` now detects Positron and walks through the bridge
    install path automatically; pass `backend = "copilot"` to force the
    legacy CLI flow.
  - `hal_models()` queries the bridge's `/models` endpoint and returns
    whatever `vscode.lm` exposes to your Copilot session.
  - `hal_available()` is backend-aware: takes `backend = "..."` to probe
    a specific transport without changing the session default.
  - `permission_policy` is now honored on the vscode backend (previously
    accepted for API parity but silently always auto-allowed). Supports
    `"auto-allow"` (default), `"auto-deny"`, `"ask"` (interactive prompt;
    denies in non-interactive sessions), or a function receiving
    `list(backend = "vscode", tool_name, input)` and returning a string
    containing `"allow"` or `"deny"`. Denied calls are surfaced to the
    model as a tool error and recorded with `status = "denied"`.

* **Smart-default backend** -- when `hal.backend` is unset, hal now
  resolves to `"vscode"` inside Positron (`POSITRON_VERSION` set) and
  `"copilot"` everywhere else. Existing users with
  `options(hal.backend = "...")` are unaffected.

* **BREAKING:** Removed the blackboard API (`hal_bb_put()`, `hal_bb_get()`,
  `hal_bb_list()`, `hal_bb_rm()`, `hal_bb_clear()`, `hal_bb()`). Replaced
  with `use_env` parameter on `hal()` that auto-injects the caller's
  environment snapshot so the model uses `eval_r` directly on your objects.
  No manual staging needed — `hal("analyze df", use_env = TRUE)` just works.

* **BREAKING:** Removed `allow_eval` parameter from `hal()` and `hal_ask()`.
  `eval_r` tool is now always registered at session init. It is inert
  without the `use_env` system prompt nudge. `write_r_script` removed
  (redundant with the SDK's built-in `create` tool).

* Bidirectional IPC: `eval_r` tool calls execute in the user's live R
  session via file-based message passing between the MCP subprocess and
  parent R session. Model can inspect and modify caller environment objects.

* `hal_do()` retry mechanism: automatic retry on code parse/execution
  failure, model self-corrects within the same disposable session.
  Configurable via `hal_configure(do_retries = N)`.

* Enhanced environment descriptions in `hal_do()` standalone mode:
  data frame column names/types, function bodies for small functions.

* `hal_configure()` now accepts `permission_policy` and `session_quiet`
  for controlling the global session without using the R6 API.

* `hal_chat()` and `hal_client()` wrappers now accept `permission_policy`
  and `quiet` directly.

* `hal_setup()` -- guided CLI installation and authentication helper.
  Checks for Node.js, installs the Copilot CLI via npm, and walks
  through `copilot login`.

* Full functional API: `hal()`, `hal_ask()`, `hal_do()`, `hal_history()`,
  `hal_reset()`.

* Pipe-friendly verbs: `hal_ask()` for data-aware analysis,
  `hal_do()` for code generation (pipe and standalone modes).

* Edit-in-place: `hal_do()` replaces itself in the editor with generated
  code when running from an IDE script.

* Configuration via `hal_configure()` / `hal_config()` with `hal.*` options.

* Custom tool calling via MCP: `hal_tool()`, `hal_register_tool()`,
  `hal_register_package_tools()`, `hal_register_tool_specs()`.

* Governance layer: eval_r denylist (AST walker), credential scanner,
  eval timeout, spawn cap.

* ANSI-colored output formatting with optional typewriter streaming.

* High-level chat API via `HalChat` R6 class, mirroring the
  `ellmer::Chat` interface.

* Low-level ACP transport via `HalClient` R6 class (NDJSON over stdio).

* S3 data objects: `hal_response`, `hal_turn`, `hal_tool_call` with
  print/format methods.

* Mid-session model switching via `$switch_model()`.

* Session modes: `$set_mode("agent")` or `$set_mode("plan")`.

* Cancel support: Ctrl+C interrupt + `$cancel()` method.

* Streaming callbacks: `on_text`, `on_tool_call`, `on_thought`.

* Permission handling for built-in tool calls (`"auto-allow"`,
  `"auto-deny"`, or custom function).

* `hal_available()` checks for a working Copilot CLI installation.

* `hal_models()` lists available models and usage multipliers.

* CLI discovery via standalone `copilot` binary, `gh copilot --`,
  or `COPILOT_CLI_PATH` env var.

* Tool registration compatible with `ellmer::ToolDef`.
