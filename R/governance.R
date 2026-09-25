# ==============================================================================
# hal Governance -- Security scanners and safety controls
# ==============================================================================
#
# Ported from HAL's governance module. Backend-agnostic security layer:
#   - eval_r denylist: block destructive function calls before execution
#   - Credential scanner: detect secrets in outbound text
#   - Timeout wrapper: time-limit eval_r execution
#   - Spawn cap: limit parallel worker count
#
# Config options (set via hal_configure()):
#   hal.eval_denylist     -- character vector of blocked fns, or FALSE
#   hal.credential_action -- "warn" (default), "redact", or "block"
#   hal.eval_timeout      -- numeric seconds (default 30)

# ------------------------------------------------------------------------------
# Default denylist for eval_r
# ------------------------------------------------------------------------------

#' Default denylist for eval_r
#'
#' eval_r is scoped to the user's R environment: compute, transform, and
#' inspect objects. File I/O, shell, package surgery, and bulk network are
#' handled by the SDK's built-in tools (create/edit/apply_patch/bash), which
#' have their own permission prompts. Routing those through eval_r bypasses
#' the intended checkpoints, so they're blocked here globally across all
#' modes.
#'
#' @keywords internal
#' @noRd
.HAL_DEFAULT_EVAL_DENYLIST <- c(
  # Shell access -- use the SDK bash tool instead
  "system", "system2", "shell", "shell.exec",
  # File destruction -- use the SDK edit tool (or the user does it)
  "unlink", "file.remove", "file.rename",
  # File writing -- use the SDK create/edit/apply_patch tools instead
  "writeLines", "write", "write.csv", "write.csv2", "write.table",
  "writeBin", "saveRDS", "save", "save.image", "sink",
  "file.create", "file.copy", "dir.create",
  # Network/download
  "download.file", "url", "socketConnection",
  # Code injection
  "source", ".Internal", ".Call", ".External",
  # Environment / state mutation
  "Sys.setenv", "Sys.unsetenv", "setwd", "rm", "assign",
  # Package surgery -- the user manages their own library
  "install.packages", "remove.packages", "update.packages",
  # Process control
  "quit", "q"
)

# ------------------------------------------------------------------------------
# eval_r denylist check
# ------------------------------------------------------------------------------

#' Check R code against the eval_r denylist
#'
#' Parses submitted code and walks the AST to find calls to blocked functions.
#'
#' @param code Character string of R code to check.
#' @return NULL if clean, character error message if blocked.
#' @keywords internal
#' @noRd
.hal_check_eval_denylist <- function(code) {
  denylist <- getOption("hal.eval_denylist", .HAL_DEFAULT_EVAL_DENYLIST)

  # FALSE or empty character = disabled
  if (identical(denylist, FALSE) || (is.character(denylist) && length(denylist) == 0L)) {
    return(NULL)
  }

  expr <- tryCatch(parse(text = code), error = function(e) NULL)
  if (is.null(expr)) return(NULL)  # let eval report parse errors

  blocked <- .hal_find_blocked_calls(expr, denylist)

  if (length(blocked) > 0L) {
    unique_blocked <- unique(blocked)
    paste0(
      "Blocked by governance policy: code contains call(s) to restricted function(s): ",
      paste(unique_blocked, collapse = ", "),
      ". These functions are on the eval_r denylist for safety. ",
      "If you need this capability, ask the user to run the code directly."
    )
  } else {
    NULL
  }
}

#' Recursively walk an AST and find calls to blocked functions
#'
#' @param expr An R expression (from parse()).
#' @param denylist Character vector of blocked function names.
#' @return Character vector of blocked function names found.
#' @keywords internal
#' @noRd
.hal_find_blocked_calls <- function(expr, denylist) {
  blocked <- character()

  .walk <- function(node) {
    if (is.call(node)) {
      fn_name <- NULL
      if (is.symbol(node[[1L]])) {
        fn_name <- as.character(node[[1L]])
      } else if (is.call(node[[1L]]) &&
                 as.character(node[[1L]][[1L]]) %in% c("::", ":::")) {
        fn_name <- as.character(node[[1L]][[3L]])
      }
      if (!is.null(fn_name) && fn_name %in% denylist) {
        blocked <<- c(blocked, fn_name)
      }
      for (i in seq_along(node)[-1L]) {
        if (!is.null(node[[i]])) .walk(node[[i]])
      }
    } else if (is.recursive(node)) {
      for (i in seq_along(node)) {
        if (!is.null(node[[i]])) .walk(node[[i]])
      }
    }
  }

  .walk(expr)
  blocked
}

# ------------------------------------------------------------------------------
# Credential scanner
# ------------------------------------------------------------------------------

#' Known credential patterns (regex)
#' @keywords internal
#' @noRd
.HAL_CREDENTIAL_PATTERNS <- list(
  # GitHub tokens
  github_pat       = "ghp_[A-Za-z0-9]{36}",
  github_pat_fine  = "github_pat_[A-Za-z0-9_]{82}",
  github_oauth     = "gho_[A-Za-z0-9]{36}",
  github_app       = "ghs_[A-Za-z0-9]{36}",
  github_refresh   = "ghr_[A-Za-z0-9]{36}",
  # OpenAI: project / service-account / admin keys (sk-proj-…, sk-svcacct-…)
  # must precede the legacy pattern so the longer match wins on detection.
  openai_project   = "sk-(?:proj|svcacct|admin)-[A-Za-z0-9_-]{20,}",
  # OpenAI legacy keys (sk-…)
  openai_key       = "sk-[A-Za-z0-9]{20,}",
  # Anthropic keys (sk-ant-api03-…)
  anthropic_key    = "sk-ant-[A-Za-z0-9_-]{20,}",
  # AWS
  aws_access_key   = "AKIA[0-9A-Z]{16}",
  aws_secret_key   = "(?i)aws[_\\-]?secret[_\\-]?access[_\\-]?key[\\s=:]+[A-Za-z0-9/+=]{40}",
  # Google API key
  google_api_key   = "AIza[0-9A-Za-z_-]{35}",
  # Slack tokens + incoming webhooks
  slack_token      = "xox[a-z]-[A-Za-z0-9-]{10,}",
  slack_webhook    = "https://hooks\\.slack\\.com/services/[A-Za-z0-9/]{20,}",
  # PEM private keys -- match the whole block so redaction removes the key body,
  # not just the header. [\\s\\S] spans newlines under perl = TRUE.
  private_key_pem  = "-----BEGIN [A-Z ]*PRIVATE KEY-----[\\s\\S]*?-----END [A-Z ]*PRIVATE KEY-----",
  # Generic assignment of a quoted secret-looking value
  generic_key_val  = "(?i)(api[_\\-]?key|secret|token|password|passwd|credentials)[\\s]*[=:][\\s]*['\"][A-Za-z0-9+/=_\\-]{16,}['\"]"
)

#' Known sensitive environment variable names
#' @keywords internal
#' @noRd
.HAL_SENSITIVE_ENV_VARS <- c(
  "GITHUB_PAT", "GITHUB_TOKEN", "GH_TOKEN",
  "OPENAI_API_KEY", "ANTHROPIC_API_KEY",
  "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN",
  "AZURE_API_KEY", "GOOGLE_API_KEY", "GOOGLE_APPLICATION_CREDENTIALS",
  "DATABASE_URL", "DB_PASSWORD",
  "SMTP_PASSWORD", "SENDGRID_API_KEY",
  "SLACK_TOKEN", "DISCORD_TOKEN",
  "JWT_SECRET", "SESSION_SECRET", "ENCRYPTION_KEY"
)

#' Scan text for credential patterns
#'
#' @param text Character string to scan.
#' @return List with `found`, `matches`, `cleaned`.
#' @keywords internal
#' @noRd
.hal_scan_credentials <- function(text) {
  if (!is.character(text) || length(text) == 0L || !nzchar(text)) {
    return(list(found = FALSE, matches = character(), cleaned = text))
  }

  matched_names <- character()

  for (nm in names(.HAL_CREDENTIAL_PATTERNS)) {
    pattern <- .HAL_CREDENTIAL_PATTERNS[[nm]]
    if (grepl(pattern, text, perl = TRUE)) {
      matched_names <- c(matched_names, nm)
    }
  }

  for (var_name in .HAL_SENSITIVE_ENV_VARS) {
    val <- Sys.getenv(var_name, unset = "")
    if (nzchar(val) && nchar(val) >= 8L && grepl(val, text, fixed = TRUE)) {
      matched_names <- c(matched_names, paste0("env:", var_name))
    }
  }

  found <- length(matched_names) > 0L
  cleaned <- text
  if (found) {
    for (nm in names(.HAL_CREDENTIAL_PATTERNS)) {
      if (nm %in% matched_names) {
        cleaned <- gsub(.HAL_CREDENTIAL_PATTERNS[[nm]], "[REDACTED]",
                        cleaned, perl = TRUE)
      }
    }
    env_matches <- matched_names[startsWith(matched_names, "env:")]
    for (env_match in env_matches) {
      var_name <- sub("^env:", "", env_match)
      val <- Sys.getenv(var_name, unset = "")
      if (nzchar(val)) {
        cleaned <- gsub(val, "[REDACTED]", cleaned, fixed = TRUE)
      }
    }
  }

  list(found = found, matches = matched_names, cleaned = cleaned)
}

# ------------------------------------------------------------------------------
# Gateway: scan outbound text
# ------------------------------------------------------------------------------

#' Scan outbound text through all active governance scanners
#'
#' @param text Character string to scan.
#' @return The text (possibly redacted). Stops if action is "block".
#' @keywords internal
#' @noRd
.hal_scan_outbound <- function(text) {
  if (!is.character(text) || length(text) == 0L) return(text)

  action <- getOption("hal.credential_action", "warn")
  if (!action %in% c("warn", "redact", "block")) {
    cli::cli_abort("{.arg credential_action} must be one of {.val {c('warn', 'redact', 'block')}}, not {.val {action}}.")
  }

  result <- .hal_scan_credentials(text)

  if (result$found) {
    detail <- paste("Detected potential credentials in outbound text:",
                    paste(result$matches, collapse = ", "))

    switch(action,
      "block" = cli::cli_abort(c(
        "hal governance: {detail}",
        "x" = "Transmission blocked.",
        "i" = "Review your data before sending to an LLM API."
      )),
      "redact" = {
        cli::cli_warn(c(
          "hal governance: {detail}",
          "i" = "Credentials have been redacted."
        ))
        return(result$cleaned)
      },
      "warn" = {
        cli::cli_warn(c(
          "hal governance: {detail}",
          "i" = "Consider using {.code hal_configure(credential_action = 'redact')}."
        ))
      }
    )
  }

  text
}

# ------------------------------------------------------------------------------
# eval_r timeout wrapper
# ------------------------------------------------------------------------------

#' Execute an expression with a time limit
#'
#' @param expr An R expression to evaluate.
#' @param envir Environment to evaluate in.
#' @param timeout_secs Numeric seconds.
#' @return The result of evaluating the expression.
#' @keywords internal
#' @noRd
.hal_eval_with_timeout <- function(expr, envir, timeout_secs = NULL) {
  timeout <- timeout_secs %||% getOption("hal.eval_timeout", 30)

  if (is.null(timeout) || !is.numeric(timeout) || timeout <= 0) {
    return(eval(expr, envir = envir))
  }

  setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)

  eval(expr, envir = envir)
}

