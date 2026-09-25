# ==============================================================================
# hal Output -- Streaming display and response formatting
# ==============================================================================
#
# Ported from HAL's output module. Provides ANSI-colored, markdown-aware
# response formatting with optional typewriter streaming.
#
# Simplified from HAL: no persona-specific header/footer, cleaner API.

# ------------------------------------------------------------------------------
# Main entry point
# ------------------------------------------------------------------------------

#' Format and optionally stream a response to the console
#'
#' @param text The response text to display.
#' @param stream Logical; if TRUE, use typewriter streaming.
#' @param speed Character; "instant", "fast", "medium", or "slow".
#' @return Invisibly returns the original text.
#' @keywords internal
#' @noRd
.hal_display_response <- function(text, stream = NULL, speed = NULL) {
  if (!nzchar(text)) return(invisible(text))

  stream <- stream %||% getOption("hal.stream", TRUE)
  speed <- speed %||% getOption("hal.stream_speed", "medium")
  use_colors <- .hal_supports_color()

  # Response frame: header with colored dots
  .hal_frame_open(use_colors)

  formatted <- .hal_format_response(text)

  if (!isTRUE(stream) || identical(speed, "instant")) {
    cat("\n", formatted, "\n", sep = "")
    .hal_frame_close(use_colors)
    return(invisible(text))
  }

  chunks <- .hal_parse_chunks(formatted)

  speed_settings <- list(
    fast   = list(delay = 0.008, chunk_delay = 0.04),
    medium = list(delay = 0.015, chunk_delay = 0.08),
    slow   = list(delay = 0.04,  chunk_delay = 0.2)
  )
  settings <- speed_settings[[speed]] %||% speed_settings[["medium"]]

  cat("\n")
  for (i in seq_along(chunks)) {
    .hal_stream_text(chunks[[i]], settings$delay)
    if (settings$chunk_delay > 0 && i < length(chunks)) {
      Sys.sleep(settings$chunk_delay * runif(1, 0.8, 1.2))
    }
  }
  cat("\n")

  .hal_frame_close(use_colors)

  invisible(text)
}

# ------------------------------------------------------------------------------
# Response frame -- colored dots + border
# ------------------------------------------------------------------------------

#' Open response frame with colored dots header and timestamp
#' @keywords internal
#' @noRd
.hal_frame_open <- function(use_colors) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  if (use_colors) {
    dot <- "\u25cf"
    # Wide-spaced dots: red, yellow, green
    dots <- paste0(
      " \033[31m", dot, "\033[0m  ",
      "\033[33m", dot, "\033[0m  ",
      "\033[32m", dot, "\033[0m"
    )
    cat(paste0(
      "\n", dots, "  \033[1;97mhal\033[0m",
      " \033[90m[ ", ts, " ]\033[0m\n"
    ), sep = "")
  } else {
    cat(sprintf("\n o  o  o  hal [ %s ]\n", ts))
  }
}

#' Close response frame with sign-off footer
#' @keywords internal
#' @noRd
.hal_frame_close <- function(use_colors) {
  corner <- "\u250c"
  dash <- "\u2500"
  if (use_colors) {
    cat(paste0(
      "\n \033[90m", corner, dash, "\033[0m",
      " \033[3;90mAnalysis complete.\033[0m\n\n"
    ), sep = "")
  } else {
    cat("\n +- Analysis complete.\n\n")
  }
}

# ------------------------------------------------------------------------------
# Response formatter
# ------------------------------------------------------------------------------

#' Format response text with ANSI colors and structure
#'
#' @param text Raw response text.
#' @return Formatted text string.
#' @keywords internal
#' @noRd
.hal_format_response <- function(text) {
  use_colors <- .hal_supports_color()
  lines <- strsplit(text, "\n")[[1]]
  formatted <- character()
  in_code_block <- FALSE


  for (line in lines) {
    if (!nzchar(trimws(line))) {
      formatted <- c(formatted, "")
      next
    }

    # Code blocks
    if (grepl("^```", line)) {
      in_code_block <- !in_code_block
      if (in_code_block) {
        formatted <- c(formatted, .hal_style_code_start(use_colors))
      } else {
        formatted <- c(formatted, .hal_style_code_end(use_colors))
      }
      next
    }

    if (in_code_block) {
      formatted <- c(formatted, .hal_style_code_line(line, use_colors))
      next
    }

    # Headers
    if (grepl("^#{1,6}\\s", line)) {
      formatted <- c(formatted, .hal_style_header(line, use_colors))
    }
    # Numbered lists
    else if (grepl("^\\s*\\d+\\.\\s", line)) {
      formatted <- c(formatted, .hal_style_numbered(line, use_colors))
    }
    # Bullet lists
    else if (grepl("^\\s*[-*]\\s", line)) {
      formatted <- c(formatted, .hal_style_bullet(line, use_colors))
    }
    # Bold key: value
    else if (grepl("\\*\\*[^*]+\\*\\*:", line)) {
      formatted <- c(formatted, .hal_style_key_value(line, use_colors))
    }
    # Inline code
    else if (grepl("`[^`]+`", line)) {
      formatted <- c(formatted, .hal_style_inline_code(line, use_colors))
    }
    # Bold
    else if (grepl("\\*\\*[^*]+\\*\\*", line)) {
      formatted <- c(formatted, .hal_style_bold(line, use_colors))
    }
    # Regular
    else {
      formatted <- c(formatted, .hal_style_regular(line, use_colors))
    }
  }

  paste(formatted, collapse = "\n")
}

# ------------------------------------------------------------------------------
# Style helpers
# ------------------------------------------------------------------------------

.hal_style_header <- function(line, use_colors) {
  level <- nchar(gsub("^(#{1,6}).*", "\\1", line))
  text <- trimws(gsub("^#{1,6}\\s*", "", line))
  if (!use_colors) {
    return(paste0("\n", toupper(text), "\n",
                  paste(rep("=", nchar(text)), collapse = "")))
  }
  if (level == 1) {
    # Box-drawn header
    top_bar <- paste(rep("\u2550", nchar(text) + 2), collapse = "")
    paste0(
      "\n",
      "\033[1;36m", "\u2554", top_bar, "\u2557", "\033[0m", "\n",
      "\033[1;36m", "\u2551", " ", "\033[1;97m", text, "\033[1;36m", " ", "\u2551", "\033[0m", "\n",
      "\033[1;36m", "\u255a", top_bar, "\u255d", "\033[0m"
    )
  } else if (level == 2) {
    paste0("\n", "\033[1;35m", "\u25b8", " ", "\033[1;97m", text, "\033[0m")
  } else {
    paste0("\n", "\033[1;34m", "\u2022", " ", "\033[1;93m", text, "\033[0m")
  }
}

.hal_style_numbered <- function(line, use_colors) {
  if (!use_colors) return(line)
  number <- gsub("^\\s*(\\d+)\\..*", "\\1", line)
  content <- gsub("^\\s*\\d+\\.\\s*", "", line)
  indent <- gsub("^(\\s*)\\d+\\..*", "\\1", line)
  paste0(indent, "\033[1;32m", number, ".\033[0m \033[97m", content, "\033[0m")
}

.hal_style_bullet <- function(line, use_colors) {
  if (!use_colors) return(line)
  content <- gsub("^\\s*[-*]\\s*", "", line)
  indent <- gsub("^(\\s*)[-*].*", "\\1", line)
  paste0(indent, "\033[1;33m", "\u25b8", "\033[0m \033[97m", content, "\033[0m")
}

.hal_style_key_value <- function(line, use_colors) {
  if (!use_colors) return(gsub("\\*\\*([^*]+)\\*\\*", "\\1", line))
  key <- gsub("^\\s*[-*]?\\s*\\*\\*([^*]+)\\*\\*:.*", "\\1", line)
  value <- gsub("^\\s*[-*]?\\s*\\*\\*[^*]+\\*\\*:\\s*", "", line)
  indent <- gsub("^(\\s*).*", "\\1", line)
  paste0(indent, "\033[1;94m", key, ":\033[0m \033[96m", value, "\033[0m")
}

.hal_style_inline_code <- function(line, use_colors) {
  if (!use_colors) return(gsub("`([^`]+)`", " \\1 ", line))
  gsub("`([^`]+)`", " \033[1;93m\033[100m \\1 \033[0m", line)
}

.hal_style_bold <- function(line, use_colors) {
  if (!use_colors) return(gsub("\\*\\*([^*]+)\\*\\*", "\\1", line))
  gsub("\\*\\*([^*]+)\\*\\*", "\033[1;97m\\1\033[0m", line)
}

.hal_style_code_start <- function(use_colors) {
  if (!use_colors) return("\n CODE ")
  paste0("\n \033[1;30m\033[47m", " CODE ", "\033[0m")
}

.hal_style_code_end <- function(use_colors) {
  if (!use_colors) return("")
  "\033[0m"
}

.hal_style_code_line <- function(line, use_colors) {
  if (!use_colors) return(paste0("  ", line))
  paste0("  \033[1;37m\033[40m", line, "\033[0m")
}

.hal_style_regular <- function(line, use_colors) {
  if (!use_colors) return(line)
  paste0("\033[97m", line, "\033[0m")
}

# ------------------------------------------------------------------------------
# Thought display
# ------------------------------------------------------------------------------

#' Display agent thought chunks inline
#'
#' Streams thought text in dimmed italic style with a thinking indicator.
#' Called via the `on_thought` callback during `hal()` prompts.
#'
#' @param chunk A single thought text chunk.
#' @param use_colors Whether to use ANSI colors.
#' @keywords internal
#' @noRd
.hal_thought_open <- function(use_colors = .hal_supports_color()) {
  cat("\n")
  if (use_colors) {
    cat("\033[3;90m", "\u25cc ", sep = "")
  } else {
    cat("~ ", sep = "")
  }
  flush.console()
}

.hal_thought_chunk <- function(chunk, use_colors = .hal_supports_color()) {
  if (use_colors) {
    cat("\033[3;90m", chunk, "\033[0m", sep = "")
  } else {
    cat(chunk, sep = "")
  }
  flush.console()
}

.hal_thought_close <- function(use_colors = .hal_supports_color()) {
  cat("\n\n")
  flush.console()
}

# ------------------------------------------------------------------------------
# Streaming helpers
# ------------------------------------------------------------------------------

#' Parse formatted text into logical chunks for streaming
#' @keywords internal
#' @noRd
.hal_parse_chunks <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  chunks <- character()
  current <- ""

  structural <- "^(?:#{1,6}\\s|\\s*\\d+\\.\\s|\\s*[-*]\\s|```)"

  for (line in lines) {
    if (grepl(structural, line, perl = TRUE) || grepl("^\\s*$", line)) {
      if (nzchar(current)) {
        chunks <- c(chunks, current)
        current <- ""
      }
      if (nzchar(line)) chunks <- c(chunks, line)
    } else {
      if (nzchar(current)) current <- paste0(current, "\n")
      current <- paste0(current, line)
    }
  }
  if (nzchar(current)) chunks <- c(chunks, current)
  chunks
}

#' Stream text character by character
#' @keywords internal
#' @noRd
.hal_stream_text <- function(text, delay) {
  if (delay <= 0) {
    cat(text, "\n")
    flush.console()
    return(invisible())
  }

  chars <- strsplit(text, "")[[1]]
  for (char in chars) {
    cat(char, sep = "")
    flush.console()
    char_delay <- if (char %in% c(".", "!", "?")) {
      delay * 3
    } else if (char %in% c(",", ";", ":")) {
      delay * 2
    } else if (char == " ") {
      delay * 0.5
    } else {
      delay
    }
    Sys.sleep(char_delay * runif(1, 0.8, 1.2))
  }
  cat("\n")
  flush.console()
}

# ------------------------------------------------------------------------------
# Terminal detection
# ------------------------------------------------------------------------------

#' Check terminal color support
#' @keywords internal
#' @noRd
.hal_supports_color <- function() {
  if (!isTRUE(getOption("hal.use_colors", TRUE))) return(FALSE)

  # IDE detection -- Positron and RStudio support ANSI on all platforms
  if (nzchar(Sys.getenv("RSTUDIO", ""))) return(TRUE)
  if (nzchar(Sys.getenv("POSITRON", ""))) return(TRUE)

  if (!interactive()) return(FALSE)

  term <- Sys.getenv("TERM", "")
  colorterm <- Sys.getenv("COLORTERM", "")
  nzchar(colorterm) ||
    grepl("color|xterm|screen|tmux", term, ignore.case = TRUE) ||
    .Platform$OS.type != "windows"
}
