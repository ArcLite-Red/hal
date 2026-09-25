#' hal: GitHub Copilot SDK for R
#'
#' @description
#' Native R interface to GitHub Copilot via the Agent Client Protocol (ACP).
#' Communicates with the Copilot CLI over NDJSON stdio, giving R users direct
#' access to multiple models (GPT, Claude, Gemini) through GitHub's enterprise
#' infrastructure -- no API keys required.
#'
#' @section Getting started:
#' ```r
#' chat <- hal_chat()
#' chat$chat("What is R?")
#' ```
#'
#' @section Prerequisites:
#' - A GitHub Copilot subscription (Individual, Business, or Enterprise)
#' - The GitHub CLI (`gh`) with the Copilot extension, or a standalone
#'   Copilot CLI binary
#' - Use [hal_available()] to check your setup
#'
#' @seealso
#' - [hal_chat()] to create a chat session
#' - [hal_available()] to check CLI availability
#' - [hal_models()] to list available models
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom R6 R6Class
#' @importFrom rlang %||%
#' @importFrom utils flush.console head
#' @importFrom stats setNames runif
## usethis namespace: end
NULL
