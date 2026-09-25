# Tests for HalClientVSCode (R6 transport for the hal-bridge backend).
#
# These cover the parts of the client that don't require an actual HTTP
# round-trip: construction, tool registration, mode handling, session
# swapping, and cancel-flag propagation. The HTTP /chat path is exercised
# end-to-end by dev/test_vscode_bridge.R against the live bridge.

test_that("HalClientVSCode constructs with sensible defaults", {
  client <- HalClientVSCode$new(quiet = TRUE)
  expect_s3_class(client, "HalClientVSCode")
  expect_null(client$get_session_id())
  expect_false(client$is_alive())
})

test_that("register_tools accumulates and dedupes by name", {
  client <- HalClientVSCode$new(quiet = TRUE)

  client$register_tools(list(foo = list(name = "foo", fun = identity,
                                          description = "first")))
  client$register_tools(list(bar = list(name = "bar", fun = identity,
                                          description = "second")))

  # Access internal state via the R6 private env so the test inspects
  # actual storage rather than a derived view.
  tools <- client$.__enclos_env__$private$registered_tools
  expect_named(tools, c("foo", "bar"))

  # Re-register foo with new description -> overwrites by name (not duplicate).
  client$register_tools(list(foo = list(name = "foo", fun = identity,
                                          description = "updated")))
  tools <- client$.__enclos_env__$private$registered_tools
  expect_length(tools, 2L)
  expect_identical(tools$foo$description, "updated")
})

test_that("tool_payload surfaces parameters from as_tool_def schema and top-level", {
  client <- HalClientVSCode$new(quiet = TRUE)

  # as_tool_def() shape: JSON Schema nested under schema$function$parameters.
  as_td <- list(
    name = "scan", description = "scan it", fun = identity,
    schema = list(type = "function", "function" = list(
      name = "scan", description = "scan it",
      parameters = list(
        type = "object",
        properties = list(data = list(type = "string")),
        required = list("data")
      )
    ))
  )
  # eval_r shape: parameters at the top level.
  toplevel_td <- list(
    name = "eval_r", description = "run code", fun = identity,
    parameters = list(type = "object",
                      properties = list(code = list(type = "string")))
  )

  client$register_tools(list(scan = as_td, eval_r = toplevel_td))
  payload <- client$.__enclos_env__$private$tool_payload()
  names(payload) <- vapply(payload, function(x) x$name, character(1))

  # The bug: as_tool_def tools were sent with empty parameters.
  expect_true("data" %in% names(payload$scan$parameters$properties))
  expect_equal(payload$scan$parameters$required, list("data"))
  # eval_r (top-level parameters) still works.
  expect_true("code" %in% names(payload$eval_r$parameters$properties))
})

test_that("set_mode warns for non-agent modes and falls back silently for agent", {
  # The warn is gated behind !quiet (consistent with other clients);
  # build a non-quiet client for the warning assertions.
  client <- HalClientVSCode$new(quiet = FALSE)

  expect_warning(client$set_mode("plan"), "not supported")
  expect_warning(client$set_mode("autopilot"), "not supported")

  expect_no_warning(client$set_mode("agent"))
})

test_that("switch_model updates the stored model id", {
  client <- HalClientVSCode$new(model = "auto", quiet = TRUE)
  expect_identical(
    client$.__enclos_env__$private$model_id, "auto"
  )
  client$switch_model("claude-haiku-4.5")
  expect_identical(
    client$.__enclos_env__$private$model_id, "claude-haiku-4.5"
  )
})

test_that("cancel sets the cancelled flag", {
  client <- HalClientVSCode$new(quiet = TRUE)
  expect_false(client$.__enclos_env__$private$cancelled)
  client$cancel()
  expect_true(client$.__enclos_env__$private$cancelled)
})

test_that("swap_session / restore_session round-trips state", {
  # Force the discover step to succeed without a live bridge by writing a
  # synthetic port file where .hal_bridge_port_file() will look.
  td <- withr::local_tempdir()
  withr::with_envvar(.bridge_env(td), {
    .write_port_file(list(port = 49152L, pid = 1L, version = "0.1.0"))
    client <- HalClientVSCode$new(quiet = TRUE)
    client$start()

    sid1 <- client$new_session()$sessionId
    expect_true(nzchar(sid1))

    saved <- client$swap_session()
    expect_identical(saved, sid1)

    # After swap we have a fresh session id.
    sid2 <- client$get_session_id()
    expect_false(identical(sid1, sid2))

    client$restore_session(sid1)
    expect_identical(client$get_session_id(), sid1)
  })
})

test_that("handshake returns a sensible info shape without touching HTTP", {
  td <- withr::local_tempdir()
  withr::with_envvar(.bridge_env(td), {
    .write_port_file(list(port = 49152L, pid = 1L, version = "0.1.0"))
    client <- HalClientVSCode$new(quiet = TRUE)
    info <- client$handshake()
    expect_identical(info$agentInfo$name, "hal-bridge")
    expect_identical(info$agentInfo$version, "0.1.0")
  })
})

test_that("set_ipc is a no-op (the vscode backend runs tools in-process)", {
  client <- HalClientVSCode$new(quiet = TRUE)
  # Must accept arguments matching the other clients' API without erroring,
  # and must not mutate any state we can observe.
  expect_invisible(
    client$set_ipc(ipc_dir = tempdir(), eval_fn = identity,
                   permission_fn = identity)
  )
})

test_that("is_alive flips after start()", {
  td <- withr::local_tempdir()
  withr::with_envvar(.bridge_env(td), {
    .write_port_file(list(port = 49152L, pid = 1L, version = "0.1.0"))
    client <- HalClientVSCode$new(quiet = TRUE)
    expect_false(client$is_alive())
    client$start()
    expect_true(client$is_alive())
    client$stop()
    expect_false(client$is_alive())
  })
})

test_that("authorize() honors string and function permission policies", {
  mk <- function(policy) {
    HalClientVSCode$new(quiet = TRUE, permission_policy = policy)
  }

  # auto-allow (default): always allow
  c1 <- mk("auto-allow")
  d <- c1$.__enclos_env__$private$authorize("eval_r", list(code = "1+1"))
  expect_true(d$allow)

  # auto-deny: always deny
  c2 <- mk("auto-deny")
  d <- c2$.__enclos_env__$private$authorize("eval_r", list(code = "1+1"))
  expect_false(d$allow)
  expect_match(d$reason, "auto-deny")

  # function policy: allow path
  allow_fn <- function(params) {
    expect_identical(params$backend, "vscode")
    expect_identical(params$tool_name, "eval_r")
    "allow-once"
  }
  c3 <- mk(allow_fn)
  d <- c3$.__enclos_env__$private$authorize("eval_r", list(code = "1+1"))
  expect_true(d$allow)

  # function policy: deny path
  c4 <- mk(function(params) "reject-once")
  d <- c4$.__enclos_env__$private$authorize("eval_r", list(code = "1+1"))
  expect_false(d$allow)

  # function policy: errors are treated as deny
  c5 <- mk(function(params) stop("boom"))
  expect_warning(
    d <- c5$.__enclos_env__$private$authorize("eval_r", list()),
    "boom"
  )
  expect_false(d$allow)

  # ask in a non-interactive context defaults to deny
  c6 <- mk("ask")
  d <- c6$.__enclos_env__$private$authorize("eval_r", list(code = "x"))
  expect_false(d$allow)
  expect_match(d$reason, "non-interactive")
})

test_that("backend factory routes to HalClientVSCode under hal.backend='vscode'", {
  withr::with_options(list(hal.backend = "vscode"), {
    b <- hal:::.hal_backend()
    expect_identical(b, "vscode")
    # The factory should produce a HalClientVSCode instance.
    td <- withr::local_tempdir()
    withr::with_envvar(.bridge_env(td), {
      .write_port_file(list(port = 49152L, pid = 1L, version = "0.1.0"))
      client <- hal:::.hal_make_client(quiet = TRUE)
      expect_s3_class(client, "HalClientVSCode")
    })
  })
})

# ------------------------------------------------------------------------------
# Plot vision -- structured tool results + image pruning
# ------------------------------------------------------------------------------

test_that("execute_tool returns list(text, image) for plain and structured tools", {
  client <- HalClientVSCode$new(quiet = TRUE)
  plain <- list(name = "plain", fun = function() "hello", description = "d")
  imgy <- list(name = "imgy", description = "d", fun = function() {
    structure(list(text = "drew a plot",
                   image = list(mimeType = "image/png", data = "QUJD")),
              class = "hal_tool_result")
  })
  client$register_tools(list(plain = plain, imgy = imgy))
  priv <- client$.__enclos_env__$private

  res_plain <- priv$execute_tool("plain", list())
  expect_identical(res_plain$text, "hello")
  expect_null(res_plain$image)

  res_img <- priv$execute_tool("imgy", list())
  expect_identical(res_img$text, "drew a plot")
  expect_identical(res_img$image$data, "QUJD")

  res_missing <- priv$execute_tool("nope", list())
  expect_match(res_missing$text, "not registered")
  expect_null(res_missing$image)

  err_tool <- list(name = "boom", fun = function() stop("kapow"),
                   description = "d")
  client$register_tools(list(boom = err_tool))
  res_err <- priv$execute_tool("boom", list())
  expect_match(res_err$text, "kapow")
  expect_null(res_err$image)
})

test_that("prune_images keeps only the newest 2 image payloads", {
  client <- HalClientVSCode$new(quiet = TRUE)
  priv <- client$.__enclos_env__$private

  img_part <- function(id) {
    list(type = "tool_result", callId = id, content = "plotted",
         image = list(mimeType = "image/png", data = paste0("img", id)))
  }
  priv$messages <- list(
    list(role = "user", content = "hi"),
    list(role = "user", content = list(img_part("a"))),
    list(role = "user", content = list(img_part("b"),
                                       list(type = "text", value = "t"))),
    list(role = "user", content = list(img_part("c")))
  )
  priv$prune_images()

  imgs <- unlist(lapply(priv$messages, function(m) {
    if (!is.list(m$content)) return(NULL)
    vapply(Filter(function(p) is.list(p) && !is.null(p$image), m$content),
           function(p) p$image$data, character(1))
  }))
  expect_identical(unname(imgs), c("imgb", "imgc"))
  # Text content of the pruned part survives
  expect_identical(priv$messages[[2]]$content[[1]]$content, "plotted")
})

test_that("prune_images is a no-op when 2 or fewer images", {
  client <- HalClientVSCode$new(quiet = TRUE)
  priv <- client$.__enclos_env__$private
  priv$messages <- list(
    list(role = "user", content = list(
      list(type = "tool_result", callId = "a", content = "x",
           image = list(mimeType = "image/png", data = "one"))
    ))
  )
  before <- priv$messages
  priv$prune_images()
  expect_identical(priv$messages, before)
})
