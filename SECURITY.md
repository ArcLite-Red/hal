# Security Policy

## Supported versions

Only the latest released version of hal is supported with security fixes.

## Reporting a vulnerability

Please report suspected vulnerabilities privately via
[GitHub Security Advisories](https://github.com/ArcLite-Red/hal/security/advisories/new)
("Report a vulnerability"). Do not open a public issue for security
reports. You should receive a response within a week.

## Scope notes

hal executes model-generated R code in your session by design (`eval_r`,
`hal_do()`). The governance layer (AST denylist, credential scanner, eval
timeout, permission policies) reduces risk but is not a sandbox — see
`vignette("safety", package = "hal")` for the threat model and its limits.
Reports that bypass the documented governance controls (e.g. denylist
evasion, credential-scanner bypass) are in scope and welcome.
