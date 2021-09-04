pol2env <- function(init, PolArgs, envir=NULL) {
  if (length(PolArgs) != sum(nzchar(names(PolArgs)))) {
    rlang::abort("All arguments must be named.")
  }
  # === === === === ===
  model <- do.call(init, PolArgs)
  if (typeof(model) != "list") {
    rlang::abort("A policy initiator must return a named list.")
  }
  if (length(model) != sum(nzchar(names(model)))) {
    rlang::abort("A policy initiator must return a named list.")
  }
  # === === === === ===
  list2env(model, envir=envir)
}
