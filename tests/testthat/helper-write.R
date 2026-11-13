# Run a write_*() function; return the generated code and the printed lines
run_writer <- function(writer, result, ...) {
  out <- capture.output(code <- writer(result, ...))
  list(code = code, out = out)
}
