# Utilities

# Changes numeric decimals to fixed length, and left justifies by padding spaces. Vectorised. Output is character. Use before writing tab-delimited file.
justify <- function(x, k) {
	format(round(x, k), nsmall = k, trim = FALSE)
}
# Similar but not left padded. Use in strings.
decimal <- function(x, k) {
	trimws(format(round(x, k), nsmall = k, trim = TRUE))
}