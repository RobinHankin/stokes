# stokes 1.1-0

- tighter integration with the `spray` and `disordR` packages
- comparision operators `!=` and `==` with numeric implemented
- `is.zero(0)` now true [no need to `drop()`!]
- `as.one()` added
- dependency on magrittr removed; `%>%` is now `|>`
