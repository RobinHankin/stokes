# stokes 1.1-0

- tighter integration with the `spray` and `disordR` packages
- comparison operators `!=` and `==` with numeric implemented
- `is.zero(0)` now true [no need to `drop()`!]
- `as.one()` added
- dependency on magrittr removed; `%>%` is now `|>`

# stokes 1.1-1

- Can use `^` in place of `%^%`