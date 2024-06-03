# stokes 1.2-1

- edge-cases for vector_cross_product(), now works with a single 2D vector
- removal of dependence on emulator [in favour of quadform]

# stokes 1.2-0

- spray functionality imported, dependency removed
- consistentification of vignettes


# stokes 1.1-0

- tighter integration with the `spray` and `disordR` packages
- comparison operators `!=` and `==` with numeric implemented
- `is.zero(0)` now true [no need to `drop()`!]
- `as.one()` added
- dependency on magrittr removed; `%>%` is now `|>`

# stokes 1.1-1

- Can use `^` in place of `%^%`

# stokes 1.1-2

- vector_cross_product() implemented
- new vignette

# stokes 1.1-3

- bugfix
- more vignettes

# stokes 1.1-4

- New function `0tensor()`

# stokes 1.1-6

- Improvements to summary print methods
- citation points to arXiv preprint
- tweaks to vignettes
- remove overlooked dependence on pracma

# stokes 1.1-7

- consistentification of the stokes vignette

# stokes 1.2-0

- arXiv preprint cited
- minor documentation improvements
- consistentifciation of the zero object
- spray package now imported rather than attached
- coverage increased
- three-dimensional vector cross product redefined and discussed in vignette `vector_cross_product`
