library("hexSticker")
library("magick")
library("magrittr")

docstring <- "

Function sticker() below reads file stokes_theorem.png, which not held under version control.  To make it from scratch:

change directory to stokes/inst/sticker

pdflatex stokes_theorem  [produces stokes_theorem.pdf]

pdftoppm -png -x 3700 -W 1100 -y 1900 -H 500 -r 1000  stokes_theorem.pdf -singlefile stokes_theorem

The line above produces stokes_theorem.png [not under version control] but with a *white* background and
we need it to be #7733FF.   Do this in gimp, and save it as stokes_temp.png.

The flag transparent_background, if TRUE, uses experimental techniques that give a transparent background.

"

imgurl <- "stokes_theorem.png"
transparent_background <- FALSE

sticker(imgurl, package="stokes", p_size=30, s_x=1, s_y=.85,
s_width=1, asp=sqrt(3)/2, white_around_sticker=TRUE, h_fill="#7733FF",
h_color="#000000", filename="stokes_temp.png")

if(transparent_background){
    ## Following methodology shamelessly ripped off from the excellent watson package:
    fuzz <- 50
    p <- image_read("stokes_temp.png")
    w <- image_info(p)$width-1
    pp <- p %>%
        image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = "+1+1"                   ) %>%
        image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+" , w  , "+1"  )) %>%
        image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+1", "+", w     )) %>%
        image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+" , w  , "+", w)) %>%
        image_write(path = "stokes.png")
} else {
    p <- image_read("stokes_temp.png")
    image_write(p, path = "stokes.png")
}


