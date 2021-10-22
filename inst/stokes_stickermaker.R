library("hexSticker")

imgurl <- "stokes_theorem.png"
sticker(imgurl, package="stokes", p_size=24, s_x=1, s_y=.85,
s_width=1, asp=sqrt(3)/2, white_around_sticker=TRUE, h_fill="#7733FF",
h_color="#000000", filename="stokes.png")
