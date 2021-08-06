library("hexSticker")

imgurl <- system.file("stokes_theorem.png", package="stokes")
sticker(imgurl, package="stokes", p_size=8, s_x=1, s_y=.85,
s_width=1, asp=0.85, white_around_sticker=TRUE, h_fill="#7733FF",
h_color="#000000", filename="stokes.png")
