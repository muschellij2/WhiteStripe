library(hexSticker)
library(desc)
desc = desc::description$new()
package = desc$get("Package")
outline = "#3f8cb1"
background = "#e6e7e8"
sticker("icon.png",	
        package = package,
        h_fill = background,
        h_color = outline, 
        p_size = 6,
        s_height = 0.4,
        s_width = 0.4 * 2.2, 
        s_x = 1,
        s_y = 0.9, 
        filename = "sticker.png")

