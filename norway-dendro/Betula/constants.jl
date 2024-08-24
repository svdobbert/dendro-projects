# Colors
hex_colors_full = ["#064c72", "#026c80", "#1b3d37", "#8db4ad", "#ecae7d", "#ed6335", "#e9311a", "black"]
palette_full = [parse(Colorant, hex) for hex in hex_colors_full]

hex_colors_min = ["#064c72", "#ed6335", "#616D6B"]
palette_min = [parse(Colorant, hex) for hex in hex_colors_min]

hex_colors_new = ["#f3bb06", "#cd2a3b", "#bc93a2", "#df8b41", "#cce691", "#304a76", "#505a45", "#2c1112"]
palette_new = [parse(Colorant, hex) for hex in hex_colors_new]

hex_colors_env = ["#f3bb06", "#df8b41", "#cd2a3b", "#bc93a2", "#304a76", "#cce691"]
palette_env = [parse(Colorant, hex) for hex in hex_colors_env]


hex_colors_resorted  = ["#f3bb06", "#df8b41", "#cd2a3b", "#bc93a2", "#cce691", "#505a45", "#304a76", "#2c1112"]
palette_resorted = [parse(Colorant, hex) for hex in hex_colors_resorted]

hex_colors_contrast = ["#bc93a2", "#304a76", "#cd2a3b", "#f3bb06"]
palette_contrast = [parse(Colorant, hex) for hex in hex_colors_contrast]

hex_colors_short = ["#f3bb06", "#df8b41", "#cd2a3b", "#304a76", "#2c1112"]
palette_short = [parse(Colorant, hex) for hex in hex_colors_short]

hex_colors_grb = ["#f3bb06", "#cd2a3b", "#304a76"]
palette_grb = [parse(Colorant, hex) for hex in hex_colors_grb]

hex_colors_pattern = ["#f3bb06", "#df8b41", "#cd2a3b", "#bc93a2", "#cce691","#304a76", "#2c1112"]
palette_pattern = [parse(Colorant, hex) for hex in hex_colors_pattern]

hex_colors_rb = ["#cd2a3b", "#304a76"]
palette_rb = [parse(Colorant, hex) for hex in hex_colors_rb]

hex_colors_sorted =  ["#f3bb06", "#df8b41", "#cd2a3b", "#bc93a2", "#cce691","#304a76", "#2c1112", "#505a45"]
palette_sorted = [parse(Colorant, hex) for hex in hex_colors_sorted]

hex_colors_main = ["#d53d26", "#f3bb06", "#f4b9ab", "#6d6325", "#100202"]
palette_main = [parse(Colorant, hex) for hex in hex_colors_main]

alpha_value = 0.7

palette_light = [RGBA(red(c), green(c), blue(c), alpha_value) for c in parse.(Colorant, hex_colors_full)]

palette_grb_light = [RGBA(red(c), green(c), blue(c), alpha_value) for c in parse.(Colorant, hex_colors_grb)]

palette_rb_light = [RGBA(red(c), green(c), blue(c), alpha_value) for c in parse.(Colorant, hex_colors_rb)]
