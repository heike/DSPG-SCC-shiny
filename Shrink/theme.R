library(fresh)

# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#C8102E"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#383838",  
    dark_color = "#fffbf5",
    dark_hover_bg = "#C8102E"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#fffbf5", 
    info_box_bg = "#D8DEE9"
  )
)
