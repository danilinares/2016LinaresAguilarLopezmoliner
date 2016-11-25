setting_theme <- function() {
  
  theme_bias <- theme_set(theme_classic(10))
  theme_bias <- theme_update(axis.line = element_line(size = size_line),
                           axis.ticks= element_line(size = size_line))
}