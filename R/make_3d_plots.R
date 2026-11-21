# Builds static images of the 3D plots used to look for interactions in Problem 1.
# USAGE is on the vertical axis in every plot.
# Run from the repository root:  Rscript R/make_3d_plots.R

library(plotly)
library(htmlwidgets)
library(webshot2)

water = read.csv("https://raw.githubusercontent.com/Thuntida/dataset/refs/heads/main/water.csv")
water.df = data.frame(water)

plots = list(
  "3d_prod_temp_usage" = plot_ly(water.df, x = ~PROD, y = ~TEMP, z = ~USAGE, color = "purple", type = "scatter3d"),
  "3d_prod_hour_usage" = plot_ly(water.df, x = ~PROD, y = ~HOUR, z = ~USAGE, color = "purple", type = "scatter3d"),
  "3d_temp_hour_usage" = plot_ly(water.df, x = ~TEMP, y = ~HOUR, z = ~USAGE, color = "purple", type = "scatter3d")
)

dir.create("figures", showWarnings = FALSE)
for (name in names(plots)) {
  html = tempfile(fileext = ".html")
  saveWidget(plots[[name]], html, selfcontained = TRUE)
  webshot(html, file.path("figures", paste0(name, ".png")), vwidth = 800, vheight = 600, delay = 2)
}
