suppressPackageStartupMessages({
  library(ggplot2)
  library(hexSticker)
  library(pwr)
})

# Run from the package root (the folder that holds DESCRIPTION).
stopifnot(file.exists('DESCRIPTION'))

# Smooth power curve: two-sample t-test, d = 0.5
n_vals <- seq(10, 200, by = 1)
pw <- sapply(n_vals, function(n) {
  pwr::pwr.t.test(n = n, d = 0.5, sig.level = 0.05,
                  type = 'two.sample')$power
})
curve_df <- data.frame(n = n_vals, power = pw)

p <- ggplot(curve_df, aes(x = n, y = power)) +
  geom_hline(yintercept = 0.8, linetype = 'dashed',
             color = '#A8C7E8', linewidth = 0.4) +
  geom_line(color = 'white', linewidth = 1.4) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    plot.background  = element_rect(fill = 'transparent', color = NA)
  )

dir.create('man/figures', recursive = TRUE, showWarnings = FALSE)

hexSticker::sticker(
  subplot   = p,
  s_x       = 1,
  s_y       = 0.80,
  s_width   = 1.5,
  s_height  = 0.95,
  package   = 'zzpower',
  p_x       = 1,
  p_y       = 1.48,
  p_size    = 22,
  p_color   = 'white',
  p_family  = 'sans',
  h_fill    = '#0054AD',
  h_color   = 'white',
  h_size    = 1.2,
  filename  = 'man/figures/logo.png',
  dpi       = 600
)

cat('Wrote man/figures/logo.png\n')
