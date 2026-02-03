# powerrules hex sticker — pure ggplot implementation

library(ggplot2)
library(sysfonts)
library(showtext)

# --- Coordinate system and constants ---

y_curve_base <- 0.19
curve_scale  <- 0.675

x_grid <- seq(-3.6, 3.6, length.out = 2000)
y_base <- y_curve_base + curve_scale * dnorm(x_grid, mean = 0, sd = 1)

df_base <- data.frame(x = x_grid, y = y_base)

# --- Colors ---

bg    <- "#152234"
gold  <- "#E8B931"
ruby  <- "#A31D3A"
green <- "#1B6B3A"

# --- Font loading ---

font_add_google("Raleway", regular.wt = 400)
showtext_auto()
showtext_opts(dpi = 600)

# --- Hex polygon (6 vertices, clockwise from top) ---

df_hex <- data.frame(
  x = c(0, 4, 4, 0, -4, -4),
  y = c(0.85, 0.50, -0.20, -0.55, -0.20, 0.50)
)

# --- Ruler ---

# 3-sided border (bottom + left + right, no top)
# matching the curve x-extent [-3.6, 3.6]
y_ruler_bottom <- y_curve_base - 0.015 - 0.09
df_ruler_border <- data.frame(
  x    = c(-3.6, -3.6, 3.6),
  xend = c(-3.6,  3.6, 3.6),
  y    = c(y_curve_base, y_ruler_bottom, y_ruler_bottom),
  yend = c(y_ruler_bottom, y_ruler_bottom, y_curve_base)
)

# Hierarchical ticks: all rise up from bottom edge, uniform width,
# length-only variation (short / medium / long)
tick_x <- seq(-3.5, 3.5, length.out = 41)
tick_idx <- seq_along(tick_x) - 1  # 0-based index

tick_length <- ifelse(
  tick_idx %% 10 == 0, 0.09,                    # every 10th: long
  ifelse(tick_idx %% 5 == 0, 0.065, 0.04)       # every 5th: medium
)

df_ticks <- data.frame(
  x    = tick_x,
  xend = tick_x,
  y    = y_ruler_bottom,
  yend = y_ruler_bottom + tick_length
)

# --- Crown overlay ---

# Crown top ridge function (Laplace/double-exponential spikes)
b_side   <- 0.15   # scale parameter for side spikes
b_center <- 0.17   # scale parameter for center spike
A_side   <- 0.1725 # peak amplitude above base curve
A_center <- 0.195  # peak amplitude above base curve

spike_laplace <- function(x, m, b, A) {
  A * exp(-abs(x - m) / b)
}

y_top <- y_base +
  spike_laplace(x_grid, -1.20, b_side, A_side) +
  spike_laplace(x_grid,  0.00, b_center, A_center) +
  spike_laplace(x_grid,  1.20, b_side, A_side)

# Clamp outside crown span
y_top[abs(x_grid) > 2.05] <- y_base[abs(x_grid) > 2.05]

df_crown <- data.frame(x = x_grid, y_base = y_base, y_top = y_top)

# Crown fill polygon (gold region between base and top ridge)
crown_idx <- abs(x_grid) <= 2.05
df_crown_fill <- data.frame(
  x = c(x_grid[crown_idx], rev(x_grid[crown_idx])),
  y = c(y_top[crown_idx], rev(y_base[crown_idx]))
)

# Crown ridge stroke (subset for crown span)
df_crown_span <- df_crown[crown_idx, ]

# --- Fill regions ---

x_cut <- qnorm(0.80)
tail_idx <- x_grid >= x_cut
left_idx <- x_grid <= x_cut

# Green fill: left 80% of normal, from -3.6 to x_cut
df_left_normal <- data.frame(
  x = c(-3.6, x_grid[left_idx], x_cut),
  y = c(y_curve_base, y_base[left_idx], y_curve_base)
)

# Ruby fill: ruler interior rectangle
df_ruler_fill <- data.frame(
  x = c(-3.6, -3.6, 3.6, 3.6),
  y = c(y_ruler_bottom, y_curve_base, y_curve_base, y_ruler_bottom)
)

# Ruby fill: right 20% tail
df_tail <- data.frame(
  x = c(x_cut, x_cut, x_grid[tail_idx], 3.6),
  y = c(y_curve_base,
        y_curve_base + curve_scale * dnorm(x_cut, mean = 0, sd = 1),
        y_base[tail_idx],
        y_curve_base)
)

# --- Crown jewels ---

crown_jewel_x <- c(-1.20, 0.00, 1.20)
crown_jewel_y <- c(
  y_curve_base + curve_scale * dnorm(-1.20) + A_side + 0.03,
  y_curve_base + curve_scale * dnorm(0.00) + A_center + 0.03,
  y_curve_base + curve_scale * dnorm(1.20) + A_side + 0.03
)

df_crown_jewels <- data.frame(x = crown_jewel_x, y = crown_jewel_y)

# --- Build plot (layers bottom to top) ---

p <- ggplot() +
  # 1. hex polygon (bg fill)
  geom_polygon(data = df_hex, aes(x, y),
               fill = bg, color = NA) +
  # 2. green fill: left 80% of normal
  geom_polygon(data = df_left_normal, aes(x, y),
               fill = green, color = NA) +
  # 3. ruby fill: right 20% tail
  geom_polygon(data = df_tail, aes(x, y),
               fill = ruby, color = NA) +
  # 4. ruby fill: ruler interior
  geom_polygon(data = df_ruler_fill, aes(x, y),
               fill = ruby, color = NA) +
  # 5. thin baseline across bottom of normal
  annotate("segment", x = -3.6, xend = 3.6,
           y = y_curve_base, yend = y_curve_base,
           linewidth = 0.15, color = gold, lineend = "round") +
  # 6. base normal curve stroke (gold)
  geom_path(data = df_base, aes(x, y),
            linewidth = 0.55, color = gold,
            lineend = "round", linejoin = "round") +
  # 7. crown fill polygon (gold)
  geom_polygon(data = df_crown_fill, aes(x, y),
               fill = gold, color = NA) +
  # 8. crown ridge stroke (gold)
  geom_path(data = df_crown_span, aes(x, y_top),
            linewidth = 0.55, color = gold,
            lineend = "round", linejoin = "round") +
  # 9. ruler border (bottom + left + right, no top)
  geom_segment(data = df_ruler_border,
               aes(x = x, xend = xend, y = y, yend = yend),
               linewidth = 0.35, color = gold, lineend = "round") +
  # 10. ruler ticks
  geom_segment(data = df_ticks, aes(x = x, xend = xend, y = y, yend = yend),
               linewidth = 0.25, color = gold, lineend = "round") +
  # 11. crown jewels
  geom_point(data = df_crown_jewels, aes(x, y),
             shape = 21, size = 2, fill = green, stroke = 0) +
  # 12. "powerrules" text
  annotate("text", x = 0, y = -0.02, label = "powerrules",
           hjust = 0.5, vjust = 0.5,
           color = gold, family = "Raleway", fontface = "plain",
           size = 7, size.unit = "mm") +
  # --- Theme and scales ---
  scale_x_continuous(expand = c(0, 0), limits = c(-4, 4)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.55, 0.85)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

# --- Export ---

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("man/figures/logo.png", plot = p,
       width = 43.301, height = 50, units = "mm",
       dpi = 600, bg = "transparent")
