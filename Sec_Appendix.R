################################################################################
#                               APPENDIX
################################################################################
library("here")
library("ggplot2")
library("ggthemes")
library("ggside")

# Functions --------------------------------------------------------------------
source(here("utils/utils_syn_data.R"))

beta <- 0.75
x <- seq(-3.2, 3.2, length.out = 1000)

# Piece-wise linear function
y_pwlin <- dgp_pwlinear(x, beta, 0.75)$lp

# Smooth function
y_smooth <- dgp_smooth(x, beta, 0)$lp

# Non-Continous
y_noncont <- dgp_nonlinear(x, beta, 0.75)$lp


data <- data.frame(
  x = x, 
  y = c(y_pwlin, y_smooth, y_noncont),
  type = rep(c("Piece-wise linear", "Smooth", "Non-Continuous"), each = 1000)
)

data_density <- data.frame(
  x = c(rnorm(100000) - 0.75, rnorm(100000), rnorm(100000) + 0.75),
  type = rep(c("N(-0.75, 1)", "N(0, 1)", "N(0.75, 1)"), each = 100000)
)

ggplot(data) +
  geom_line(aes(x = x, y = y, color = type), linewidth = 1, alpha = 0.8) +
  scale_color_colorblind() +
  geom_xsidehline(yintercept = 0) +
  geom_xsidedensity(data = data_density, mapping = aes(x = x, xfill = type, xcolor = type), linewidth = 0.2, alpha = 0.7) +
  guides(xfill = "none", xcolor = "none") + 
  scale_xfill_manual(values = c("gray25", "gray50", "gray75")) +
  scale_xcolor_manual(values = c("gray25", "gray50", "gray75")) +
  xlim(c(-3.2, 3.2)) +
  labs(color = "Function f", y = "f(x)") +
  ggside(x.pos = "top") +
  geom_hline(yintercept = 0, color = "black") +
  theme(
    ggside.panel.border = element_blank(),
    ggside.panel.grid = element_blank(),
    ggside.axis.ticks = element_blank(),
    ggside.axis.text = element_blank(),
    ggside.panel.background = element_blank()
  )

ggsave(here("figures/Appendix_functions.pdf"), width = 9, height = 5)
