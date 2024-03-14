################################################################################
#              SCRIPT FOR REPRODUCING THE FIGURES IN THE PAPER                 #
#              "Toward Understanding the Disagreement Problem in               #
#                   Neural Network Feature Attribution"                        #
#                                                                              #
#                             Appendix:                                        #
#                                                                              #
################################################################################
library("here")
library("ggplot2")
library("ggsci")
library("ggside")
library("envalysis")

# Functions --------------------------------------------------------------------
source(here("utils/utils_syn_data.R"))

beta <- 1
x <- seq(-5.2, 5.2, length.out = 1000)

# Piece-wise linear function
y_pwlin <- dgp_pwlinear(x, beta, 0)$lp

# Non-Continuous
y_noncont <- dgp_nonlinear(x, beta, 0)$lp


data <- data.frame(
  x = x, 
  y = c(y_pwlin, y_noncont),
  type = rep(c("Piece-wise linear", "Non-continuous"), each = 1000)
)

data_density <- data.frame(
  x = c(rnorm(100000) - 2, rnorm(100000), rnorm(100000) + 2),
  type = rep(c("N(-2, 1)", "N(0, 1)", "N(2, 1)"), each = 100000)
)

ggplot(data) +
  geom_line(aes(x = x, y = y, color = type), linewidth = 1, alpha = 0.8) +
  geom_xsidehline(yintercept = 0) +
  geom_xsidedensity(data = data_density, mapping = aes(x = x, xfill = type, xcolor = type), 
                    linewidth = 0.2, alpha = 0.7) +
  guides(xfill = "none", xcolor = "none") + 
  scale_xfill_manual(values = c("gray25", "gray50", "gray75")) +
  scale_xcolor_manual(values = c("gray25", "gray50", "gray75")) +
  xlim(c(-4.5, 4.5)) +
  scale_color_manual(values = rev(pal_npg()(2))) +
  labs(color = "Function g(x)", y = "g(x)") +
  ggside(x.pos = "top") +
  theme_publish(base_family = "serif", base_size = 15, base_linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "black") +
  theme(
    ggside.panel.border = element_blank(),
    ggside.panel.grid = element_blank(),
    ggside.axis.ticks = element_blank(),
    ggside.axis.text = element_blank(),
    ggside.panel.background = element_blank()
  )

if (!dir.exists(here("figures/"))) dir.create(here("figures/"))
ggsave(here("figures/Sec_App_functions.pdf"), width = 7, height = 5)
