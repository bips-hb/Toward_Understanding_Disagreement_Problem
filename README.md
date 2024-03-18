
# Toward Understanding the Disagreement Problem

This repository provides the code to reproduce results in the paper 
"Toward Understanding the Disagreement Problem in Neural Network Feature 
Attribution" submitted to the 2nd World Conference on eXplainable 
Artificial Intelligence (XAI).

The structure is as follows:

* `Sec_1_Introduction.R` reproduces Figure 1 in the introduction, which shows
the disagreement of popular feature attribution methods on the COMPAS dataset.

* `Sec_3_Understanding_the_Distribution.R` reproduces the figures from the
running example in Section 3.

* `Sec_4_Simulations.R` runs the simulation study (can last up to several days)
and creates the figures in Section 4.

* `Sec_Appendix.R` creates the Figure 7 showing the transformations for the 
non-linear effects.

All created figures are saved in the `figures/` folder. However, the final 
figures used in the paper and the results from the simulation study of Section 
4 are stored in the `results/` folder.
