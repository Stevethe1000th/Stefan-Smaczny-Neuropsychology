# Stefan-Smaczny-Neuropsychology

All of the code in this branch is related to the pre-print (and submitted paper) called:
"Reducing alertness does not affect line bisection bias in neurotypical participants" (https://psyarxiv.com/c7ank/)


McIntosh_LB.zip:
# ---------------

This contains a psychopy implementation of the McIntosh Line Bisection task. 
(e.g., McIntosh, R. D., Schindler, I., Birchall, D., & Milner, A. D. (2005). Weights and measures: A new look at bisection behaviour in neglect. Cognitive Brain Research, 25(3), 833-850.)
This specific method of Line Bisection is capable of extracting two additional factors apart from the typical "directional bisection error" (DBE) (= cm from left end of line), the Endpoint Weightings Bias (EWB) and the Endpoint Weightings Sum (EWS).
The EWB reflects the patient's individual weighting of the left and right endpoints of horizontal lines, while the EWS reflects the consistency of bisection over different line types (i.e, length and position). It was found that these measures relate better to other typical neglect diagnostic tools, such as cancellation tasks, than the DBE.

It additionally contains a manual that explains how to install Psychopy, run the experiment correctly, and extract and interpret the data output.


Analysis_Code_Alertness_does_not.R:
# ---------------------------------

This is the statistical analysis code for the above experiment. While Dominik Bauder wrote an initial version of this code, it was strongly edited by me. More precisely, I wrote the statistical analyses and visualization of results following line 203 (# MAIN HYPOTHESES).

