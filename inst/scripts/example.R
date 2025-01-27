# Example script to run functions

library(devtools)
load_all()

# Load data
results <- format_results(path = "data/data-susiex-example-output-extra", ancestries = c("EUR", "AFR"))

# Plot Post hoc probability credible set manifest causal across ancestries (ie. `POST-HOC_PROB_POP${i}`):
plotAncestryCausal(results$summary, ancestries = c("EUR", "AFR"))

# Visualize the relationship between credible set characteristics and maximum PIP:
plotPurityPIP(results$summary)
