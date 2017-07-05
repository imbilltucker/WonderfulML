11) Your boss sends you an email telling you that after buying \$5k of radio advertising sales rose to 5.5k units.  After \$10k, \$15k, and \$20k purchase, sales were 6.5, 11.5, and 8.5k respectively. Your boss wants a rough estimate for sales if he decides to spend \$37k.  What would you tell her?

x <- seq(5, 20, 5)
y <- c(5.5, 6.5, 11.5, 8.5)
df_live <- data.frame(ad_spend_k=x, sales_k=y)
df_live

## get reaquainted with variable number of input parameters...
varFunc <- function(x, y, ...) {
    args <- unlist(list(...))
    return(sum(c(x, y, args)))
}

varFunc(1,2,3,4,5,6,7)

## Returns single response based on linear model
## x_input - n-dimensional input vector to estimate response from
## params - first n-1 values are the slopes while last value is intercept
linearEstimate <- function(x_input, params=c(1, 1, 0)) {
    dim_input <- length(x_input)
    m_slopes <- params[1:dim_input]
    b_inter <- params[dim_input+1]
    #return(m_slopes) #, b_inter))
    return(sum(m_slopes * x_input) + b_inter)
}

linearEstimate(c(2, 3), c(3,7,14))  # test, should give 41, check!

# code below had some problems...

getEstimates <- function(est_vals_for=seq(5, 20, 5), est_type, estFun, ...) {
    est_params <- unlist(list(...))
    est <- estFun(spend_vals, slope, inter)
    return(est)
}

getRSS <- function(slope, inter, x1y2_df) {
    rss <- 0
    y_est <- (slope * x1y2_df[,1]) + inter
    
    return(y_est)
}

slope_est = 0.4
inter_est = 3.0
df_sales_est <- data.frame(ad_spend_k=seq(0, 20, 5), sales_k=getEstimate(seq(0, 20, 5), 'radio',
                                                                          linearEstimate, slope_est, inter_est))
df_sales_est

# These are the errors generated from the above code...

#Error in data.frame(ad_spend_k = seq(0, 20, 5), sales_k = getEstimate(seq(0, : could not find function "getEstimate"
#Traceback:
#
#1. data.frame(ad_spend_k = seq(0, 20, 5), sales_k = getEstimate(seq(0, 
# .     20, 5), "radio", linearEstimate, slope_est, inter_est))

# this is probabaly fine, but didn't because of bugs inearlier code...

library(ggplot2)
library(repr)
suppressMessages(suppressWarnings(library(repr)))
p <- ggplot(df_live, aes(x=ad_spend_k, y=sales_k))
p <- p + geom_point() + xlim(0, 20) + ylim(0, 20)
# add estimation line
p <- p + geom_line(data=df_sales_est, aes(x=ad_spend_k, y=sales_k), color='green')
# http://blog.revolutionanalytics.com/2015/09/resizing-plots-in-the-r-kernel-for-jupyter-notebooks.html
options(repr.plot.width=4, repr.plot.height=3)
print(p)