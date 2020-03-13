# Functions for upscaling gamma distributions based on m and G
# (inputs alpha, lambda and G are vectors with values for different distributions;
# if lambda is left at the default value of 1, then alpha corresponds to m)

# Variance (division by n^2)
Var_Bmean_from_m_G <- function(alpha, lambda=1, G){
  return(sum((1/alpha+1)*(G/lambda)^2, na.rm=T) / (max(c(length(alpha), length(lambda), length(G)))^2))
}
