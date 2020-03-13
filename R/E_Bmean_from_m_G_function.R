# Functions for upscaling gamma distributions based on m and G
# (inputs alpha, lambda and G are vectors with values for different distributions;
# if lambda is left at the default value of 1, then alpha corresponds to m)

# Expected value
E_Bmean_from_m_G <- function(alpha, lambda=1, G){
  return(mean((1/alpha+1)*(G/lambda), na.rm=T))
}
