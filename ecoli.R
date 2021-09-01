library(limma)
library(affy)

# Download files from: http://bioinf.wehi.edu.au/limma/data/ecoli-lrp.zip

# Read in targets
targets <- readTargets("ecoli-lrp/targets.txt")
targets

eset <- justRMA(filenames = targets$FileName, 
                celfile.path = "ecoli-lrp")

colnames(eset) <- row.names(targets)

head(exprs(eset))

plotMDS(eset)
exprs(eset)


library(tidyverse)

tibeset <- t(as.matrix(eset))


Y <- factor(targets$Strain)

testresults <- matrix(nrow=ncol(tibeset),  
                         ncol=2 ) 
rownames(testresults) <- colnames(tibeset)
colnames(testresults) <- c("statistic", "df")

for (i in seq(1,ncol(tibeset))){
  test <- t.test(tibeset[,i]~Y)
  testresults[i, "statistic"] <- test$statistic
  testresults[i, "df"] <- test$parameter
}

# step 1: make counts per bin
counts <- cut(testresults[,"statistic"], 30) %>%
  as_tibble_col("bins") %>%
  count(bins) %>%
  mutate(t = map_dbl(str_extract_all(bins, "-?\\d+\\.?\\d+"), function(x) mean(as.numeric(x)) ) )

# step 2: fit glm
fit <- glm(n ~ poly(t, 5, raw = T), family = poisson(link = "log"), data = counts) 
betas <- coef(fit)

# step 3: calculate l'()
l_prime <- function(x){
  x <- c(1, poly(x, degree = 4, raw=T)) 
  
  as.vector(x %*% (betas[2:length(betas)] * 1:5 ))
}

var_t <- var(testresults[,"statistic"])

# step 4: calculate corrected test statistics and corresponding p-values, assuming t is *approx* (cf. Efron for this) normally distributed
out <- as_tibble(testresults) %>%
  rowwise() %>%
  mutate(corrected_t = statistic + var_t*l_prime(statistic),
         corrected_p = 2*(1-pt(abs(corrected_t), df)))


out %>%
  bind_cols(as_tibble_col(colnames(tibeset), "name")) %>%
  filter(corrected_p < 0.05)

