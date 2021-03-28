# https://stats.stackexchange.com/questions/437514/replicating-a-tweedie-corrected-experiment-from-computer-age-statistical-inferen
# simulation parameters
N <- 1e3  # number of observations per dataset
nsims <- 100  # number of datasets
set.seed(123)

# this part should be repeated nsims times
create_smallest_and_largest <- function(iteration) {
  # mu_i is distributed as a standard exponential random variable
  mu_i <- rexp(N)
  # z_i is distributed as a normal normal variable with mean mu_i, variance 1
  z_i <- rnorm(N, mean = mu_i, sd = 1)
  
  # create bins by cut()
  # count nr of z_i in bin 
  # last mutate derives the middle of the bin as mean(left_bound, right_bound)
  counts <- cut(z_i, 30) %>%
    as_tibble_col("bins") %>%
    count(bins) %>%
    mutate(logn = log(n)) %>%
    mutate(z = map_dbl(str_extract_all(bins, "-?\\d+\\.?\\d+"), 
                       function(x) mean(as.numeric(x)) ) )
  
  # visualization of counts + polynomial glm (J = 5) with log link fit
  # counts %>%
  #   ggplot(aes(z, n)) +
  #   geom_point() +
  #   geom_smooth(method = "glm", formula = y ~ poly(x, 5), se = F, 
  #               method.args = list(family=poisson(link="log")))
  # 
  # glm fit
  fit <- glm(n ~ poly(z, 5, raw = T), family = poisson(link = "log"), data = counts)  
  
  # retrieve coefficients
  betas <- coef(fit)
  
  # l(z) or log(f(z)), the log of the estimated density
  # TODO: my f(z) does not integrate to 1 yet! 
  l_of_z <- function(x){
    c(1, poly(x, 5, raw=T)) %*% coef(fit)
  }
  
  # derivative of l(z) or l'(z)
  l_prime <- function(x){
    x <- c(1, poly(x, degree = 4, raw=T)) 
    
    as.vector(x %*% (betas[2:length(betas)] * 1:5 ))
  }
  
  # take 20 smallest z_i with corresponding mu_i, and calculate correction
  
  smallest <- tibble(mu_i, z_i, iteration) %>%
    slice_min(order_by = z_i, n = 20) %>%
    rowwise() %>%
    mutate(corrected = z_i + l_prime(z_i))
  
  # idem but for largest 20
  largest <- tibble(mu_i, z_i, iteration) %>%
    slice_max(order_by = z_i, n = 20) %>%
    rowwise() %>%
    mutate(corrected = z_i + l_prime(z_i))
  
  list(smallest = smallest, largest = largest)
}
smallest_results <- tibble()
largest_results <- tibble()
for (i in 1:nsims){
  results <- create_smallest_and_largest(i)
  
  smallest_results <- smallest_results %>%
    bind_rows(results$smallest)
  
  largest_results <- largest_results %>%
   bind_rows(results$largest)
}


smallest_results %>%
  pivot_longer(-iteration) %>%
  ggplot(aes(value)) +
  geom_density(aes(fill = name), alpha = 0.5)

smallest_results %>%
  mutate(d_i = z_i-mu_i,
         d_hat = corrected - mu_i) %>%
  pivot_longer(c(d_i, d_hat)) %>%
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = 0.4) +
  labs(x = "bias",
       fill = "differences") +
  geom_vline(xintercept = 0)

largest_results %>%
  mutate(d_i = z_i-mu_i,
         d_hat = corrected - mu_i) %>%
  pivot_longer(c(d_i, d_hat)) %>%
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = 0.4) +
  labs(x = "bias",
       fill = "differences") +
  geom_vline(xintercept = 0)



### d (width of bins)
d <- counts %>% 
  rowwise() %>% 
  mutate(dif = as.numeric(str_extract_all(bins, "-?\\d+\\.?\\d+")[[1]][1]) 
         - as.numeric(str_extract_all(bins, "-?\\d+\\.?\\d+")[[1]][2])) %>% pull(dif) %>% mean()
d <- abs(d)
