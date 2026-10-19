library(tidyverse)

# expected competence ----
beta <- function(x, n, k) {
  return(x^k * (1-x)^(n-k))
}

rho <- function(c, theta) {
  return(c * theta + (1-c) * (1-theta))
}

i1 <- function(a, b, theta, n, k) {
  return( integrate( function(c) c * beta(rho(c, theta), n, k), a, b) )
}

i2 <- function(a, b, theta, n, k) {
  return( integrate( function(c) beta(rho(c, theta), n, k), a, b) )
}

f <- function(h, a, b, theta_1, theta_2, n, k) {
  numerator   <- h * i1(a, b, theta_1, n, k)$value +
                 (1-h) * i1(a, b, theta_2, n, k)$value
  denominator <- h * i2(a, b, theta_1, n, k)$value +
                 (1-h) * i2(a, b, theta_2, n, k)$value
  return( numerator / denominator )
}

n <- 10
h <- 1/2
a <- 1/2
b <- 1
theta_1 = 3/4
theta_2 = 1/4

hs <- seq(0.1, 0.9, by = 0.2)

g <- function(x, h) map_dbl(x, function(k) f(h, a, b, theta_1, theta_2, n, k))

df <- expand_grid(h = hs, k = 0:n) |>
  mutate(y = map2_dbl(h, k, function(h, k) f(h, a, b, theta_1, theta_2, n, k)))

p1 <- ggplot() +
  map(hs, function (hh) geom_function(fun = g, args = list(h = hh), aes(colour = hh))) +
  geom_point(data = df, aes(k, y, colour = h)) +
  scale_x_continuous(limits = c(0, n), breaks = 0:n, minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  scale_colour_viridis_c(end = 0.9, option="plasma") +
  labs(x = "Reported heads", y = "Expected competence", colour = quote(italic(P)*(italic(H)))) +
  theme_minimal()


# likelihoods ----
df <- tibble(k = 0:n) |>
  mutate(
    `c = 1/2` = dbinom(k, n, 1/2),
    `c = 1`   = h * dbinom(k, n, theta_1) + (1 - h) * dbinom(k, n, theta_2)
  ) |>
  pivot_longer(-k, names_to = "competence", values_to = "lik") |>
  mutate(competence = fct_inorder(competence))

p2 <- ggplot(df, aes(k, lik, colour = competence)) +
  geom_linerange(aes(ymin = 0, ymax = lik), position = position_dodge(width = 0.1)) +
  geom_point(position = position_dodge(width = 0.1), size = 2) +
  scale_x_continuous(breaks = 0:n, minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(x = "Reported heads", y = "Probability", colour = NULL) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal()


# log-likelihoods ----
p3 <- ggplot(df, aes(k, lik)) +
  geom_line(aes(group = k), colour = "grey70") +
  geom_point(aes(colour = competence), size = 2) +
  scale_y_log10(labels = scales::label_number(), minor_breaks = NULL) +
  scale_x_continuous(breaks = 0:n, minor_breaks = NULL) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Reported heads", y = "Likelihood (log scale)", colour = NULL) +
  theme_minimal()

ggsave(filename = "p1.png", plot = p1, width = 6, height = 4, units = "in", dpi = 300)
ggsave(filename = "p2.png", plot = p2, width = 6, height = 4, units = "in", dpi = 300)
ggsave(filename = "p3.png", plot = p3, width = 6, height = 4, units = "in", dpi = 300)
