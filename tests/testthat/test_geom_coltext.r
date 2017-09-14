library(ggplot2)
library(statupinternal)

X <- data.frame(x = 1:5, y = sample(1:100, 5))
Y <- data.frame(x = 1:5, y = sample(1:100, 10), g = rep(c("a", "b"), each = 5))
Z <- data.frame(x = sample(letters[1:5], 100, TRUE))

ggplot(X, aes(x, y)) +
  geom_coltext(injust = 0)

ggplot(X, aes(x, y)) +
  geom_coltext(injust = 1)

ggplot(X, aes(x, y)) +
  geom_coltext(injust = -1)

ggplot(Y, aes(x, y)) +
  geom_coltext(injust = -0.5)


ggplot(Z, aes(x)) +
  geom_coltext(stat = "count", injust = 1.5)

ggplot(X, aes(x, y)) +
  geom_bartext(injust = 1.5)


ggplot(Y, aes(x, y, label = percentify(y), fill = g)) +
  geom_coltext(injust = -0.5) +
  coord_flip()

ggplot(Y, aes(x, y,
              label = percentify(y),
              fill = g,
              injust = 1 - 2*(g == "a"))) +
  geom_coltext(just_mirror = FALSE, position = "dodge") +
  coord_flip()

ggplot(Y, aes(x, y,
              label = percentify(y),
              fill = g)) +
  geom_coltext(aes(injust = 1.1*(-1)^(g == "a")),
               just_mirror = TRUE,
               position = "stack") +
  coord_flip()


ggplot(Y, aes(x, y, label = percentify(y))) +
  geom_coltext(injust = -1.1) +
  coord_flip() +
  facet_wrap(~g)


ggplot(Y, aes(x, y, label = percentify(y), fill = g)) +
  geom_coltext(injust = 0, position = "fill", center_text = TRUE)
