# Step 1 -- initial layer
library(ggplot2)
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Petal.Length))

# Step 2 -- Geometric layer
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point()

# Step 3 -- Aesthtics inside Geometric layer
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(aes(colour = Species), size = 3)

# Step 4 -- Add labels
ggplot(data = iris,mapping = aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(aes(colour = Species), size = 3) +
  labs(
    title = "Scatter plot of Iris flower petal and sepal lengths",
    x = "Sepal Length",
    y = "Petal Length",
    caption = "Source: Fisher's Iris data set"
  )

# Step 5 -- Statistical layer
ggplot(data = iris,mapping = aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(aes(colour = Species), size = 3) +
  labs(
    title = "Scatter plot of Iris flower petal and sepal lengths",
    x = "Sepal Length",
    y = "Petal Length",
    caption = "Source: Fisher's Iris data set"
  ) +
  stat_smooth()

# Step 6 -- Customise theme
ggplot(data = iris,mapping = aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(aes(colour = Species), size = 3) +
  labs(
    title = "Plotting species of Iris flowers with ggplot2",
    x = "Sepal Length",
    y = "Petal Length",
    caption = "Source: Fisher's Iris data set"
  ) +
  stat_smooth() +
  theme_light()
