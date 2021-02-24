


# ggforce version

# dataframe that indicates which grid locations are on/off
# could use previous rotate versions to direct this
# also, indicate which shape is in each
# so, for now, either square or triangle
# then based on each '1' in the matrix, compute 3 or 4 coords for the plot

# dataframe for each shape used
# if square, need 4 coords
# if triangle, need 3 coords and something to determine where to start
# relative to neighboouring squares/triangles

# need function to set the max plot coords
# need function to compute area and perimeter
# need functions that set x/y coords for each location in matrix



library(tidyverse)
library(ggforce)


ids <- factor(c("1", "2"))

values <- data.frame(
  id = ids,
  value = c(1, 2)
)

shape <- data.frame(
  id = rep(ids, each = 4),
  x = c(0, 1, 1, 0, 0, 1, 1, 0),
  y = c(0, 0, 1, 1, 1, 1, 2, 2)
)

datasquare <- merge(values, shape, by = c("id"))




ggplot(datasquare, aes(x = x, y = y)) +
  geom_shape(aes(group = id,
                 #fill = value
  ),
  colour = "white",
  fill = "blue"
  #expand = unit(-0.25, 'mm')
  ) +
  ylim(0, 10) + xlim(0, 10) +
  theme_void()
#+
# scale_colour_manual(palette = c("blue", "green"))




###########################################################
# test x-agon


# Lets make some data
circles <- data.frame(
  x0 = rep(1:3, 3),
  y0 = rep(1:3, each = 3),
  r = seq(0.1, 1, length.out = 9)
)

# Behold the some circles
ggplot() +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles)

# Use coord_fixed to ensure true circularity
ggplot() +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles) +
  coord_fixed()



ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r, fill=r), data=circles, n=5) +
  coord_fixed()




###############################################



rotate  <- function(x, clockwise=T) {
  if (clockwise) { t( apply(x, 2, rev))
  } else {apply( t(x),2, rev)}
}

# NOTE: restarting this for a different project



# 1. transpose t() will mirror around y = x line
# 2. rotate?
# 3. mirror?




library(tidyverse)



# 3 WORKS FOR PLOTTING WHAT I WANT
t1 <- data.frame(
  "M1" <- c(1, 1, 1, 1, 1, 1),
  "M2" <- c(1, 1, 0, 0, 0, 0),
  "M3" <- c(1, 1, 0, 0, 0, 0),
  "M4" <- c(1, 1, 0, 0, 0, 0),
  "M5" <- c(1, 1, 0, 0, 0, 0),
  "M6" <- c(0, 0, 0, 0, 0, 0)
)
colnames(t1) <- c("M1", "M2", "M3", "M4", "M5", "M6")


# mirror around y = x
t2 <- data.frame(t(t1))
colnames(t2) <- c("M1", "M2", "M3", "M4", "M5", "M6")

# mirror around horizontal line
x1 <- t1[order(nrow(t1):1),] #invert row order
colnames(x1) <- c("M1", "M2", "M3", "M4", "M5", "M6")


# mirror around vertical line
x2 <- t1[, order(ncol(t1):1)]

# 90 deg rotate, clockwise
x3 <- data.frame(rotate(t1))
#colnames(x3) <- c("M1", "M2", "M3", "M4", "M5", "M6")

# 90 deg rotate, counterclockwise
x4 <- data.frame(rotate(t1, clockwise = FALSE))
#colnames(x3) <- c("M1", "M2", "M3", "M4", "M5", "M6")

# 180 deg rotate
x5 <- data.frame(rotate(rotate(t1)))








t1$id <- 1:nrow(t1)
m_t1 <- reshape2::melt(t1, id.var="id")
t1$id <- NULL


t2$id <- 1:nrow(t2)
m_t2 <- reshape2::melt(t2, id.var="id")
t2$id <- NULL


x1$id <- 1:nrow(x1)
m_x1 <- reshape2::melt(x1, id.var="id")
x1$id <- NULL


x2$id <- 1:nrow(x2)
m_x2 <- reshape2::melt(x2, id.var="id")
x2$id <- NULL

x3$id <- 1:nrow(x3)
m_x3 <- reshape2::melt(x3, id.var="id")
x3$id <- NULL


x4$id <- 1:nrow(x4)
m_x4 <- reshape2::melt(x4, id.var="id")
x4$id <- NULL


x5$id <- 1:nrow(x5)
m_x5 <- reshape2::melt(x5, id.var="id")
x5$id <- NULL







perimeter <- sum(
  sum(t1$M1),
  sum(t1$M6),
  sum(t1$M1[1], t1$M2[1], t1$M3[1], t1$M4[1], t1$M5[1], t1$M6[1])*2,
  abs(sum(t1$M1)-sum(t1$M2)),
  abs(sum(t1$M2)-sum(t1$M3)),
  abs(sum(t1$M3)-sum(t1$M4)),
  abs(sum(t1$M4)-sum(t1$M5)),
  abs(sum(t1$M5)-sum(t1$M6))
)


# abs(sum(temp$M1)-sum(temp$M2)) for each column pair


# m1 will be left, start from bottom
ggplot(m_t1, aes(x=variable,
                 y=id,
)) +
  geom_tile(aes(fill = factor(value)), colour = "white") +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("white", "darkblue")) +
  labs(title = paste0("AREA = ", sum(t1), "  PERIMETER = ", perimeter))
cowplot::ggsave2(filename = "orig.png",
                 height = 4, width = 4, dpi = 1600)


ggplot(m_t2, aes(x=variable,
                 y=id,
)) +
  geom_tile(aes(fill = factor(value)), colour = "white") +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("white", "darkblue")) +
  labs(title = paste0("AREA = ", sum(t2), "  PERIMETER = ", perimeter))
cowplot::ggsave2(filename = "xy_mirror.png",
                 height = 4, width = 4, dpi = 1600)



ggplot(m_x1, aes(x=variable,
                 y=id,
)) +
  geom_tile(aes(fill = factor(value)), colour = "white") +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("white", "darkblue")) +
  labs(title = paste0("AREA = ", sum(x1), "  PERIMETER = ", perimeter))
cowplot::ggsave2(filename = "horiz_mirror.png",
                 height = 4, width = 4, dpi = 1600)

ggplot(m_x2, aes(x=variable,
                 y=id,
)) +
  geom_tile(aes(fill = factor(value)), colour = "white") +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("white", "darkblue")) +
  labs(title = paste0("AREA = ", sum(x2), "  PERIMETER = ", perimeter))
cowplot::ggsave2(filename = "vert_mirror.png",
                 height = 4, width = 4, dpi = 1600)



ggplot(m_x3, aes(x=variable,
                 y=id,
)) +
  geom_tile(aes(fill = factor(value)), colour = "white") +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("white", "darkblue")) +
  labs(title = paste0("AREA = ", sum(x2), "  PERIMETER = ", perimeter))
cowplot::ggsave2(filename = "90_clock.png",
                 height = 4, width = 4, dpi = 1600)


ggplot(m_x4, aes(x=variable,
                 y=id,
)) +
  geom_tile(aes(fill = factor(value)), colour = "white") +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("white", "darkblue")) +
  labs(title = paste0("AREA = ", sum(x2), "  PERIMETER = ", perimeter))
cowplot::ggsave2(filename = "90_counterclock.png",
                 height = 4, width = 4, dpi = 1600)



ggplot(m_x5, aes(x=variable,
                 y=id,
)) +
  geom_tile(aes(fill = factor(value)), colour = "white") +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("white", "darkblue")) +
  labs(title = paste0("AREA = ", sum(x2), "  PERIMETER = ", perimeter))
cowplot::ggsave2(filename = "180.png",
                 height = 4, width = 4, dpi = 1600)











#####################################################################





# FIRST: re-do with ggforce to see if it is doable and more flexible
# NEXT: how to specify the grid and also, how to evaluate area and perimeter
# area would be the sum of all the columns (sum(temp) without id col)
# perimeter is much harder though, need to think about it
# length of first column (m1 = 6)
# length of last column (m6 = 5)
# number of columns with 1 in first value (= 6)
# number of columns with at least 1 value (= 6)
# difference of each adjacent column
# m1/m2 = 2
# m2/m3 = 0
# m3/m4 = 1
# m4/m5 = 1
# m5/m6 = 1

# total: 6 + 5 + 6 + 6 + 2 + 0 + 1 + 1 + 1


# so, create an area function and a perimeter function
# also, create functions to generate the grid array
# that checks for no gaps in the grids
# determine number of total rows/cols
# determine number of cols in this item
# determine number in col1, then col 2, etc, all other non-cols = 0
# could technically use middle 3 cols or whatever, sort that out later
# can also flip image at 90 degree increments, or mirror
# later, change how to fill the colours






























# 1
#assign an id to plot rows to y-axis
temp$id <- 1:nrow(temp)

#reshape to long
m_temp <- melt(temp, id.var="id")


p1 <- ggplot(m_temp, aes(x=variable,
                         y=id,fill=factor(value))) +
  geom_tile()

p1


# 2
library(ggplot2)

x <- data.frame(
  "a" <- c(1, 0, 1),
  "b" <- c(1, 1, 0),
  "c" <- c(0, 0, 1)
)
colnames(x) <- c("a", "b", "c")




ggplot(x,
       aes(x = a, y = b),
       fill = factor(col)) +
  geom_tile()

# dd <- expand.grid(x = 1:ncol(temp), y = 1:nrow(temp))
# dd$col <- unlist(c(temp))
# ggplot(dd, aes(x = x, y = y, fill = factor(col))) + geom_tile()











# each item has several cells:
# 1. a grid of N x N
# 2. a set of 8 response options

# each cell has elements:
# 1. a grid of M x M or just a single square area


# each stimulus creation needs:
# 1. define the area of one cell as either a single box or a grid of 3x3
# 2. define the area of the item as a grid
# 3. define the rule used
# 4. define the shape(s) used, colours used, orientations, etc
# a. each shape has its dimensions defined and positions relative to the
# other shapes
# 5. creation of constituent parts that are plotted together at the end

# functions needed: this is maybe not how this will work actually
# create a cell that could be either a single box or grid of items
# creation of the grid is also contained in this function
# maybe return different data.frames based on the shape? not sure how
# draw shapes based on certain arguments
# square, circle, arc, line, complex shapes too
# set the space based on plot size arguments
# set the rule to be applied
#





library(ggforce)



# starting parameters
shape <- "square"
element <- 1
n <- 3  # rows
m <- 3  # columns
size <- 1
axis_bounds <- n + m + 1  # this will need to be changed most likely

x_increment <- size * 2
y_increment <- size * 2

if (shape == "square") {
  num_of_coords <- 4
}

grid <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(grid) <- c("element", "shape", "x", "y")



# set initial y coord
y_start <- -1


for (i in 1:n) {

  # reset initial x coord
  x_start <- -1

  for (j in 1:m) {

    # order of points: bottom left, bottom right, top right, top left
    grid_cell <- data.frame(
      element = c(rep(element, num_of_coords)),
      shape = c(rep(shape, num_of_coords)),
      x = c(x_start,
            x_start+size,
            x_start+size,
            x_start),
      y = c(y_start-size,
            y_start-size,
            y_start,
            y_start)
    )

    grid <- bind_rows(grid, grid_cell)

    element <- element + 1


    # increment x to move grid cell right
    x_start <- x_start - x_increment



  }  # end m loop


  # decrement y to move grid cell down
  y_start <- y_start - y_increment

}  # end n loop



# to alter the grids, have some sort of draw_elements boolean
# 1 = true, rest = false means only top left is drawn, for example


elements_to_draw <- c(1,2)

elements_to_draw <- c(1,4)

elements_to_draw <- c(1:9)

grid_to_draw <- grid %>%
  filter(element %in% elements_to_draw)


# remove grid, axes, axis labels, etc
ggplot(grid_to_draw, aes(x = x, y = y)) +
  geom_shape(aes(group = element)
             #expand = unit(1, 'cm'), # would add a border
             #radius = unit(0.5, 'cm')  # not sure what radius does
  ) +
  #geom_polygon(fill = 'black') +
  xlim(-5, 2) + ylim(-6, 1) +
  theme_void() #+
# theme(axis.title.x = element_blank()) +
# theme(axis.title.y = element_blank()) +
# theme(axis.text = element_blank()) +
# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



# remove axes and labels and gridlines






# examples


# -------------------------------------------------------------------------



# Expand and round
ggplot(shape, aes(x = x, y = y)) +
  geom_shape(expand = unit(1, 'cm'), radius = unit(0.5, 'cm')) +
  geom_polygon(fill = 'red') +
  xlim(0, 3) + ylim(0, 3) + theme_minimal()

# Contract
ggplot(shape, aes(x = x, y = y)) +
  geom_polygon(fill = 'red') +
  geom_shape(expand = unit(-1, 'cm'))

# Only round corners
ggplot(shape, aes(x = x, y = y)) +
  geom_polygon(fill = 'red') +
  geom_shape(radius = unit(1, 'cm'))










# Adapted from geom_polygon documentation
ids <- factor(c("1.1", "2.1", "3.1",
                "1.2", "2.2", "3.2",
                "1.3", "2.3", "3.3"))

# values = colors in this case, so no important
values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5, 2.9, 2.8, 2.7)
)

# each id has 4 x,y coords, the points of the polygon
positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(5, 5, 5, 5, 7, 7, 7, 7, 9, 9, 9, 9, 2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(5, 5, 5, 5, 7, 7, 7, 7, 9, 9, 9, 9, -0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

# this is what you are plotting, the previous code is a way to create it
datapoly <- merge(values, positions, by = c("id"))

ggplot(datapoly, aes(x = x, y = y)) +
  geom_shape(aes(fill = value, group = id), expand = unit(-3, 'mm'))









# Adapted from geom_polygon documentation
ids <- factor(c("1.1", "2.1", "3.1",
                "1.2", "2.2", "3.2",
                "1.3", "2.3", "3.3"))

# values = colors in this case, so no important
values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5, 2.9, 2.8, 2.7)
)

# each id has 4 x,y coords, the points of the polygon
positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(5, 5, 5, 5),
  y = c(5, 5, 5, 5)
)

# this is what you are plotting, the previous code is a way to create it
datapoly <- merge(values, positions, by = c("id"))

ggplot(datapoly, aes(x = x, y = y)) +
  geom_shape(aes(fill = value, group = id), expand = unit(-3, 'mm'))
