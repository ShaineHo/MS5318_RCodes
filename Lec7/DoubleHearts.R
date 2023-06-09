# This is an exercise of running an R script
# There is no need to understand the following functions


t = seq(0, 2 * pi, length.out = 100)
x1 = 16 * sin(t)^3
y1 = 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)

x2 = x1 + 15
y2 = y1 - 5

plot(x1, y1, type = "l", xaxt = "n", yaxt = "n", xlab = "", 
     ylab = "", frame.plot = FALSE, xlim = c(-16, 40), 
     ylim = c(-21, 16), main = "To my dear ...")
polygon(x1, y1, col = "red", border = "hotpink")

polygon(x2, y2, col = "red", border = "hotpink")
arrows(x0 = -16, y0 = -5, x1 = 36, y1 = -5, col = "red", lwd = 5)


