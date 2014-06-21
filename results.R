fw1_stats <- function() {
    m <- data.frame(I=c(1E0, 1E1,1E2,1E3),
                                  T=c(0.00652647, 0.08332276, 5.456374, 8.705324*60))
    m
}

plot_fm1 <- function() {
    ggplot(m, aes(x=m$I, y=m$T))
    +geom_point(size=3)
    +stat_smooth(method="lm")
    +scale_y_log10(labels=fmt(), breaks=c(m$T[1],m$T[2],m$T[3],m$T[4]))
    +scale_x_log10(breaks=c(1E0,1E1,1E2,1E3))
    +xlab("Quantidade de itens")
    + ylab("Tempo de processamento (s)")

    ggsave(file="latex/beamer/img/ixt.png")

}
