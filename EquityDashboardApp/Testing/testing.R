plot_dot(equity_purse$coin[[4]], dset = "Raw", iCode = "Poverty_agg", usel = c("01106"))


plot_dot(equity_purse$coin[[4]], dset = "Normalised", iCode = "Poverty_agg", usel = c("01106"))


plot_dot(equity_purse$coin[[4]], dset = "Raw", iCode = "Poverty_agg", usel = c("01106")) +
  ggplot2::annotate("text", x = udf$x, y = 1 + vert_adjust/100, label = udfi$uCode,
                    angle = 45, hjust = 0.3, size = 3.5)






vert_adjust = 0.5
coin = equity_purse$coin[[4]]
dset = "Raw"
iCode = "Poverty_agg" 
Level = NULL
usel = "01106"

iData <- get_data(coin, dset = dset, iCodes = iCode, Level = Level)
iCodes <- names(iData)[!(names(iData) %in% names(coin$Meta$Unit))]
iData_ <- iData[iCodes]
ind_data <- cbind(y = 1, iData_)
colnames(ind_data) <- c("y", "x")

plt <- ggplot2::ggplot(ind_data, ggplot2::aes(x=.data$x, y=.data$y)) +
  ggplot2::theme_minimal() +
  ggplot2::geom_point(
    color="blue",
    fill="blue",
    shape=21,
    alpha=0.5,
    size=3,
    #stroke = 0
  ) +
  ggplot2::ylab(NULL) +
  ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank())


# HIGHLIGHT UNITS ---------------------------------------------------------


ind_data_wcodes <- iData[c("uCode", colnames(iData_))]
  

udfi <- ind_data_wcodes[ind_data_wcodes$uCode %in% usel,]

udf <- data.frame(y = 1, udfi[[names(iData_)]])
colnames(udf) <- c("y", "x")
  

plt <- plt + ggplot2::geom_point(
    data = udf,
    ggplot2::aes(x=.data$x, y=.data$y),
    color="red",
    fill="blue",
    shape=21,
    alpha=0.7,
    size=3,
    stroke = 2
    )
  

ggplot2::annotate("text", x = udf$x, y = 1 + vert_adjust/100, label = udfi$uCode,
                        angle = 45, hjust = 0.3, size = 3.5)




xref_person <- as.data.table(xref_person)[,.(ssn,person_id)] %>%
  .[,seq:=seq(1,.N),by=c("ssn")] %>% 
  .[,sumseq:=sum(seq),by=c("ssn")] %>%
  .[sumseq > 1]


