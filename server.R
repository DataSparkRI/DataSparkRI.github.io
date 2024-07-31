#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

circ_bar_t = 12
circ_bar_st = 8
circ_bar_cap = 5
cap_size = 5
bar_draft_size = 42
bar_draft_text = "DRAFT"
circ_draft_size = 10
circ_draft_text = "DRAFT\n\nDRAFT\n\nDRAFT"

# "#CD5B45","#CB7969","#C6978E"
# "#ef93e9","#F5BEF2","#FCE9FB"
# "#FFBC42", "#FA824C", "#FF9B71"
# "#fa8a11", "#fab669", "#fcdcb8"
indexColors <- data.frame(index = c("conditions","resources","experiences","outcomes"),
                          lowcolor = c("#3ed1df","#fa8a11","#0eb393","#541690"),
                          midAcolor= c("#65DAE5","#ffa23d","#3EC2A9","#7645A6"),
                          midcolor = c("#8BE3EC","#fab669","#6ED1BE","#9873BC"),
                          midBcolor= c("#B2EDF2","#ffca8f","#9FE1D4","#BBA2D3"),
                          highcolor = c("#D8F6F9","#fcdcb8","#9FE1D4","#BBA2D3")) %>% data.table()
# c("#B2EDF2","#F9D4F6","#9FE1D4","#BBA2D3")
# c("#D8F6F9","#FCE9FB","#CFF0E9","#DDD0E9")
indicatorColors <- data.frame(indicator = c("stinspnd", "engage", "commenvhlth","currbrdth","climate","tchefficacy","proflearn","leadership","fteperpupil","perpplspend","collegecareer"),
                              color = c("#3ed1df","#0eb393","#3ed1df","#0eb393","#0eb393","#fa8a11","#fa8a11","#fa8a11","#fa8a11","#fa8a11","#541690"),
                              color2 = c("#8BE3EC","#6ED1BE","#8BE3EC","#6ED1BE","#6ED1BE","#fab669","#fab669","#fab669","#fab669","#fab669","#9873BC")) %>% data.table()

##### Saved Older Versions -------------------------------------------------------------


# bar_plot_Index_og <- function(input_purse,sy_var,schoolcode,index_var,index_lowcolor,index_midcolor,index_highcolor) {
#   
#   iData <- get_data(input_purse$coin[[sy_var]], dset = "Aggregated", iCodes = index_var, also_get = "uName")
# 
#   iData$plbs <- iData$uCode
#   # get iMeta
#   iMeta <- input_purse$coin[[sy_var]]$Meta$Ind
#   # get child codes
#   iCodes_ch <- iMeta$iCode[iMeta$Parent == "index"]
#   # remove NAs
#   iCodes_ch <- iCodes_ch[!is.na(iCodes_ch)]
#   # get data
#   iData_ch <- get_data(input_purse$coin[[sy_var]], dset = "Aggregated", iCodes = iCodes_ch)
#   iData_ch_l <- lengthen(iData_ch, cols = iCodes_ch)
#   names(iData_ch_l)[names(iData_ch_l) == "name"] <- "Component"
#   # merge onto iData
#   iData <- merge(iData, iData_ch, by = "uCode")
#   # scale children to add up to parent score
#   iData$scale_fac <- iData[["index"]]/rowSums(iData[iCodes_ch])
#   iData[iCodes_ch] <- sapply(iData[iCodes_ch], `*`, iData$scale_fac)
#   # make long for plotting, and rename some things
#   iData <- lengthen(iData, cols = iCodes_ch)
#   names(iData)[names(iData) == "name"] <- "Component"
#   names(iData)[names(iData) == "index"] <- paste0("index", "2")
#   names(iData)[names(iData) == "Value"] <- "index"
#   iData <- merge(iData, iData_ch_l, by = c("uCode","Component"))
#   
#  p <- ggplot2::ggplot(iData, ggplot2::aes(x = stats::reorder(.data[["plbs"]], -1*.data[["index"]]),
#                                       y = .data[["index"]],
#                                       label = .data[["plbs"]],
#                                       fill = .data[["Component"]],
#                                       alpha = uCode == schoolcode,
#                                       tooltip = paste0("<span style='font-size:16px;'>",.data[["uName"]],"</span>", "<br>",
#                                                     index_var,": ","<b>",round(.data[["index2"]]),"</b>", "<br>",
#                                                     case_when(.data[["Component"]] == "commenvhlth" ~ "Community & Environmental Health: ",
#                                                               .data[["Component"]] == "engage" ~ "Engagement in Schooling: ",
#                                                               .data[["Component"]] == "stinspnd" ~ "Student Instructional Support Needs: ",
#                                                               .data[["Component"]] == "climate" ~ "School Climate: ",
#                                                               .data[["Component"]] == "currbrdth" ~ "Curricular Breadth: "),
#                                                     "<b>",round(.data[["Value"]]),"</b>"
#                                                     )))+
#    ggplot2::ggtitle(iData$uName[iData$uCode == schoolcode], 
#                     subtitle = paste0("Index Ranking",", ",case_when(sy_var == 1 ~ "2017-18",
#                                                                      sy_var == 2 ~ "2018-19",
#                                                                      sy_var == 3 ~ "2019-20",
#                                                                      sy_var == 4 ~ "2020-21"))) +
#     geom_bar_interactive(stat = "identity", position = "stack") +
#     ggplot2::theme_minimal() + 
#     scale_alpha_manual(values = c(0.3,1),guide = "none") +
#     scale_fill_manual(values = c(index_lowcolor,index_midcolor,index_highcolor),
#                       labels = c("Community & Environmental Health",
#                                  "Engagement in Schooling",
#                                  "Student Instructional Support Needs")) +
#     theme_minimal() +
#     labs(caption = paste0("Generated: ",today(), " (", purse_file,")")) +
#     coord_cartesian(ylim = c(0,105)) +
#     scale_y_continuous(expand = c(0,0)) +
#     theme(plot.title = element_text(face = "bold", hjust = 0.5, size = circ_bar_t, color ="#656565"),
#           plot.subtitle = element_text(hjust = 0.5, size = circ_bar_st, color ="#656565"),
#           legend.title=element_blank(), 
#           legend.text=element_text(size=7),
#           legend.position = c(.85, .9),
#           legend.key.size = unit(0.4, 'cm'),
#           axis.text.x = element_text(angle = 45, hjust=1, size = 8),
#           axis.text.y = element_text(size = 8),
#           plot.caption = element_text(color ="lightgrey", size = cap_size),
#           axis.title.x = element_blank(),
#           axis.title.y = element_blank(),
#           panel.grid.major.x = element_blank(),
#           axis.ticks.x = element_line())  +
#     geom_text(aes(label = round(.data[["index2"]],0), y = .data[["index2"]] + 3), 
#               color = index_lowcolor, fontface="bold", size = 3,
#               data = iData[iData$uCode == schoolcode,]) +
#    annotate("text", x = 25, y = 50, label = bar_draft_text, size = bar_draft_size, color = "black", alpha = 0.1)
#    
#    x <- girafe(ggobj = p, height_svg = 5, width_svg = 10)
#    x <- girafe_options(x, opts_tooltip(offx = 20, offy = -10,
#                                        use_fill = F, use_stroke = F,
#                                        delay_mouseout = 1000,
#                                        zindex = 9999,
#                                        css = case_when(index_var == "Outcomes"~"background-color: #9873BC; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 272px; padding: 6px;",
#                                                        index_var == "Conditions"~"background-color: #8BE3EC; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 272px; padding: 6px;",
#                                                        index_var == "Resources"~"background-color: #fab669; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 272px; padding: 6px;",
#                                                        index_var == "Experiences"~"background-color: #6ED1BE; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 272px; padding: 6px;")),
#                        opts_toolbar(saveaspng = FALSE),
#                        options = list(opts_sizing(rescale = FALSE)))
#    return(x)
#    
# }

bar_plot_Index <- function(input_purse,sy_var,schoolcode,index_var,index_lowcolor,index_midAcolor,index_midcolor,index_midBcolor,index_highcolor,charter_var) {
  
  iData <- get_data(input_purse$coin[[sy_var]], dset = "Aggregated", iCodes = index_var, also_get = c("uName","subtype_group")) %>% as.data.frame()
  iData <- iData[iData$subtype_group %in% charter_var,]
     
  
  iData$plbs <- iData$uCode
  # get iMeta
  iMeta <- input_purse$coin[[sy_var]]$Meta$Ind
  # get child codes
  iCodes_ch <- iMeta$iCode[iMeta$Parent == index_var]
  # remove NAs
  iCodes_ch <- iCodes_ch[!is.na(iCodes_ch)]
  # get data
  iData_ch <- get_data(input_purse$coin[[sy_var]], dset = "Aggregated", iCodes = iCodes_ch)
  iData_ch_l <- lengthen(iData_ch, cols = iCodes_ch)
  names(iData_ch_l)[names(iData_ch_l) == "name"] <- "Component"
  # merge onto iData
  iData <- merge(iData, iData_ch, by = "uCode")
  # scale children to add up to parent score
  iData$scale_fac <- iData[[index_var]]/rowSums(iData[iCodes_ch],na.rm=T)
  iData[iCodes_ch] <- sapply(iData[iCodes_ch], `*`, iData$scale_fac)
  # make long for plotting, and rename some things
  iData <- lengthen(iData, cols = iCodes_ch)
  names(iData)[names(iData) == "name"] <- "Component"
  names(iData)[names(iData) == index_var] <- paste0("index", "2")
  names(iData)[names(iData) == "Value"] <- "index"
  
  iData <- merge(iData, iData_ch_l, by = c("uCode","Component"))
  
  p <- ggplot2::ggplot(iData, ggplot2::aes(x = stats::reorder(.data[["plbs"]], -1*.data[["index2"]]),
                                           y = .data[["index"]],
                                           label = .data[["plbs"]],
                                           fill = .data[["Component"]],
                                           alpha = uCode == schoolcode,
                                           tooltip = paste0("<span style='font-size:16px;'>",.data[["uName"]],"</span>", "<br>",
                                                            index_names[IndexShort==index_var]$IndexLong,": ","<b>",round(.data[["index2"]]),"</b>", "<br>",
                                                            indicator_names[IndicatorShort%in%.data[["Component"]]]$IndicatorLong,": ",
                                                            "<b>",round(.data[["Value"]]),"</b>"
                                           )))+
    ggplot2::ggtitle(iData$uName[iData$uCode == schoolcode], 
                     subtitle = paste0("Index Ranking",", ",case_when(sy_var == 1 ~ "2017-18",
                                                                      sy_var == 2 ~ "2018-19",
                                                                      sy_var == 3 ~ "2019-20",
                                                                      sy_var == 4 ~ "2020-21"))) +
    geom_bar_interactive(stat = "identity", position = "stack") +
    ggplot2::theme_minimal() + 
    scale_alpha_manual(values = c(0.3,1),guide = "none") +
    scale_fill_manual(values = c(index_lowcolor,index_midAcolor,index_midcolor,index_midBcolor,index_highcolor))+
    theme_minimal() +
    labs(caption = paste0("Generated: ",today(), " (", purse_file,")")) +
    coord_cartesian(ylim = c(0,105)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = circ_bar_t, color ="#656565"),
          plot.subtitle = element_text(hjust = 0.5, size = circ_bar_st, color ="#656565"),
          legend.title=element_blank(), 
          legend.text=element_text(size=7),
          legend.position = c(.85, .9),
          legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(angle = 45, hjust=1, size = 8),
          axis.text.y = element_text(size = 8),
          plot.caption = element_text(color ="lightgrey", size = cap_size),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_line())  +
    geom_text(aes(label = round(.data[["index2"]],0), y = .data[["index2"]] + 3), 
              color = index_lowcolor, fontface="bold", size = 3,
              data = iData[iData$uCode == schoolcode,]) +
    annotate("text", x = uniqueN(iData$uName)/2, y = 50, label = bar_draft_text, size = bar_draft_size, color = "black", alpha = 0.1) +
    # POTENTIALLY NEED TO UPDATE LEGEND
    theme(legend.position="none")
  
  x <- girafe(ggobj = p, height_svg = 5, width_svg = 10)
  x <- girafe_options(x, opts_tooltip(offx = 20, offy = -10,
                                      use_fill = F, use_stroke = F,
                                      delay_mouseout = 1000,
                                      zindex = 9999,
                                      css = case_when(index_var == "outcomes"~"background-color: #9873BC; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 272px; padding: 6px;",
                                                      index_var == "conditions"~"background-color: #8BE3EC; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 272px; padding: 6px;",
                                                      index_var == "resources"~"background-color: #fab669; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 272px; padding: 6px;",
                                                      index_var == "experiences"~"background-color: #6ED1BE; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 272px; padding: 6px;")),
                      opts_toolbar(saveaspng = FALSE),
                      options = list(opts_sizing(rescale = FALSE)))
  return(x)
  
}

bar_plot_Indicator <- function(input_purse,indicator_var,sy_var,schoolcode,index_var,charter_var2){
  results <- as.data.frame(get_results(input_purse$coin[[sy_var]], 
                                       dset = "Aggregated", 
                                       tab_type = "Aggs",
                                       also_get = c("uName","subtype_group"))) %>%
    filter(subtype_group %in% charter_var2)
  
  p <- ggplot(results, 
              aes(x = reorder(uCode,-.data[[indicator_var]]),
                  #The below reorder label bars based on alphabetical order of names
                  #x = reorder(uCode,.data[["uName"]]),
                  y = .data[[indicator_var]]
                  )) + 
    geom_bar_interactive(stat = "identity",
                         aes(fill = uCode == schoolcode,
                             alpha = uCode == schoolcode,
                             tooltip = paste0("<span style='font-size:16px;'>",uName,"</span>", "<br>",
                                              indicator_names[IndicatorShort==indicator_var]$IndicatorLong,": ",
                                              "<b>",round(.data[[indicator_var]]),"</b>")
                         )) + 
    scale_fill_manual(values = c("lightgray",indicatorColors[indicator == indicator_var]$color)) +
    scale_alpha_manual(values = c(0.7,1)) +
    ggtitle(results$uName[results$uCode == schoolcode],
            subtitle = paste0(indicator_names[IndicatorShort==indicator_var]$IndicatorLong,", ",case_when(sy_var == 1 ~ "2017-18",
                                                                                                          sy_var == 2 ~ "2018-19",
                                                                                                          sy_var == 3 ~ "2019-20",
                                                                                                          sy_var == 4 ~ "2020-21"))) +
    theme_minimal() +
    labs(caption = paste0("Generated: ",today(), " (", purse_file,")")) +
    coord_cartesian(ylim = c(0,105)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 45, hjust=1, size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(face = "bold", hjust = 0.5, size = circ_bar_t, color ="#656565"),
          plot.subtitle = element_text(hjust = 0.5, size = circ_bar_st, color ="#656565"),
          plot.caption = element_text(color ="lightgrey", size = cap_size),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_line()) +
    # geom_text(aes(label = round(.data[[indicator_var]],0), y = .data[[indicator_var]] + 3), 
    #           color = indicatorColors[indicator == indicator_var]$color, fontface="bold", size = 3,
    #           #The below commented code will label all bars
    #           #data = results
    #           data = results[results$uCode == schoolcode,]
    #           ) +
    geom_text_interactive(aes(x=uCode,y=2, label=ifelse(is.na(.data[[indicator_var]]),"*"," "),
                              tooltip = paste0("<span style='font-size:16px;'>",uName,"</span>", "<br>",
                                               indicator_names[IndicatorShort==indicator_var]$IndicatorLong,": ",
                                               "<b>N/A</b>*","<br>","<br>",
                                               "*The underlying data for this indicator are not available for this school year.")),
                          color = "#656565", fontface="bold", size = 3
                          ) +
    # geom_text(aes(label = ifelse(is.na(.data[[indicator_var]]),"*",""), y = 2),
    #           color = "#656565", fontface="bold", size = 3,
    #           # data = results[[indicator_var]],
    #           # data = results[rowSums(is.na(results))>0,],
    # ) +
    geom_text(aes(label = ifelse(is.na(.data[[indicator_var]]),"*",round(.data[[indicator_var]],0)),
                  y = ifelse(is.na(.data[[indicator_var]]),2,.data[[indicator_var]] + 3)),
              color = indicatorColors[indicator == indicator_var]$color, fontface="bold", size = 3,
              #The below commented code will label all bars
              #data = results
              data = results[results$uCode == schoolcode,]
    ) +

    annotate("text", x = uniqueN(results$uName)/2, y = 50, label = bar_draft_text, size = bar_draft_size, color = "black", alpha = 0.1) +
    # POTENTIALLY NEED TO UPDATE LEGEND
    theme(legend.position="none")
  
  # ggplotly(p, tooltip = c("text")) %>%
  #   layout(showlegend = FALSE) %>%
  #   style(showlegend = FALSE)  %>%
  #   layout(title = list(text = paste0('<br>','<b>',results$uName[results$uCode == schoolcode],'</b>',
  #                                     ': ','<i>','Indicator Ranking, ',
  #                                     case_when(sy_var == 1 ~ "2017-18",
  #                                               sy_var == 2 ~ "2018-19",
  #                                               sy_var == 3 ~ "2019-20",
  #                                               sy_var == 4 ~ "2020-21"),'</i>'),
  #                       font = list(size = 24)))
  
  x <- girafe(ggobj = p, height_svg = 5, width_svg = 10)
  x <- girafe_options(x, opts_tooltip(offx = 20, offy = -10,
                                      use_fill = F, use_stroke = F,
                                      delay_mouseout = 1000,
                                      zindex = 9999,
                                      css = case_when(index_var == "outcomes"~"background-color: #9873BC; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 250px; padding: 6px;",
                                                      index_var == "conditions"~"background-color: #8BE3EC; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 250px; padding: 6px;",
                                                      index_var == "resources"~"background-color: #fab669; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 250px; padding: 6px;",
                                                      index_var == "experiences"~"background-color: #6ED1BE; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 250px; padding: 6px;")),
                      opts_toolbar(saveaspng = FALSE),
                      options = list(opts_sizing(rescale = FALSE)))
  return(x)
  
}

bar_plot_peer <- function(input_purse,sy_var,schoolcode,peercode,indicator_var,indicator_color,index_var){
  results <- as.data.frame(get_results(input_purse$coin[[sy_var]], 
                                       dset = "Aggregated", 
                                       tab_type = "Aggs",
                                       also_get = "uName"))
  
  peer1 <- peers[peers$uCode == schoolcode & peers$year == as.character(sy_var),]$peer_1
  peer2 <- peers[peers$uCode == schoolcode & peers$year == as.character(sy_var),]$peer_2
  peer3 <- peers[peers$uCode == schoolcode & peers$year == as.character(sy_var),]$peer_3
  peer4 <- peers[peers$uCode == schoolcode & peers$year == as.character(sy_var),]$peer_4
  peer <- c(peer1,peer2,peer3,peer4)
  
  
  p <- ggplot(results, 
         aes(x = reorder(uCode,-.data[[indicator_var]]),
             y = .data[[indicator_var]],
             tooltip = paste0("<span style='font-size:16px;'>",uName,"</span>", "<br>",
                              indicator_names[IndicatorShort==indicator_var]$IndicatorLong,": ",
                              "<b>",round(.data[[indicator_var]]),"</b>")
             # tooltip = paste0("<span style='font-size:16px;'>",.data[["uName"]],"</span>","<br>",
             #               case_when(indicator_var == "commenvhlth" ~ "Community & Environmental Health: ",
             #                         indicator_var == "engage" ~ "Engagement in Schooling: ",
             #                         indicator_var == "stinspnd" ~ "Student Instructional Support Needs: "),
             #               "<b>",round(.data[[indicator_var]]),"</b>")
             )) +
    geom_bar_interactive(stat = "identity",
             aes(fill = uCode == schoolcode,
                 alpha = uCode == schoolcode)) + 
    scale_fill_manual(values = c("lightgray",indicator_color)) +
    scale_alpha_manual(values = c(0.7,1)) +
    ggtitle("Peer Comparison",
            subtitle = paste0(indicator_names[IndicatorShort==indicator_var]$IndicatorLong,", ",case_when(sy_var == 1 ~ "2017-18",
                                                                                                          sy_var == 2 ~ "2018-19",
                                                                                                          sy_var == 3 ~ "2019-20",
                                                                                                          sy_var == 4 ~ "2020-21"))
            ) +
    theme_minimal() +
    labs(caption = paste0("Generated: ",today(), " (", purse_file,")")) +
    coord_cartesian(ylim = c(0,105)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 45, hjust=1, size = 8),
          axis.text.y = element_text(size = 8),
          plot.caption = element_text(color ="lightgrey", size = cap_size),
          plot.title = element_text(face = "bold", hjust = 0.5, size = circ_bar_t, color ="#656565"),
          plot.subtitle = element_text(hjust = 0.5, size = circ_bar_st, color ="#656565"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_line()) +
    geom_text(aes(label = round(.data[[indicator_var]],0), y = .data[[indicator_var]] + 3), 
              color = indicator_color, fontface="bold", size = 3,
              data = results[results$uCode == schoolcode,]) +
    #geom_bar(data = results[results$uCode == peer1,], aes(x= uCode, y = .data[[indicator_var]]), color = "#3ed1df", stat = "identity",fill = "lightgray",) +
    #geom_col_pattern(data = results[results$uCode == peer2,], aes(x= uCode, y = .data[[indicator_var]]),pattern_fill = "#3ed1df", pattern = 'circle', fill = "lightgray", colour = "#3ed1df",pattern_size=0.01) +
    geom_bar(data = results[results$uCode == peer1,], aes(x= uCode, y = .data[[indicator_var]]), color = "#3ed1df", stat = "identity",fill = "lightgray",size = 0.7) +
    geom_bar(data = results[results$uCode == peer2,], aes(x= uCode, y = .data[[indicator_var]]), color = "#3ed1df", stat = "identity",fill = "lightgray",size = 0.7) +
    geom_bar(data = results[results$uCode == peer3,], aes(x= uCode, y = .data[[indicator_var]]), color = "#3ed1df", stat = "identity",fill = "lightgray",size = 0.7) +
    geom_bar(data = results[results$uCode == peer4,], aes(x= uCode, y = .data[[indicator_var]]), color = "#3ed1df", stat = "identity",fill = "lightgray",size = 0.7) +
    #geom_col_pattern(data = results[results$uCode == peer1,], aes(x= uCode, y = .data[[indicator_var]]),pattern_fill = "#3ed1df", pattern = 'stripe', fill = "lightgray", colour = "#3ed1df",pattern_size=0.01) +
    #geom_col_pattern(data = results[results$uCode == peer2,], aes(x= uCode, y = .data[[indicator_var]]),pattern_fill = "#3ed1df", pattern = 'stripe', fill = "lightgray", colour = "#3ed1df",pattern_size=0.01) +
    #geom_col_pattern(data = results[results$uCode == peer3,], aes(x= uCode, y = .data[[indicator_var]]),pattern_fill = "#3ed1df", pattern = 'stripe', fill = "lightgray", colour = "#3ed1df",pattern_size=0.01) +
    #geom_col_pattern(data = results[results$uCode == peer4,], aes(x= uCode, y = .data[[indicator_var]]),pattern_fill = "#3ed1df", pattern = 'stripe', fill = "lightgray", colour = "#3ed1df",pattern_size=0.01) +
    #geom_bar(data = results[results$uCode == peer3,], aes(x= uCode, y = .data[[indicator_var]]), fill = "#C0E4E7", stat = "identity") +
    #geom_bar(data = results[results$uCode == peer4,], aes(x= uCode, y = .data[[indicator_var]]), fill = "#C0E4E7", stat = "identity") +
    geom_bar(data = results[results$uCode == peercode,], aes(x= uCode, y = .data[[indicator_var]]), fill = "#6a6a6a", stat = "identity") +
    geom_text(aes(label = round(.data[[indicator_var]],0), y = .data[[indicator_var]] + 3), 
              color = "#6a6a6a", fontface="bold", size = 3,
              data = results[results$uCode == peercode,])  +
    annotate("text", x = 25, y = 50, label = bar_draft_text, size = bar_draft_size, color = "black", alpha = 0.1)

  
  x <- girafe(ggobj = p, height_svg = 5, width_svg = 12)
  x <- girafe_options(x, opts_tooltip(offx = 20, offy = -10,
                                      use_fill = F, use_stroke = F,
                                      delay_mouseout = 1000,
                                      zindex = 9999,
                                      css = case_when(index_var == "outcomes"~"background-color: #9873BC; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 250px; padding: 6px;",
                                                      index_var == "conditions"~"background-color: #8BE3EC; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 250px; padding: 6px;",
                                                      index_var == "resources"~"background-color: #fab669; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 250px; padding: 6px;",
                                                      index_var == "experiences"~"background-color: #6ED1BE; border-style: solid; border-width: thin; border-color: #656565; color: black; width: 250px; padding: 6px;")
                                      ),
                      opts_toolbar(saveaspng = FALSE),
                      options = list(opts_sizing(rescale = FALSE)))
  return(x)

}

circ_bar_norm <- function(input_purse,indicator_var,sy_var,schoolcode,label_size = 14, multiview = F){
  
  agg_data <- get_data(input_purse$coin[[sy_var]],dset="Aggregated",iCodes = indicator_var) %>% data.table()
  
  circ_bar <- get_data(input_purse$coin[[sy_var]],dset="Normalised",iCodes = indicator_var,Level = 1, also_get = "uName") %>%
    filter(uCode == schoolcode) %>%
    pivot_longer(cols = measure_desc[Indicator == indicator_var]$MeasureShort, names_to = "metric", values_to = 'metric_score') %>%
    data.table() %>%
    .[,metric := factor(metric,
                        levels = measure_desc[Indicator == indicator_var]$MeasureShort,
                        labels = measure_desc[Indicator == indicator_var]$metric)] %>%
    setorder(metric) %>%
    merge(.,agg_data,by="uCode") %>%
    merge(.,measure_desc,by = "metric") %>%
    merge(.,measure_dir,by.x = "MeasureShort",by.y = "iCode") %>%
    .[,seq := seq(1,.N)] %>%
    .[,-c("MeasureShort","Indicator","Index")]
  
  # circ_bar$indicator <- get_data(equity_purse$coin[[4]],dset="Aggregated",iCodes = "commenvhlth") 
  
  label_data <- circ_bar
  
  number_of_bar <- nrow(label_data)
  angle <-  90 - 360 * (label_data$seq-0.5) /number_of_bar
  
  # label_data$hjust<-ifelse(angle < -90, 1, 0)
  label_data$hjust<-1
  # label_data$angle<-ifelse(angle < -90, angle+180, angle)
  label_data$angle<-angle-90
  
  p <- ggplot(circ_bar, mapping = aes(tooltip = paste0(# "<u>",uName,"</u>",# "<br>","<br>",
                                                       str_replace_all(metric,"\\n"," "), " Score:  ",
                                                       "<b>",round(metric_score),"</b>",
                                                       "<br>","<br>",
                                                       "<em>","(",Description,")","</em>",
                                                       "<br>","<br>",
                                                       ifelse(Direction>0,
                                                              case_when(round(metric_score)==100~"This school's score of <b>100</b> means that the raw value that this Measure is based on was the highest raw value that was observed among high schools within the school year.",
                                                                        round(metric_score)==0~"This school's score of <b>0</b> means that the raw value that this Measure is based on was the lowest raw value that was observed among high schools within the school year.",
                                                                        T~paste0("This school's score of ","<b>",round(metric_score),"</b>","  means that the raw value that this Measure is based on was ","<b>",round(metric_score),"%","</b>"," of the way between the lowest and highest raw values observed among high schools within the school year.")),
                                                              case_when(round(metric_score)==100~"This school's score of <b>100</b> means that the raw value that this Measure is based on was the lowest raw value that was observed among high schools within the school year.",
                                                                        round(metric_score)==0~"This school's score of <b>0</b> means that the raw value that this Measure is based on was the higheset raw value that was observed among high schools within the school year.",
                                                                        T~paste0("This school's score of ","<b>",round(metric_score),"</b>","  means that the raw value that this Measure is based on was ","<b>",round(metric_score),"%","</b>"," of the way between the highest and lowest raw values observed among high schools within the school year."))
                                                       )))) +
    geom_hline(aes(yintercept = y), data.frame(y = c(0:4)*25),color = "lightgrey") +
    geom_segment(aes(x = metric,y = 0,xend = metric,yend = 100),linetype = "dashed",color = "#777777") +
    # geom_col(aes(x = metric,y = metric_score,fill = metric,),position = "dodge2",show.legend = TRUE,alpha = .9) +
    geom_col_interactive(aes(x = metric,y = metric_score),position = "dodge2",show.legend = TRUE,alpha = .8, fill = indicatorColors[indicator == indicator_var]$color2) +
    # scale_fill_gradient(low = "#3ed1df", high = "#D8F6F9") +
    coord_polar(start = 0, clip = "off") +
    # geom_text(data=label_data, aes(x=metric, y=case_when(metric_score <= 8 ~ 0, metric_score == 100~ metric_score-12, T~metric_score-8), label=round(metric_score,0), hjust=hjust),
    #           color="black", fontface="bold",alpha=1, size=4.25, angle= label_data$angle, inherit.aes = FALSE) +  
    # geom_text(data=label_data, aes(x=circ_bar$metric, y=1, label=ifelse(is.na(metric_score),"*",""), hjust=hjust),
    #           color=indicatorColors[indicator == indicator_var]$color2, fontface="bold",alpha=1, size=6, angle= label_data$angle, inherit.aes = FALSE) +
    geom_text_interactive(data=label_data,
                          mapping=aes(x=metric, y=1, label=ifelse(is.na(metric_score),"*"," "),
                                      # hjust=hjust,
                                      tooltip = paste0(str_replace_all(metric,"\\n"," "), " Score:  ","<b>N/A</b>*","<br>","<br>",
                                                       "*The underlying data for this measure are not available for this school year.","<br>","<br>",
                                                       "<em>","(",Description,")","</em>")),
                          color=indicatorColors[indicator == indicator_var]$color2,fontface="bold",alpha=1, size=5,angle=label_data$angle,inherit.aes = FALSE
                          ) +
    annotate("text",x = 5, y = -25,label = round(agg_data[uCode == schoolcode][,2][[1]],0),fontface="bold",alpha=1, size=10, color = indicatorColors[indicator == indicator_var]$color) +
    scale_y_continuous(limits = c(-25, 100),expand = c(0, 0),breaks = c(0, 25, 50, 75, 100)) +
    labs(caption = paste0("Generated: ",today(), " (", purse_file,")")) +
    # labs(caption = paste0("<span style='color:#656565;font-size:12.0pt'>*Data for this measure is not available.</span>","<br><span style='color:lightgrey;'>Generated: ",today(), " (", purse_file,")</span>")) +
  
    theme(axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),
          axis.text.x = element_text(color = "gray12", size = label_size),legend.position = "none",
          panel.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),panel.grid.major.x = element_blank(),
          plot.caption = element_text(color ="lightgrey", size = cap_size),
          # plot.caption = element_markdown(hjust=1,size=cap_size)
          ) +
    annotate("text", x = 1, y = -25, label = circ_draft_text, size = circ_draft_size, color = "black", alpha = 0.07)
  
  p <- if (multiview == F){
    p + 
      ggtitle(str_wrap(circ_bar$uName[[1]],width = 40), 
              subtitle = paste0(indicator_names[IndicatorShort==indicator_var]$IndicatorLong,", ",case_when(sy_var == 1 ~ "2017-18",
                                                                                                            sy_var == 2 ~ "2018-19",
                                                                                                            sy_var == 3 ~ "2019-20",
                                                                                                            sy_var == 4 ~ "2020-21"))) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = circ_bar_t, color ="#656565"),
            plot.subtitle = element_text(hjust = 0.5, size = circ_bar_st, color ="#656565"),)
  } else {
    p 
    # + 
    #   ggtitle(paste0(indicator_names[IndicatorShort==indicator_var]$IndicatorLong,", ",case_when(sy_var == 1 ~ "2017-18",
    #                                                                                              sy_var == 2 ~ "2018-19",
    #                                                                                              sy_var == 3 ~ "2019-20",
    #                                                                                              sy_var == 4 ~ "2020-21"))) +
    #   theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 8, color ="#656565"))
  }
  
  x <- girafe(ggobj = p, height_svg = 6, width_svg = 6)
  x <- girafe_options(x, opts_tooltip(offx = 20, offy = -10,
                                      use_fill = T, use_stroke = F,
                                      delay_mouseout = 1000,
                                      zindex = 9999,
                                      css = "border-style: solid; border-width: thin; border-color: #656565; color: black; width: 225px; padding: 6px;"),
                      opts_toolbar(saveaspng = FALSE),
                      options = list(opts_sizing(rescale = FALSE)))
  return(x)
  
}

donut_plot_conditions1 <- function(input_purse,sy_var,schoolcode){

  data <- get_results(input_purse$coin[[sy_var]], 
                      dset = "Aggregated", 
                      tab_type = "Aggs",
                      also_get = "uName") %>%
    data.table() %>%
    .[,.(uCode,uName,conditions)] %>%
    .[,max := max(conditions)] %>%
    .[,diff := 100-conditions] %>%
    .[,conditions2 := round(100*conditions/max,0)]
  
  
  df <- data[uCode == schoolcode] %>% 
    melt(id.vars = c("uCode","uName"),measure.vars = c("conditions","diff"))
  
  ggplot(df,
         aes(x = 1,
             y = value,
             fill = variable)) +
    geom_col(show.legend = FALSE) +
    coord_polar(theta = "y",
                start = -pi/2,
                direction = -1
                ) +
    xlim(c(-2, 2)) +
    ylim(c(0,200)) +
    scale_fill_manual(values = c("#3ed1df", "grey90")) +
    theme_void() +
    annotate("text",
             label = round(df[variable=="conditions"]$value,0),
             fontface = "bold",
             color = "#3ed1df",
             size = 16,
             x = -2,
             y = 0) +
    theme(plot.margin = unit(c(-15,-10,-50,-10), "pt"),
          plot.background = element_rect(fill = "#f5f5f5"),
          panel.background = element_rect(fill = "#f5f5f5"))
}


donut_plot_conditions <- function(input_purse,sy_var,schoolcode){
  
  data <- get_results(input_purse$coin[[sy_var]], 
                      dset = "Aggregated", 
                      tab_type = "Aggs",
                      also_get = "uName") %>%
    data.table() %>%
    .[,.(uCode,uName,conditions)] %>%
    .[,max := max(conditions)] %>%
    .[,diff := 100-conditions] %>%
    .[,conditions2 := round(100*conditions/max,0)]
  
  
  df <- data[uCode == schoolcode] %>% 
    melt(id.vars = c("uCode","uName"),measure.vars = c("conditions","diff"))
  
  ggplot(df,
         aes(x = 1,
             y = value,
             fill = variable)) +
    geom_col(show.legend = FALSE) +
    coord_polar(theta = "y",
                direction = -1
    ) +
    scale_fill_manual(values = c("#3ed1df", "grey90")) +
    theme_void() +
    annotate("text",
             label = round(df[variable=="conditions"]$value,0),
             fontface = "bold",
             color = "#3ed1df",
             size = 14,
             x = -2,
             y = 0) +
    ggtitle("Conditions",)+
    theme(plot.title = element_text(hjust = 0.5,color = "#3ed1df",size=22,face="bold"))
  #+
  #  theme(plot.background = element_rect(fill = "transparent"),
     #     panel.background = element_rect(fill = "transparent",color=NA))

}



# plot_framework_cstm2 <- function(iMeta,color){
#   plt <- ggplot2::ggplot(iMeta, ggplot2::aes(x = .data$Level,
#                                              y = .data$EffWeight,
#                                              fill = .data$colourcol,
#                                              label = .data$iName))
#   
#   plt <- plt + ggplot2::geom_bar(stat = "identity", color='white', alpha = iMeta$Alf, fill=color)
#   
#   # text
#   plt <- plt + ggplot2::geom_text(size = text_size, check_overlap = TRUE, position = ggplot2::position_stack(vjust = 0.5),
#                                   colour = text_colour)
#   
#   # styling
#   plt <- plt + ggplot2::theme_minimal() + ggplot2::ylab("") + ggplot2::xlab("") +
#     ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
#                    panel.grid.minor = ggplot2::element_blank(),
#                    panel.border = ggplot2::element_blank(),
#                    panel.background = ggplot2::element_blank(),
#                    strip.background = ggplot2::element_blank(),
#                    axis.text= ggplot2::element_blank(),
#                    axis.ticks= ggplot2::element_blank(),
#                    legend.position="none"
#     )
#   
#   plt  +
#     ggplot2::theme(text=ggplot2::element_text(family="sans"))
# }

framework_plot <- function(input_purse,indicator_var,color){
  
  coin <- input_purse$coin[[4]]
  iMeta <- coin$Meta$Ind[!is.na(coin$Meta$Ind$Level), ]
  maxlev <- coin$Meta$maxlev
  text_colour <- "white"
  text_size <- 4.5
  colour_level <-  maxlev - 1
  coin <- get_eff_weights(coin, out2 = "coin")
  iMeta <- coin$Meta$Ind[!is.na(coin$Meta$Ind$Parent), ]
  lin <- coin$Meta$Lineage
  iMeta$colourcol <- "a"
  
  for(lev in 1:maxlev){
    # get codes
    codes <- iMeta$iCode[iMeta$Level == lev]
    if(lev <= colour_level){
      # get groups at colour_level
      iMeta$colourcol[match(codes, iMeta$iCode)] <-
        lin[[colour_level]][match(codes, lin[[lev]])]
    } else {
      iMeta$colourcol[match(codes, iMeta$iCode)] <- codes
    }
  }
  
  fac_order <- unique(Reduce(c,rev(lin[-ncol(lin)])))
  iMeta$colourcol <- factor(iMeta$colourcol, fac_order)
  # iMeta[iMeta$colourcol=="conditions",]$colourcol2 <- "#3ed1df"
  # iMeta[iMeta$colourcol=="experiences",]$colourcol2 <-  "#fa8a11"
  # iMeta[iMeta$colourcol=="resources",]$colourcol2 <- "#0eb393"
  # iMeta[iMeta$colourcol=="outcomes",]$colourcol2 <- "#541690"
                                
  iMeta <- iMeta[match(fac_order, iMeta$iCode), ]
  trans <- c(0.8,0.6,rep(0.4, 100))
  iMeta$Alf <- 1
  iMeta$Alf[iMeta$Level < colour_level] <- trans[colour_level - iMeta$Level[iMeta$Level < colour_level]]
  iMeta$Level <- maxlev - iMeta$Level + 1
  
  # measure_filter <- case_when(indicator_var=="conditions" ~ c("conditions","stinspnd","commenvhlth","test1","test2"),
  #                             indicator_var=="experiences" ~ c("experiences","climate","currbrdth","engage","test1"),
  #                             indicator_var=="resources" ~ c("resources","tchefficacy","leadership","proflearn","fteperpupil"),
  #                             T ~ c("outcomes","test1","test2","test3","test4"))
  # iMeta <- iMeta[iMeta$iCode == indicator_var|iMeta$Parent %in% measure_filter,]
  
  plt <- ggplot2::ggplot(iMeta, ggplot2::aes(x = .data$Level,
                                             y = .data$EffWeight,
                                             fill = .data$colourcol,
                                             label = .data$iName))
  
  # plt <- plt + ggplot2::geom_bar(stat = "identity", color='white', alpha = iMeta$Alf, fill=color)
  plt <- plt + ggplot2::geom_bar(stat = "identity", color='white', alpha = iMeta$Alf)
  
  # text
  plt <- plt + ggplot2::geom_text(size = case_when(iMeta$Level==4~text_size,
                                                   iMeta$Level==3~8,
                                                   iMeta$Level==2~11,
                                                   T~20), check_overlap = TRUE, position = ggplot2::position_stack(vjust = 0.5),
                                  colour = text_colour)
  
  # styling
  plt <- plt + ggplot2::theme_minimal() + ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   axis.text= ggplot2::element_blank(),
                   axis.ticks= ggplot2::element_blank(),
                   legend.position="none",
                   plot.margin = margin(-2, -0.1, -2, -0.1, "cm")
    )
  
  plt  +
    ggplot2::theme(text=ggplot2::element_text(family="sans")) +
    scale_fill_manual(values=c("conditions"="#3ed1df","resources"="#fa8a11","experiences"="#0eb393","outcomes"="#541690"))
  
}




## Server Function ---------------
function(input, output, session) {
  
  observeEvent(input$enterbutton, {
    updateTabsetPanel(session, "maintab",
                      selected = "tab2")
  })
  
  observeEvent(input$cond, {
    updateTabsetPanel(session, "maintab",
                      selected = "tab2")
    updateRadioGroupButtons(session,
                       inputId = "indexselect",
                       selected = "conditions")
  })
  
  observeEvent(input$res, {
    updateTabsetPanel(session, "maintab",
                      selected = "tab2")
    updateRadioGroupButtons(session,
                       inputId = "indexselect",
                       selected = "resources")
  })
  
  observeEvent(input$out, {
    updateTabsetPanel(session, "maintab",
                      selected = "tab2")
    updateRadioGroupButtons(session,
                       inputId = "indexselect",
                       selected = "outcomes")
  })
  
  observeEvent(input$opp, {
    updateTabsetPanel(session, "maintab",
                      selected = "tab2")
    updateRadioGroupButtons(session,
                       inputId = "indexselect",
                       selected = "experiences")
  })
  
  observeEvent(input$indexselect, {
    if (input$indexselect == "conditions") {
      updateSelectizeInput(session,
                           inputId = "indicatorselect",
                           choices = c("Student Inst. Support Needs" = "stinspnd", 
                                       "Community Env. Health" = "commenvhlth"))
    } else if (input$indexselect == "experiences") {
      updateSelectizeInput(session,
                           inputId = "indicatorselect",
                           choices = c("Engagement in Schooling" = "engage","School Climate" = "climate",
                                       "Curricular Breadth" = "currbrdth"))
    } else if (input$indexselect == "resources") {
      updateSelectizeInput(session,
                           inputId = "indicatorselect",
                           choices = c("Teacher Self-Efficacy"="tchefficacy","Leadership"="leadership",
                                       "Professional Learning"="proflearn","FTE per Pupil"="fteperpupil",
                                       "Per Pupil Spending" = "perpplspend"))
    } else if (input$indexselect == "outcomes") {
      updateSelectizeInput(session,
                           inputId = "indicatorselect",
                           choices = c("College & Career"="collegecareer"))
    } else {
      updateSelectizeInput(session,
                           inputId = "indicatorselect",
                           choices = c("TBD"))
    }
  })
  
  observeEvent(input$charterselect2, {
    # if (input$charterselect == F) {
    #   updatePickerInput(session,
    #                        inputId = "schoolcodeselect",
    #                        choices = sort(unique(filter(equity_purse$coin[[4]]$Meta$Unit,subtype_group%in%c(3,4,5,15))$uName)))
    #   updateSelectizeInput(session,
    #                        inputId = "initschoolcodeselect",
    #                        selected = sort(unique(filter(equity_purse$coin[[4]]$Meta$Unit,subtype_group%in%c(3,4,5,15))$uName)))
    #                     
    # } else {
    #   updatePickerInput(session,
    #                        inputId = "schoolcodeselect",
    #                        choices = sort(unique(equity_purse$coin[[4]]$Meta$Unit$uName)))
    #   updateSelectizeInput(session,
    #                        inputId = "initschoolcodeselect",
    #                        selected = sort(unique(equity_purse$coin[[4]]$Meta$Unit$uName)))
    # }
    if (input$charterselect2 == "noncharters") {
      updatePickerInput(session,
                           inputId = "schoolcodeselect",
                           choices = sort(unique(filter(equity_purse$coin[[4]]$Meta$Unit,subtype_group%in%c(3,4,5,15))$uName)))
      updatePickerInput(session,
                           inputId = "initschoolcodeselect",
                           choices = sort(unique(filter(equity_purse$coin[[4]]$Meta$Unit,subtype_group%in%c(3,4,5,15))$uName)))

    } else if (input$charterselect2 == "charters") {
      updatePickerInput(session,
                           inputId = "schoolcodeselect",
                           choices = sort(unique(filter(equity_purse$coin[[4]]$Meta$Unit,subtype_group%in%c(11))$uName)))
      updatePickerInput(session,
                           inputId = "initschoolcodeselect",
                           choices = sort(unique(filter(equity_purse$coin[[4]]$Meta$Unit,subtype_group%in%c(11))$uName)))
    } else {
      updatePickerInput(session,
                            inputId = "schoolcodeselect",
                            choices = sort(unique(equity_purse$coin[[4]]$Meta$Unit$uName)))
      updatePickerInput(session,
                            inputId = "initschoolcodeselect",
                            choices = sort(unique(equity_purse$coin[[4]]$Meta$Unit$uName)))
    }
  })
  
  observeEvent(input$initschoolcodeselect, {
    updatePickerInput(session,
                           inputId = "schoolcodeselect",
                           selected = input$initschoolcodeselect
                           )
  })
  
  observeEvent(input$schoolcodeselect, {
    updatePickerInput(session,
                         inputId = "initschoolcodeselect",
                         selected = input$schoolcodeselect
    )
  })
  
  observeEvent(input$schoolcodeselect, {
    updatePickerInput(session,
                      inputId = "peerselect",
                      # selected = data.table(get_data(equity_purse$coin[[4]],dset="Aggregated",iCodes="engage",also_get = "uName"))[uCode==data.table(peers)[year==year_var()&uName==input$schoolcodeselect]$peer_1]$uName
                      selected = data.table(get_data(equity_purse$coin[[4]],dset="Aggregated",iCodes="engage",also_get = "uName"))[uCode==data.table(peers)[year==year_var()&uName==input$schoolcodeselect]$peer_1]$uName
    )
  })
  
  
  schoolnames <- as.data.frame(get_results(equity_purse$coin[[4]], 
                                           dset = "Aggregated", 
                                           tab_type = "Aggs",
                                           also_get = "uName")) %>%
    .[c("uCode","uName")]
  
  ### Reactive Variables ---------------
  year_var <- reactive(input$schoolyearselect)
  # schcode_var <- reactive(input$schoolcodeselect)
  schcode_var <- reactive(schoolnames$uCode[schoolnames$uName %in% input$schoolcodeselect])
  # peerschcode_var <- reactive(input$peerselect)
  peerschcode_var <- reactive(schoolnames$uCode[schoolnames$uName %in% input$peerselect])
  indicator_var <- reactive(input$indicatorselect)
  multiindicator_var <- reactive(input$multiview3select)
  index_var <- reactive(input$indexselect)
  index_lowcolor <- reactive(indexColors$lowcolor[indexColors$index %in% input$indexselect])
  index_midAcolor <- reactive(indexColors$midAcolor[indexColors$index %in% input$indexselect])
  index_midcolor <- reactive(indexColors$midcolor[indexColors$index %in% input$indexselect])
  index_midBcolor <- reactive(indexColors$midBcolor[indexColors$index %in% input$indexselect])
  index_highcolor <- reactive(indexColors$highcolor[indexColors$index %in% input$indexselect])
  indicator_color <- reactive(indicatorColors$color[indicatorColors$indicator %in% input$indicatorselect])
  multiInd1_var <- reactive(input$multiIndselect1)
  multiInd2_var <- reactive(input$multiIndselect2)
  multiInd3_var <- reactive(input$multiIndselect3)
  multiInd4_var <- reactive(input$multiIndselect4)
  multiYr1_var <- reactive(input$multiYrselect1)
  multiYr2_var <- reactive(input$multiYrselect2)
  multiYr3_var <- reactive(input$multiYrselect3)
  multiYr4_var <- reactive(input$multiYrselect4)
  # 3    4    5   11   15
  # charter_var2 <- reactive({
  #   if (input$charterselect == T) {
  #     return(c(3,4,5,11,15))}
  #   else {
  #     return(c(3,4,5,15))
  #   }
  # })
  charter_var2 <- reactive({
    if (input$charterselect2 == "noncharters") {
      return(c(3,4,5,15))
      }
    else if (input$charterselect2 == "charters") {
      return(c(11))
      }
    else {
      return(c(3,4,5,11,15))
    }
  })
 
  # output$fDataTable = DT::renderDataTable({
  #   iris_coloured <- iris
  #   colnames(iris_coloured)[c(1,3)] <- paste0('<span style="color:',c("red","blue"),'">',colnames(iris)[c(1,3)],'</span>')
  #   DT::datatable(iris_coloured,escape=F) %>%
  #     formatStyle(columns = 1, color = "red") %>%
  #     formatStyle(columns = 3, color = "blue")
  # })
   
  ### Index Table ---------------
  output$aggtable <- renderDataTable({
    index_table <- data.table(get_results(equity_purse$coin[[as.integer(year_var())]],dset = "Aggregated",tab_type = "Aggs",also_get=c("uName","subtype_group"))) %>%
      .[,(c("conditions","resources","experiences","outcomes")) := round(.SD,0), .SDcols=c("conditions","resources","experiences","outcomes")]
    index_table <- index_table[subtype_group %in% charter_var2()] %>%
      .[,.(uCode,uName,conditions,resources,experiences,outcomes)] %>%
      setnames(c("uCode","uName","resources","conditions","experiences","outcomes"),c("Code","School Name","Resources","Conditions","Experiences","Outcomes"))
    colnames(index_table)[c(3,4,5,6)] <- paste0('<span style="color:',c("#3ed1df","#fa8a11","#0eb393","#541690"),'">',colnames(index_table)[c(3,4,5,6)],'</span>')
    datatable(index_table,
              options = list(pageLength = -1,
                             lengthChange = FALSE
                             ),
              rownames=FALSE,
              escape=FALSE) %>%
      formatStyle(columns = c(3:6), 'text-align' = 'center') %>%
      # color_gradient("Conditions",gradient_colors = c("#3ed1df","#8BE3EC","#D8F6F9")) %>%
      color_gradient(colnames(index_table)[c(3)],gradient_colors = c("#8BE3EC","#D8F6F9","#ebfbfc")) %>%
    #   # color_gradient("Resources",gradient_colors = c("#fa8a11","#fab669","#fcdcb8")) %>%
      color_gradient(colnames(index_table)[c(4)],gradient_colors = c("#fab669","#fcdcb8","#ffefde")) %>%
    #   # color_gradient("Experiences",gradient_colors = c("#0eb393","#6ED1BE","#9FE1D4")) %>%
      color_gradient(colnames(index_table)[c(5)],gradient_colors = c("#6ED1BE","#9FE1D4","#bce0d9")) %>%
    #   # color_gradient("Outcomes",gradient_colors = c("#541690","#9873BC","#BBA2D3")) %>%
      color_gradient(colnames(index_table)[c(6)],gradient_colors = c("#9873BC","#BBA2D3","#c8bcd4"))
    # %>% 
    #   formatStyle("Resources",backgroundColor="#fab669") %>% formatStyle("Conditions",backgroundColor="#8BE3EC") %>% formatStyle("Experiences",backgroundColor="#6ED1BE") %>% formatStyle("Outcomes",backgroundColor="#9873BC")
  })
  
  ### Donut Plot ---------------
  output$condplot <- renderPlot({
    
    donut_plot_conditions(equity_purse,as.integer(year_var()),schcode_var())
    
  },bg="transparent")
  
  
  ## Bar Plots ---------------
  output$indexPlot <- renderGirafe({
    if (index_var() %in% c("conditions","experiences","resources")){

      # bar_plot_Index(equity_purse,as.integer(year_var()),schcode_var(),index_var(),index_lowcolor(),index_midcolor(),index_highcolor(),charter_var1())
      # bar_plot_Index(equity_purse,as.integer(year_var()),schcode_var(),index_var(),index_lowcolor(),index_midcolor(),index_midBcolor(),index_highcolor(),charter_var2())
      bar_plot_Index(equity_purse,as.integer(year_var()),schcode_var(),index_var(),index_lowcolor(),index_midAcolor(),index_midcolor(),index_midBcolor(),index_highcolor(),charter_var2())

    } else {

    p <- ggplot() +
      annotate("text", x = 25, y = 50, label = "The visualization\nfor the Outcomes index\nis not yet available.", size = 14, color = "black", alpha = 0.5) +# Draw ggplot2 plot with text only
      theme_void()
      girafe(ggobj = p,height_svg = 5, width_svg = 10)

    }
  })
  

  
  output$rankPlot <- renderGirafe({
    
    # if (indicator_var() == "TBD"){ 
    #   
    #   p <- ggplot() + 
    #     annotate("text", x = 25, y = 50, label = "This index is\nnot yet available.", size = 18, color = "black", alpha = 0.5) +# Draw ggplot2 plot with text only
    #     theme_void()
    #   girafe(ggobj = p,height_svg = 5, width_svg = 10) 
    #   
    # } else {
    #   
      bar_plot_Indicator(equity_purse,indicator_var(),as.integer(year_var()),schcode_var(),index_var(),charter_var2())
      
    # }
    
  })
    

  ### Measure Plots ---------------
  output$brkdwnPlot <- renderGirafe({
    
    if (indicator_var() == "TBD"){ 
      
      p <- ggplot() + 
        annotate("text", x = 25, y = 50, label = "This index is\nnot yet available.", size = 18, color = "black", alpha = 0.5) +# Draw ggplot2 plot with text only
        theme_void()
      girafe(ggobj = p,height_svg = 5, width_svg = 10) 
      
    } else {
      
    circ_bar_norm(equity_purse,indicator_var(),as.integer(year_var()),schcode_var(),label_size = 7.5)
    
    }
    
  })
  
  output$brkdwnPlotA <- renderGirafe({
    
    if (indicator_var() == "TBD"){ 
      
      p <- ggplot() + 
        annotate("text", x = 25, y = 50, label = "This index is\nnot yet available.", size = 18, color = "black", alpha = 0.5) +# Draw ggplot2 plot with text only
        theme_void()
      girafe(ggobj = p,height_svg = 5, width_svg = 10) 
      
    } else {
      
    circ_bar_norm(equity_purse,indicator_var(),as.integer(year_var()),schcode_var(),label_size = 7.5)
    
    }
    
  })
  
  output$brkdwnPlotB <- renderGirafe({
    
    if (indicator_var() == "TBD"){ 
      
      p <- ggplot() + 
        annotate("text", x = 25, y = 50, label = "This index is\nnot yet available.", size = 18, color = "black", alpha = 0.5) +# Draw ggplot2 plot with text only
        theme_void()
      girafe(ggobj = p,height_svg = 5, width_svg = 10) 
      
    } else {
      
    circ_bar_norm(equity_purse,indicator_var(),as.integer(year_var()),peerschcode_var(),label_size = 7.5)
    
    }
    
  })
  
  output$multiViewA <- renderGirafe({

    circ_bar_norm(equity_purse,multiInd1_var(),as.integer(year_var()),schcode_var(),label_size = 7.5, multiview = T)

  })
  
  output$multiViewB <- renderGirafe({
      
    circ_bar_norm(equity_purse,multiInd2_var(),as.integer(year_var()),schcode_var(),label_size = 7.5, multiview = T)
    
  })
  
  output$multiViewC <- renderGirafe({
      
    circ_bar_norm(equity_purse,multiInd3_var(),as.integer(year_var()),schcode_var(),label_size = 7.5, multiview = T)

  })
  
  output$multiViewD <- renderGirafe({

    circ_bar_norm(equity_purse,multiInd4_var(),as.integer(year_var()),schcode_var(),label_size = 7.5, multiview = T)

  })

  output$multiView2A <- renderGirafe({

    circ_bar_norm(equity_purse,multiindicator_var(),as.integer(multiYr1_var()),schcode_var(),label_size = 7.5, multiview = T)
    
  })
  
  output$multiView2B <- renderGirafe({
      
    circ_bar_norm(equity_purse,multiindicator_var(),as.integer(multiYr2_var()),schcode_var(),label_size = 7.5, multiview = T)
    
  })
  
  output$multiView2C <- renderGirafe({

    circ_bar_norm(equity_purse,multiindicator_var(),as.integer(multiYr3_var()),schcode_var(),label_size = 7.5, multiview = T)
    
  })
  
  output$multiView2D <- renderGirafe({
      
    circ_bar_norm(equity_purse,multiindicator_var(),as.integer(multiYr4_var()),schcode_var(),label_size = 7.5, multiview = T)
    
  })
  
  ### Peer Bar Plot ---------------
  output$rankplotpeer <- renderGirafe({
    
    if (indicator_var() == "TBD"){ 
      
      p <- ggplot() + 
        annotate("text", x = 25, y = 50, label = "This index is\nnot yet available.", size = 18, color = "black", alpha = 0.5) +# Draw ggplot2 plot with text only
        theme_void()
      girafe(ggobj = p,height_svg = 5, width_svg = 10) 
      
    } else {
      
    bar_plot_peer(equity_purse,as.integer(year_var()),schcode_var(),peerschcode_var(),indicator_var(),indicator_color(),index_var())
    
    }
    
  })

  ### Reactive Titles ---------------
  output$indexSchoolTitle <- renderText({
    schoolnames$uName[schoolnames$uCode == schcode_var()]
  })
  
  output$indexNarrative <- renderText({
    if (index_var() == "conditions"){
      paste0("The Conditions Index measures schools’ baselines. This Index is made up of two Indicators: Student 
             Instructional Support Needs and Community and Environmental Health."
             # and Human Service Support Needs.
             # ," This school’s score of ", round(data.table(get_results(equity_purse$coin[[as.integer(year_var())]], 
             #                                        dset = "Aggregated", 
             #                                        tab_type = "Aggs"))[uCode==schcode_var()]$conditions,0),
             # " on this Index is the average of those two Indicator scores. This Conditions score does not inherently 
             # indicate how this school performed relative to other schools. For that comparison, consult the bar chart 
             # below, which shows how this school’s average score on the aforementioned Indicators compares to other 
             # schools. The subsections within each bar show how each Indicator contributes to the Index score."
             )
    } else if (index_var() == "resources") {
      # "The Resources index measure schools’ investments."
      paste0("The Resources Index measures schools’ investments. This Index is made up of four Indicators: 
             Teacher Self-Efficacy, Professional Learning, Leadership, and FTE per Pupil."
             # and UCOA Spending.
             # ," This school’s score of ", round(data.table(get_results(equity_purse$coin[[as.integer(year_var())]], 
             #                                                          dset = "Aggregated", 
             #                                                          tab_type = "Aggs"))[uCode==schcode_var()]$resources,0),
             # " on this Index is the average of those four Indicator scores. This Resources score does not inherently 
             # indicate how this school performed relative to other schools. For that comparison, consult the bar chart 
             # below, which shows how this school’s average score on the aforementioned Indicators compares to other 
             # schools. The subsections within each bar show how each Indicator contributes to the Index score."
             )
    } else if (index_var() == "experiences") {
      # "The Experiences index measures students’ experiences. It includes indicators related to
      # Engagement in Schooling, School Climate, Curricular Breadth, and Access to High-Quality
      # Academic Supports."
      paste0("The Experiences Index measures schools’ experiences. This Index is made up of three Indicators: 
             Engagement in Schooling, School Climate, and Curricular Breadth."
             # ," This school’s score of ", round(data.table(get_results(equity_purse$coin[[as.integer(year_var())]], 
             #                                                          dset = "Aggregated", 
             #                                                          tab_type = "Aggs"))[uCode==schcode_var()]$experiences,0),
             # " on this Index is the average of those three Indicator scores. This Experiences score does not inherently 
             # indicate how this school performed relative to other schools. For that comparison, consult the bar chart 
             # below, which shows how this school’s average score on the aforementioned Indicators compares to other 
             # schools. The subsections within each bar show how each Indicator contributes to the Index score."
             )
    } else {
      # "The Outcomes index measures students’ achievements. It includes indicators related to
      # Participation in Advanced Coursework and College & Career Readiness.\n(Note: Index forthcoming.)"
      paste0("The Outcomes Index measures schools’ experiences. This Index is made up of three Indicators: 
             Performance on Tests, Promotion, and College & Career Readiness."
             # ," This school’s score of ", round(data.table(get_results(equity_purse$coin[[as.integer(year_var())]], 
             #                                                          dset = "Aggregated", 
             #                                                          tab_type = "Aggs"))[uCode==schcode_var()]$outcomes,0),
             # " on this Index is the average of those three Indicator scores. This Outcomes score does not inherently 
             # indicate how this school performed relative to other schools. For that comparison, consult the bar chart 
             # below, which shows how this school’s average score on the aforementioned Indicators compares to other 
             # schools. The subsections within each bar show how each Indicator contributes to the Index score."
             )
    }
  })
  
  output$indexNarrativeNote <- renderUI({
    if (index_var() == "conditions"){
      paste0("Note: This school’s score of ", round(data.table(get_results(equity_purse$coin[[as.integer(year_var())]], 
                                                                         dset = "Aggregated", 
                                                                         tab_type = "Aggs"))[uCode==schcode_var()]$conditions,0),
             " on this Index is the average of the two underlying Indicator scores. This Conditions score does not inherently 
             indicate how this school performed relative to other schools. For that comparison, consult the bar chart 
             below, which shows how this school’s average score on the aforementioned Indicators compares to other 
             schools. The subsections within each bar show how each Indicator contributes to the Index score.")
    } else if (index_var() == "resources") {
      # "The Resources index measure schools’ investments."
      paste0("Note: This school’s score of ", round(data.table(get_results(equity_purse$coin[[as.integer(year_var())]], 
                                                                      dset = "Aggregated", 
                                                                      tab_type = "Aggs"))[uCode==schcode_var()]$resources,0),
             " on this Index is the average of the four underlying Indicator scores. This Resources score does not inherently 
             indicate how this school performed relative to other schools. For that comparison, consult the bar chart 
             below, which shows how this school’s average score on the aforementioned Indicators compares to other 
             schools. The subsections within each bar show how each Indicator contributes to the Index score.")
    } else if (index_var() == "experiences") {
      # "The Experiences index measures students’ experiences. It includes indicators related to
      # Engagement in Schooling, School Climate, Curricular Breadth, and Access to High-Quality
      # Academic Supports."
      paste0("Note: This school’s score of ", round(data.table(get_results(equity_purse$coin[[as.integer(year_var())]], 
                                                                      dset = "Aggregated", 
                                                                      tab_type = "Aggs"))[uCode==schcode_var()]$experiences,0),
             " on this Index is the average of the three underlying Indicator scores. This Experiences score does not inherently 
             indicate how this school performed relative to other schools. For that comparison, consult the bar chart 
             below, which shows how this school’s average score on the aforementioned Indicators compares to other 
             schools. The subsections within each bar show how each Indicator contributes to the Index score.")
    } else {
      # "The Outcomes index measures students’ achievements. It includes indicators related to
      # Participation in Advanced Coursework and College & Career Readiness.\n(Note: Index forthcoming.)"
      paste0("Note: This school’s score of ", round(data.table(get_results(equity_purse$coin[[as.integer(year_var())]], 
                                                                      dset = "Aggregated", 
                                                                      tab_type = "Aggs"))[uCode==schcode_var()]$outcomes,0),
             " on this Index is the average of the three underlying Indicator scores. This Outcomes score does not inherently 
             indicate how this school performed relative to other schools. For that comparison, consult the bar chart 
             below, which shows how this school’s average score on the aforementioned Indicators compares to other 
             schools. The subsections within each bar show how each Indicator contributes to the Index score.")
    }
  })
  
  # output$indexNarrativeNote <- renderText({
  #   paste0("Note: The Index score does not inherently indicate how this school performed 
  #   relative to other schools. For that comparison, consult the bar chart below, which shows how this school’s average 
  #   score on the aforementioned Indicators compares to other schools. The subsections within each bar show how each Indicator 
  #   contributes to the Index score.")
  # })

  # output$indicatorSchoolTitle <- renderText({
  #   schoolnames$uName[schoolnames$uCode == schcode_var()]
  # })
  output$indicatorNarrative <- renderText({
    paste0(indicator_desc$Description[indicator_desc$Indicator == indicator_var()]," The ",indicator_names[IndicatorShort==indicator_var()]$IndicatorLong,
           " Indicator is made up of ",measure_desc[Indicator==indicator_var(),uniqueN(MeasureShort)]," Measures (see Measures tab).")
  })
  
  output$indicatorNarrativeNote <- renderText({
    paste0("Note: This school’s score of ",
           round(data.table(get_results(equity_purse$coin[[as.integer(year_var())]],
                                        dset = "Aggregated",
                                        tab_type = "Aggs"))[uCode==schcode_var()][[indicator_var()]],0),
           " is the average of the ",measure_desc[Indicator==indicator_var(),uniqueN(MeasureShort)]," underlying Measure scores. This ",
           indicator_names[IndicatorShort==indicator_var()]$IndicatorLong," score does not inherently indicate how this school 
          performed relative to other schools. For that comparison, consult the bar chart below, which shows how this school’s 
          average score on the aforementioned Measures compares to other schools.")
  })
  
  output$measureNarrative <- renderText({
    paste0("The ",indicator_names[IndicatorShort==indicator_var()]$IndicatorLong," Indicator is made up of the ",
    measure_desc[Indicator==indicator_var(),uniqueN(MeasureShort)]," Measures shown in the chart below. Each Measure
    is normalized* and therefore has a minimum possible score of 0 and a maximum possible score of 100. Hover over an individual 
    bar to learn about this school’s score on that Measure. The school’s average score across all ",
    measure_desc[Indicator==indicator_var(),uniqueN(MeasureShort)]," Measures in this Indicator (the school's ",
    indicator_names[IndicatorShort==indicator_var()]$IndicatorLong,
    " score) is shown in the center of the circular bar chart.")
  })
  # output$measureSchoolTitle <- renderText({
  #   schoolnames$uName[schoolnames$uCode == schcode_var()]
  # })
  # output$measureSchoolTitle1 <- renderText({
  #   schoolnames$uName[schoolnames$uCode == schcode_var()]
  # })  
  # output$measureSchoolTitle2 <- renderText({
  #   schoolnames$uName[schoolnames$uCode == peerschcode_var()]
  # })
  # output$indexYearTitle <- renderText({
  #   case_when(year_var() == 1 ~ "2017-18",year_var() == 2 ~ "2018-19",year_var() == 3 ~ "2019-20",year_var() == 4 ~ "2020-21")
  # })
  # output$indicatorYearTitle <- renderText({
  #   case_when(year_var() == 1 ~ "2017-18",year_var() == 2 ~ "2018-19",year_var() == 3 ~ "2019-20",year_var() == 4 ~ "2020-21")
  # })  
  # output$measureYearTitle <- renderText({
  #   case_when(year_var() == 1 ~ "2017-18",year_var() == 2 ~ "2018-19",year_var() == 3 ~ "2019-20",year_var() == 4 ~ "2020-21")
  # })
  # output$measureCompYearTitle1 <- renderText({
  #   case_when(year_var() == 1 ~ "2017-18",year_var() == 2 ~ "2018-19",year_var() == 3 ~ "2019-20",year_var() == 4 ~ "2020-21")
  # })  
  # output$measureCompYearTitle2 <- renderText({
  #   case_when(year_var() == 1 ~ "2017-18",year_var() == 2 ~ "2018-19",year_var() == 3 ~ "2019-20",year_var() == 4 ~ "2020-21")
  # })
  
  # output$indicatorIndTitle <- renderText({
  #   if (indicator_var() == "stinspnd"){
  #     "Student Instructional Support Needs"
  #   } else if (indicator_var() == "engage") {
  #     "Engagement in Schooling"
  #   } else {
  #     "Community and Environmental Health"
  #   }
  # })  
  # output$measureIndTitle <- renderText({
  #   if (indicator_var() == "stinspnd"){
  #     "Student Instructional Support Needs"
  #   } else if (indicator_var() == "engage") {
  #     "Engagement in Schooling"
  #   } else {
  #     "Community and Environmental Health"
  #   }
  # })
  output$yourSchTitle <- renderUI({
    if (index_var() == "conditions"){
      tags$h4("Your School:",style = "color: #8BE3EC; text-align: center; font-weight: bold; font-size: 24px;")
    } else if (index_var() == "experiences") {
      tags$h4("Your School:",style = "color: #6ED1BE; text-align: center; font-weight: bold; font-size: 24px;")
    } else if (index_var() == "outcomes") {
      tags$h4("Your School:",style = "color: #9873BC; text-align: center; font-weight: bold; font-size: 24px;")
    } else {
      tags$h4("Your School:",style = "color: #fab669; text-align: center; font-weight: bold; font-size: 24px;")
    }
  
  })
  # output$measureCompIndTitle1 <- renderText({
  #   if (indicator_var() == "stinspnd"){
  #     "Student Instructional Support Needs"
  #   } else if (indicator_var() == "engage") {
  #     "Engagement in Schooling"
  #   } else {
  #     "Community and Environmental Health"
  #   }
  # })  
  # output$measureCompIndTitle2 <- renderText({
  #   if (indicator_var() == "stinspnd"){
  #     "Student Instructional Support Needs"
  #   } else if (indicator_var() == "engage") {
  #     "Engagement in Schooling"
  #   } else {
  #     "Community and Environmental Health"
  #   }
  # })
  
  ### Measure Tables ---------------
  # output$measureTable <- renderTable({
  #   if (indicator_var() == "stinspnd"){
  #     stinspnd_desc
  #   } else if (indicator_var() == "engage") {
  #     engage_desc
  #   } else {
  #     commenvhlt_desc
  #   }
  # })
  # output$measureTablePeer <- renderTable({
  #   if (indicator_var() == "stinspnd"){
  #     stinspnd_desc
  #   } else if (indicator_var() == "engage") {
  #     engage_desc
  #   } else {
  #     commenvhlt_desc
  #   }
  #   
  # })
  
  ### Framework ---------------
  output$framework <- renderPlot({
    
    # plot_framework(equity_purse$coin[[1]], 
    #                type = "stack",
    #                colour_level = 2,
    #                text_size = 5) 
    framework_plot(equity_purse)
    # +
    #   theme(text = element_text(size = 22))
  })
  
  session$onSessionEnded(function() {
      stopApp()
  })

  # reactiveValues object for storing current data set.
  vals <- reactiveValues(data = NULL)
  


}

