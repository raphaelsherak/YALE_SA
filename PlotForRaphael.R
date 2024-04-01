#***********************************
# dfInput is a df
# QJType is a string
# ParamLevelOrderRephrase is a vec for renaming the y-axis tick
# colors is a vec for different parameter (parameter being UQJ Type)
#***********************************

GeneratePlotSecondary <- function(dfInput,QJType,ParamLevelOrderRephrase,colors){
  FinalArrowMsg = "Less likely                   OR                   More likely"
  TitleStr = paste("Association Between",QJType, sep = " ", collapse = NULL)
  TitleStr = paste(TitleStr,'Exposure and ED Outcome Measures', sep = " ", collapse = NULL)
  
  pd <- position_dodge(width = 0.3)
  p <- ggplot(dfInput, aes(OR, Outcome,group = Parameter))
  p<-p + geom_point(position = pd,aes(shape=Parameter, color=Parameter)) +
    geom_errorbarh(aes(xmax = OR_CI_Upper_BH, xmin = OR_CI_Lower_BH, color = Parameter),height = 0.1,position = pd)+
    labs(x =FinalArrowMsg, y = "ED Outcome Measures",color = "UQJ Type",shape="UQJ Type")+
    theme(
      plot.title = element_text(size=15),
      axis.title.x = element_text(size=13),
      axis.title.y = element_text(size=14),
      plot.caption = element_text(hjust = 0, face= "italic",size=10),
      axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),ends = "both")),
      axis.text.y = element_text(size=12),
      axis.text.x = element_text(size=11),
      legend.text=element_text(size=12),
      legend.title = element_text(size = 14),
      legend.key.height=unit(1, "cm")
    ) +  
    geom_vline(xintercept = 1)  + 
    scale_x_continuous(breaks = seq(0.7, 1.25, by = 0.05)) + 
    labs(caption = "Note: Horizontal lines are 95% confidence intervals")+
    ggtitle(TitleStr) + theme(plot.title = element_text(hjust = 0.5)) +
    guides(color = guide_legend(reverse=TRUE),shape =guide_legend(reverse=TRUE))+ 
    scale_colour_manual(values = colors) + scale_y_discrete(limits=rev,labels=rev(ParamLevelOrderRephrase))
  return(p)
}
