library(ggplot2)
library(scales)
theme_set(theme_classic())
?theme_set
# prep data
df <- read.csv("Flowrate-deltaP.csv")
#colnames(df) <- c("continent", "1952", "1957")
left_label <- paste(df$expNo, round(df$deltaPtheory),sep=", ")
?paste
right_label <- paste(df$expNo, round(df$deltaPexp),sep=", ")
df$class <- ifelse((df$deltaPtheory - df$deltaPexp) < 0, "red", "green")

# Plot
p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`deltaPtheory`, yend=`deltaPexp`, col=class), linewidth=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", linewidth=.1) + 
  geom_vline(xintercept=2, linetype="dashed", linewidth=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(x="", y="deltaP") +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`deltaPtheory`, df$`deltaPexp`))))  # X and Y axis limits

# Add texts
p <- p + geom_text(label=left_label, y=df$`deltaPtheory`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
p <- p + geom_text(label=right_label, y=df$`deltaPexp`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
p <- p + geom_text(label="deltaP theory", x=1, y=1.1*(max(df$`deltaPtheory`, df$`deltaPexp`)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="deltaP experience", x=2, y=1.1*(max(df$`deltaPtheory`, df$`deltaPexp`)), hjust=-0.1, size=5)  # title

# Minify theme
p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))
