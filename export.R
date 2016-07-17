### EXPORT ###

# png A4
png(filename="quick.png", type="cairo",units="cm", 
    width=21, height=12,
    pointsize=12, res=300)
last_plot()
dev.off()