#========================================================================================
# script to plot the water potential data for the 2018 chilling experiment on white pine 
# at Harvard Forest.
#----------------------------------------------------------------------------------------

# source water potential data
#----------------------------------------------------------------------------------------
source ('./readWaterPotential.R')
source ('./plotingFunctions.R')

# plot needle and branch water potential
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
plot (x = phi [['date']],
      y = phi [['phi.branch']],
      col = addOpacity (tColours [['colour']] [phi [['treatment']]], 0.7),
      xlab = 'date', ylab = expression (paste (phi[branch],'(MPa)', sep = '')),
      las = 1, pch = 19, 
      ylim = c (0, -1))

# make boxplots by date
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2019WaterPotential.png', width = 700, height = 400)
layout (matrix (1:2, byrow = TRUE, nrow = 2), height = c (1, 1.2))
par (mar = c (1, 6, 1, 1))
b1 <- boxplot (phi.leaf ~ treatment + date, data = phi [phi [['treatment']] == 1, ], 
               col = addOpacity (tColours [['colour']] [1], 0.5),
               border = tColours [['colour']] [1],
               xlim = c (as_date ('2019-05-15'),as_date ('2019-09-01')), ylim = c (-1, 0), 
               axes = FALSE, frame = FALSE,
               xlab = '', ylab = expression (paste (phi,' (MPa)', sep = '')), 
               out.pch = 19, out.col = addOpacity (tColours [['colour']] [1], 0.7),
               at = unique (phi [['date']] [phi [['treatment']] == 1])-0.5)
b2 <- boxplot (phi.leaf ~ treatment + date, data = phi [phi [['treatment']] == 5, ], 
               col = addOpacity (tColours [['colour']] [5], 0.5),
               border = tColours [['colour']] [5], add = TRUE, 
               ylim = c (-1, 0), axes = FALSE, frame = FALSE,
               out.pch = 19, out.col = addOpacity (tColours [['colour']] [5], 0.7),
               at = unique (phi [['date']] [phi [['treatment']] == 5])+0.5)
axis (side = 2, las = 1)

# add panel descriptors
#----------------------------------------------------------------------------------------
text (labels = 'Leaf', x = as_date ('2019-08-25'), y = -0.1, col = '#666666', 
      cex = 1.5)

par (mar = c (3, 6, 1, 1))
b1 <- boxplot (phi.branch ~ treatment + date, data = phi [phi [['treatment']] == 1, ], 
               col = addOpacity (tColours [['colour']] [1], 0.5),
               border = tColours [['colour']] [1],
               xlim = c (as_date ('2019-05-15'),as_date ('2019-09-01')), ylim = c (-1, 0), 
               axes = FALSE, frame = FALSE,
               xlab = '', ylab = expression (paste (phi,' (MPa)', sep = '')), 
               out.pch = 19, out.col = addOpacity (tColours [['colour']] [1], 0.7),
               at = unique (phi [['date']] [phi [['treatment']] == 1])-0.5)
abline (h = 0.04, col = '#66666666')
b2 <- boxplot (phi.branch ~ treatment + date, data = phi [phi [['treatment']] == 5, ], 
               col = addOpacity (tColours [['colour']] [5], 0.5),
               border = tColours [['colour']] [5], add = TRUE, 
               ylim = c (-1, 0), axes = FALSE, frame = FALSE,
               xlab = '', ylab = '', 
               out.pch = 19, out.col = addOpacity (tColours [['colour']] [5], 0.7),
               at = unique (phi [['date']] [phi [['treatment']] == 5])+0.5)
axis (side = 2, las = 1)

# add x-axis
#----------------------------------------------------------------------------------------
axis (side = 1, at = seq (as_date ('2019-06-01'), as_date ('2019-09-01'), length.out = 4),
      labels = c ('Jun','Jul','Aug','Sep'))

# add panel descriptors
#----------------------------------------------------------------------------------------
text (labels = 'Branch', x = as_date ('2019-08-25'), y = -0.1, col = '#666666', 
      cex = 1.5)
dev.off ()
#========================================================================================

