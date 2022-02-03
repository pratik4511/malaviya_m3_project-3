# 01 Name & Packages
print('PRATIKKUMAR INDRAVADAN MALAVIYA')
install.packages("FSA")
library(FSA)
install.packages("FSAdata")
library(FSAdata)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages('tidyr')
library(tidyr)
install.packages('plyr')
library(plyr)
install.packages('tidyverse')
library(tidyverse)

# 02 inmporting bio.csv
bio = read.csv('/Users/pratik_4511/Desktop/Quarter_1A/M3/inchBio.csv')
print(bio)

# 03 Display the head, tail and structure of <bio>
print(headtail(bio)) #headtail
print(structure(bio)) #structure

# 04 Create an object, <counts>, that counts and lists all the species records,
counts  = table(bio$species)
print(counts)

# 05 #Display just the 8 levels (names) of the species
print(unique(bio$species))

# 06 displays the different species and the number of record 
tmp = count(bio$species)
print(tmp)

# 07 <tmp2>, of just the species variable and display the first five records
tmp2 = subset(bio,select = species)
print(head(tmp2,n=5))

# 08 Create a table, <w>, of the species variable. Display the class of w
w = table(bio$species)
print(w)
class(w)

# 09 Convert <w> to a data frame named <t> and display the results
t = data.frame(w)
print(t)

# 10 Extract and display the frequency values from the <t> data frame
t %>%
  select(Freq)

# 11 Create a table named <cSpec>
cSpec = table(bio$species)
print(cSpec)

#12 <cSpecPct> that displays the species and percentage of records 
cSpecPct = prop.table(cSpec)*100
print(cSpecPct)

#13 Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame
u = data.frame(cSpecPct) 
print(u)
class(u)

#14 barplot of <cSpec>
barplot(cSpec,
        main = 'Fish Count',
        ylab = 'COUNTS',
        col = 'Light Green',
        cex.names = 0.60,
        las = 2)

#15 barplot of <cSpecPct>
barplot(cSpecPct,
        ylim = c(0,40),
        ylab = 'COUNTS',
        col.lab = 'Light Blue',
        main = 'Fish Relative Frequency')

#16 Rearrange the <u> cSpec Pct data frame in descending order of relative frequency. Save
#the rearranged data frame as the object <d>
d = u[order(-u$Freq),]
print(d)

#17 Rename the <d> columns Var 1 to Species, and Freq to RelFreq
colnames(d) = c('Var1','freq')
colnames(d)[colnames(d) %in% c('Var1','freq')] = c('Species','Relfreq')
print(d)

#18 Add new variables to <d> and call them cumfreq, counts, and cumcounts
counts
t$Freq
tdescending  = t[order(-t$Freq),] # Assign to variable &converting in to descending order
tdescending$Freq
d = d %>%
  mutate(cumfreq = cumsum(d$Relfreq),
         counts = tdescending$Freq,
         cumcounts  = cumsum(tdescending$Freq)
         )
print(d)

#19 Create a parameter variable <def_par> to store parameter variables
def_par = par(no.readonly = TRUE)

#20 barplot <pc>
pc = barplot(d$counts, 
             width = 1,
             space = 0.15,
             border = NA, 
             axes = F, 
             ylim = c(0,3.05*228), 
             ylab = "Cummulative Counts", 
             names.arg = d$Species, 
             las=2, 
             cex.names = 0.60, 
             main = "Species Pareto", 
             d$counts,na.rm=TRUE)

#21 Add a cumulative counts line to the <pc> plot, 
pc = barplot(d$counts, 
             width = 1,
             space = 0.15,
             border = NA, 
             axes = F, 
             ylim = c(0,3.05*228), 
             ylab = "Cummulative Counts", 
             names.arg = d$Species, 
             las=2, 
             cex.names = 0.60, 
             main = "Species Pareto", 
             )
lines(pc,
      d$cumcounts,
      type = 'b',
      cex = 0.7,
      pch = 19,
      col = 'cyan4')

#22  Place a grey box around the pareto plot
pc = barplot(d$counts, 
             width = 1,
             space = 0.15,
             border = NA, 
             axes = F, 
             ylim = c(0,3.05*228), 
             ylab = "Cummulative Counts", 
             names.arg = d$Species, 
             las=2, 
             cex.names = 0.60, 
             main = "Species Pareto", 
             )
lines(pc,
      d$cumcounts,
      type = 'b',
      cex = 0.7,
      pch = 19,
      col = 'cyan4')
box(col= 'grey62')

#23 Add a left side axis 
pc = barplot(d$counts, 
             width = 1,
             space = 0.15,
             border = NA, 
             axes = F, 
             ylim = c(0,3.05*228), 
             ylab = "Cummulative Counts", 
             names.arg = d$Species, 
             las=2, 
             cex.names = 0.60, 
             main = "Species Pareto", 
)
lines(pc,
      d$cumcounts,
      type = 'b',
      cex = 0.7,
      pch = 19,
      col = 'cyan4')
box(col= 'grey62')
axis(side = 2,
     cex.axis = 0.8,
     at = c(0, d$cumcounts),
     las = 1,
     col.axis = 'grey60',
     col = 'grey60')

#24 Add axis details on right side of box 
pc = barplot(d$counts, 
             width = 1,
             space = 0.15,
             border = NA, 
             axes = F, 
             ylim = c(0,3.05*228), 
             ylab = "Cummulative Counts", 
             names.arg = d$Species, 
             las=2, 
             cex.names = 0.60, 
             main = "Species Pareto", 
)
lines(pc,
      d$cumcounts,
      type = 'b',
      cex = 0.7,
      pch = 19,
      col = 'cyan4')
box(col= 'grey62')
axis(side = 2,
     cex.axis = 0.8,
     at = c(0, d$cumcounts),
     las = 1,
     col.axis = 'grey60',
     col = 'grey60')
axis(side = 4,
     at = c(0,d$cumcounts),
     labels =  c(0,d$cumfreq),
     las = 1,
     col.axis = 'cyan',
     col.lab = 'cyan4',
     cex.axis = 0.8)

#25  finished Species Pareto Plot (without the star watermarks)
pc = barplot(d$counts, 
             width = 1,
             space = 0.15,
             border = NA, 
             axes = F, 
             ylim = c(0,3.05*228), 
             ylab = "Cummulative Counts", 
             names.arg = d$Species, 
             las=2, 
             cex.names = 0.60, 
             main = "Species Pareto \n Pratik Malaviya", 
             )
lines(pc,
      d$cumcounts,
      type = 'b',
      cex = 0.7,
      pch = 19,
      col = 'cyan4')
box(col= 'grey62')
axis(side = 2,
     cex.axis = 0.8,
     at = c(0, d$cumcounts),
     las = 1,
     col.axis = 'grey60',
     col = 'grey60')
axis(side = 4,
     at = c(0,d$cumcounts),
     labels =  c(0,d$cumfreq),
     las = 1,
     col.axis = 'cyan',
     col.lab = 'cyan4',
     cex.axis = 0.8)

