library(tidyverse)

# From protein sequence to amino-acid map
# As seen in this paper: https://www.sciencedirect.com/science/article/pii/S0092867420304815

amino_acid_ordered <- strsplit("GAVCPILMFWSTYNQDERHK","") %>% unlist()

# >sp|P35453|HXD13_HUMAN Homeobox protein Hox-D13 OS=Homo sapiens OX=9606 GN=HOXD13 PE=1 SV=3
protein <- c("MSRAGSWDMDGLRADGGGAGGAPASSSSSSVAAAAASGQCRGFLSAPVFAGTHSGRAAAA
AAAAAAAAAAASGFAYPGTSERTGSSSSSSSSAVVAARPEAPPAKECPAPTPAAAAAAPP
SAPALGYGYHFGNGYYSCRMSHGVGLQQNALKSSPHASLGGFPVEKYMDVSGLASSSVPA
NEVPARAKEVSFYQGYTSPYQHVPGYIDMVSTFGSGEPRHEAYISMEGYQSWTLANGWNS
QVYCTKDQPQGSHFWKSSFPGDVALNQPDMCVYRRGRKKRVPYTKLQLKELENEYAINKF
INKDKRRRISAATNLSERQVTIWFQNRRVKDKKIVSKLKDTVS")

#Remove any return characters
protein <- gsub("\n", "", protein)

#Split the string into individual amino acids
aa <- strsplit(protein, "") %>% unlist()

# Generate a dataframe with the amino acids and their position
df_protein <- data.frame(aa=aa, position=1:length(aa))

# Reorder the data frame
df_protein <- df_protein %>% mutate(aa = fct_relevel(aa, amino_acid_ordered))

#Define plot
p <- ggplot()
 
# Add shade to differentiate amino acids properties
p <- p + annotate(geom = "rect", xmin = 0.5, ymin = -Inf, xmax=3.5, ymax = Inf, fill='blue', alpha=0.1)
p <- p + annotate(geom = "rect", xmin = 3.5, ymin = -Inf, xmax=5.5, ymax = Inf, fill='red', alpha=0.1)
p <- p + annotate(geom = "rect", xmin = 5.5, ymin = -Inf, xmax=10.5, ymax = Inf, fill='yellow', alpha=0.1)
p <- p + annotate(geom = "rect", xmin = 10.5, ymin = -Inf, xmax=20.5, ymax = Inf, fill='black', alpha=0.1)

# Plot a tile for position at the corresponding amino acid
p <- p + geom_tile(data=df_protein, aes(x=aa, y=position))

#Rotate plot 90 degrees
p <- p+coord_flip()+scale_x_discrete(limits = rev(amino_acid_ordered))
 
# Set the theme
p <- p+theme_classic(base_size = 14)

# Plot
p
 