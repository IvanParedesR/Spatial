install.packages("XML")
library("XML", lib.loc="~/R/win-library/3.5")

# Also load the other required package.
library("methods")

# Give the input file name to the function.
result <- xmlParse(file = "D:/Users/iparedes/Downloads/prices (5).xml")

# Print the result.
print(result)

# Exract the root node form the xml file.
rootnode <- xmlRoot(result)

# Find number of nodes in the root.
rootsize <- xmlSize(rootnode)

# Print the result.
print(rootsize)

# Convert the input xml file to a data frame.
xmldataframe <- xmlToDataFrame("D:/Users/iparedes/Downloads/prices (4).xml")
print(xmldataframe)