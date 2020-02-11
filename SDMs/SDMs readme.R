## reading models from Gabriel Reygondeau 

install.packages("R.matlab")

library (R.matlab)
?R.matlab

?system.file
path <- system.file("C:/Users/mireia.valle/Documents/github/functional_traits/SDMs/metadata", package = "R.matlab")
pathname <- file.path(path, "FISHID.mat")
data <- readMat(pathname)
print(data)

# read in FISHID
fishid <- readMat("C:/Users/mireia.valle/Documents/github/functional_traits/SDMs/metadata/FISHID.mat")

# check out data structure
str(fishid)

#List of 1
#$ list: num [1:6432, 1] 3 25 32 58 65 66 67 68 70 71 ...
#- attr(*, "header")=List of 3
#..$ description: chr "MATLAB 5.0 MAT-file, Platform: PCWIN64, Created on: Sat Jun 01 00:19:20 2019                                        "
#..$ version    : chr "5"
#..$ endian     : chr "little"

# print out the first few rows of the dataset
head(fishid$list)

# read in FISHNAME.mat
fishname <- readMat("C:/Users/mireia.valle/Documents/github/functional_traits/SDMs/metadata/FISHNAME.mat")

# check out data structure
str(fishname)

#List of 1
#$ name:List of 6432

# print out the first few rows of the dataset
head(fishname$name)

# read in COO.mat
coordinates <- readMat("C:/Users/mireia.valle/Documents/github/functional_traits/SDMs/coordinates/COO.mat")

# check out data structure
str(coordinates)

#List of 1
#$ COO: num [1:259200, 1:3] -180 -180 -180 -180 -180 ...
#- attr(*, "header")=List of 3
#..$ description: chr "MATLAB 5.0 MAT-file, Platform: MACI64, Created on: Fri Jan 15 11:53:12 2016                                         "
#..$ version    : chr "5"
#..$ endian     : chr "little"

# print out the first few rows of the dataset
head(coordinates$COO)

