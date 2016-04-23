
createMapVector <- function(vet){
  
  # Association of neighbour areas to zip codes (multiple neighbours belong
  # to a single zip code area) in one of the major city in Washington state
  
  AZScol = floor(((vet-min(vet))/max(vet-min(vet)))*100)+1
  
  AZSc  = 0*(1:119)
  mycol = green2red(101)
  
  AZSc[c(59, 62, 63)] = mycol[AZScol[1]]
  AZSc[c(48, 97, 100)] = mycol[AZScol[2]]
  AZSc[c(31, 32, 33, 34, 115, 118)] = mycol[AZScol[3]]
  AZSc[c(61, 64, 65)] = mycol[AZScol[4]]
  AZSc[c(40, 42)] = mycol[AZScol[5]]
  AZSc[c(89, 91, 107)] = mycol[AZScol[6]]
  AZSc[c(28, 30, 119)] = mycol[AZScol[7]]
  AZSc[c(43, 45, 47, 49)] = mycol[AZScol[8]]
  AZSc[c(54, 98, 99, 101)] = mycol[AZScol[9]]
  AZSc[c(35, 36, 37, 38, 39, 41, 96, 116)] = mycol[AZScol[10]]
  AZSc[c(74)] = mycol[AZScol[11]]
  AZSc[c(27, 29, 114, 117)] = mycol[AZScol[12]]
  AZSc[c(83, 84, 85, 86, 88, 95)] = mycol[AZScol[13]]
  AZSc[c(44, 46, 66)] = mycol[AZScol[14]]
  AZSc[c(60)] = mycol[AZScol[15]]
  AZSc[c(53, 55, 57)] = mycol[AZScol[16]]
  AZSc[c(102, 103, 104, 105, 106, 113)] = mycol[AZScol[17]]
  AZSc[c(75, 76, 77, 90, 92, 93, 94)] = mycol[AZScol[18]]
  AZSc[c(111, 112)] = mycol[AZScol[19]]
  AZSc[c(67, 70)] = mycol[AZScol[20]]
  AZSc[c(71, 72, 73, 78)] = mycol[AZScol[21]]
  AZSc[c(56, 58, 79, 87)] = mycol[AZScol[22]]
  AZSc[c(110)] = mycol[AZScol[23]]
  AZSc[c(50, 51, 52)] = mycol[AZScol[24]]
  
  return(AZSc)
  
}

# Notes:
# Coding in color vector starts from 27 and ends in 119
# Code 98108 (68, 69, 80, 81, 82) is missing in GIS data

#  ZIP  - NEIGHBOURS
# 98101 - 59, 62, 63
# 98102 - 48, 97, 100
# 98103 - 31, 32, 33, 34, 115, 118
# 98104 - 61, 64, 65
# 98105 - 40, 42
# 98106 - 89, 91, 107
# 98107 - 28, 30, 119
# 98109 - 43, 45, 47, 49
# 98112 - 54, 98, 99, 101
# 98115 - 35, 36, 37, 38, 39, 41, 96, 116
# 98116 - 74
# 98117 - 27, 29, 114, 117
# 98118 - 83, 84, 85, 86, 88, 95
# 98119 - 44, 46, 66
# 98121 - 60
# 98122 - 53, 55, 57
# 98125 - 102, 103, 104, 105, 106, 113
# 98126 - 75, 76, 77, 90, 92, 93, 94
# 98133 - 111, 112
# 98134 - 67, 70
# 98136 - 71, 72, 73, 78
# 98144 - 56, 58, 79, 87
# 98177 - 110
# 98199 - 50, 51, 52