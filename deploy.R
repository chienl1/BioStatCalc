install.packages('rsconnect')
rsconnect::setAccountInfo(name='biostatcalc', token='D78F5C2160CB78EFCB5F0E4AB6B4F978', secret='dI7zyigUWGeIgy5ie0yitnUSxL/KQE9sdvndPBLi')
library(rsconnect)
rsconnect::deployApp(
  appDir = 'C:/Users/chienl1/Dropbox/course/2026/EAB703-FA/R Shinny App',
  appPrimaryDoc = 'BioStatCalc.R',
  appMode = 'shiny'
)