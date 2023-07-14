library(tidyverse) # metapackage of all tidyverse packages

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "../input")

library(tidyverse)

ipl = read_csv('"C://Users//parip//Downloads//IPL Ball-by-Ball 2008-2020.csv"')

ipl %>%
  ggplot()+
  geom_bar(aes(x = result_margin ),
           fill = "pink",
           color = "black")