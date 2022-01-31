FROM rocker/tidyverse:latest

# create an R user 
ENV USER rstudio 
## Copy your working files over 
COPY . /home/$USER/github 


RUN R -e "install.packages('lavaan', dependencies=TRUE)"
RUN R -e "install.packages('foreign', dependencies=TRUE)"
RUN R -e "install.packages('psych', dependencies=TRUE)"
RUN R -e "install.packages('ggjoy', dependencies=TRUE)"
RUN R -e "install.packages('readr', dependencies=TRUE)"
RUN R -e "install.packages('ggridges', dependencies=TRUE)"

##Yes, I know -- but once you start cutting and pasting -- sunk costs make it hard to stop!
