setwd(getwd())
library(plumber)
r <- plumb("rest_controller.R")
r$run(port=8000, host="0.0.0.0")

#below are terminal commands
#pull in base image
#docker pull rocker/r-ver:3.5.0

#run base image
#docker run -ti rocker/r-ver:3.5.0 

#build image and call it dockertest
#docker build --no-cache -t dockertest .

#run on port 80
#docker run --rm -p 80:80 dockertest

#view the api is working at this link
#http://127.0.0.1/predict_petal_length?petal_width=1

#tag the image name and dockerhub address
#docker tag dockertest mattwanz/dockertest

# push the image to dockerhub
#docker push mattwanz/dockertest

# droplet instructions ssh in
# sudo apt update
# sudo apt install apt-transport-https ca-certificates curl software-properties-common
# curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
# sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"
# sudo apt update
# apt-cache policy docker-ce
# sudo apt install docker-ce

# check if it's working
# sudo systemctl status docker

# api_output <- POST("http://167.172.118.197:8000/response?text=purchase%20order%20import",verbose())# pull in the image from dockerhub
# docker pull mattwanz/dockertest

# run the service
# docker run --rm -p 8000:80 mattwanz/dockertest

# clean out old docker containers
# docker kill $(docker ps -q)
# docker_clean_ps
# docker rmi $(docker images -a -q)

#force remove image
#docker rmi -f image_id

# testing
# require(httr)
# require(jsonlite)

# this.answer <- fromJSON(rawToChar(api_output$content))
