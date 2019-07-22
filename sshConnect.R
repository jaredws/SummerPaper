### SSH script for running the main 
### executibles on the hoec server

library(ssh)


session <- ssh_connect("jaredws@hoek.eecis.udel.edu")
#print(session)


## UPload all the files first, to account for any changes
## Then run each main, taking the output data

