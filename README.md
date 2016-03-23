mateable 0.3.0 is now available on CRAN.


With this release you can organize mating scene data
(makeScene and simulateScene), visualize mating scenes
(plotScene, plot3DScene, and plotPotential), do some
basic analysis of mating scenes (synchrony, proximity,
compatibility), and a few other useful tasks
(matingSummary, overlap, etc.)!


This repo provides source code for an R package called
mateable. This package contains functions that
help the study of mating phenology.

Using the package devtools, you can use the following code to
automatically install package mateable.

install.packages("devtools") # if you don't already have it
library(devtools)
install_github("danhan52/mateable")
library(mateable)
?mateable

You should see a help page. Look at the index link to see what
functions are available. Copy and paste the example code to see
what happens!

Let me know if you have any questions.

If you are interested in contributing to this package, fork
the repo and modify/add functions as you see fit. After
modifying the repo, create test code that shows that the
modifications work in a variety of contexts. Also that when the modifications
show error messages, they aren't indecipherable. Submit a pull
request with your test code and someone from the Echinacea
Project will review the code and accept, deny, or suggest
modifications to your changes.
For more advanced programmers, this packages uses Rcpp to take
advantage of the speed benefits of C++. If you think that
some part of this would be optimized through C++ programming,
add your code to the src/ folder and make sure that the package
builds.
Happy coding!


For Team Echinacea Members:
Test code (more than just the example) can be found in:
/Dropbox/mateableTestCode/
Just ask Stuart or Danny to share it with you.
