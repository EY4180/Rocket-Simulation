# Rocket Simulation
## Introduction
This was just a small project I used to learn how to use forms in pascal. The program is able to run a simulation of a water bottle's flight in 2D. It will report back to you the total distance travelled, max height, and the respective velocitys/displacements at the end of each phase of flight. 

Additionally, the program produces a flight and pressure graph which are saved to the "Reports" directory. And if you choose to generate a report, the report will be saved as a .pdf to the "Reports" directory.

Like in the original project, the ability to goal-seek is avaliable. 

Overall, I aimed to create a portable version of the Flight Simulation Assignment that used propper programming constructs to simulate the flight of a rocket as opposed to simply using excel. This should be used as a drop-in replacement for the assignment and I hope either the program or code is of use to you. 

## How To Use
The .exe is all that is needed to run this program. There are 3 folders that will be created in the directory of the .exe at runtime. These are: Profiles, Fonts and Reports.

## To Do List
I stil need to tidy up the user input validation a little bit. For example it is still possible to enter an actual distance that is less than the lenght of the tube. Additionally there is an upper-bound to the rockets distance that if reached causes the program to behave unexpectedly. No checks have been implemented on the initial volume of water being less than the capacity of the bottle either.
