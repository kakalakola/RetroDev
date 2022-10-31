# NES Input Handler #

Update 2022.10.30
- Changed the CHR ROM size from 8 Kb (which works in MESEN, but causes MAME to throw an error) to 16 Kb. As of this update, the ROMS have been tested with, and works in MAME, MESEN, and NESTICLE. Though admittedly, NESTICLE does not support the NES Four Score.™

A series of examples on how to read data from the controller ports ($4016 & $4017)
- NES Input Handler 00 reads data from $4016 and stores it in RAM
- NES Input Handler 01 reads data from both controller ports
- NES Input Handler 02 uses input from controller 1 to move a sprite on screen
- NES Input Handler 03 uses input from controllers 1 and 2 to move sprites on screen using slightly more complex code
- NES Input Handler 04 moves sprites on screen using directional vectors
- NES Input Handler 05 deals with reading data from an NES Four Score™

== We're Using GitHub Under Protest ==

This project is currently hosted on GitHub.  This is not ideal; GitHub is a proprietary, trade-secret system that is not Free and Open Souce Software (FOSS).  We 
are deeply concerned about using a proprietary system like GitHub to develop our FOSS project. We urge you to read about the [Give up GitHub](https://GiveUpGitHub.org) campaign from [the Software Freedom Conservancy](https://sfconservancy.org) to understand some of the reasons why GitHub is not
a good place to host FOSS projects.

![Logo of the GiveUpGitHub campaign](https://sfconservancy.org/img/GiveUpGitHub.png)
