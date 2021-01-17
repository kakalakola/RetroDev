A little something or another I'm working on to wrap my mind around Mode 7 features of the SNES. The scaling is linear in this demo, which is probably why it looks wonky.

If you're using an emuator with debugging features, like MAME or Mesen S, you can modify the work RAM directly to see what some of the registers does. Mind you, the SNES CPU is little-endian. :D

Editable values in RAM are as follows:
 - $06 is the line counter, changing it changes when HDMA starts
 - $10-11 is the X scale amount in little-endian format (setting it around $10 starts producing weird results)
 - $12-13 is the Y scale amount
 - $14-15 is the initial value of Mode 7 Matrix A register. $0100 is a scale of 1.0 on the X axis
 - $16-17 is the initial value of Mode 7 Matrix D register. $0100 is a scale of 1.0 on the Y axis
 - $17-18 controls Mode 7 Center X
 - $19-1a controls Mode 7 Center Y
