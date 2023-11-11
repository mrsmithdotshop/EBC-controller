# EBC-controller
This is a fork of [EBC-controller](https://github.com/JOGAsoft/EBC-controller) that supports the EBCC A20 and A40 as well.
among bug fixes additional features and fixes are:
- parsing and checking of step files
- syntax highlighting while editing step files
- range checking for current / voltage for connected charger
- connect timeout checks
- status line
- shortcuts for most command/fields as well as show shortcuts via help
- corrected tab orders
- auto csv log file naming
- csv includes AH as well as time
- auto save steps if auto log is enabled
- fixed and enabled serial checksums
- moved popup menu to a traditional menu
- added function keys
- Hacks and fixes for Windows
- Multi language support (currently English and German)
## What is this ?
A GUI software for linux (as well as Windows) to control the ZTE Tech EBC series
battery testers and electronic loads A05, A10, A20 and A40.
This version has been tested with the A20 and A40 only as i do not have an A05 or A10, testers are welcome.

Written in Free Pascal/Lazarus.

Needs TLazSerial package (to be removed).

The aim is to provide the same or more functionallity as the Windows software
from ZKE Tech (Except multiple devices from one instance but you can run
multiple instances at the same time). Supports the EBC-A05,EBC-A10H,A20 and
A40 devices by default.
Other devices probably work. You can add the identification byte for
any device in the .conf file (run the program once first to auto-generate
the .conf file based on the .init file).

Added features over the original software:
* More versatile cut offs, such as current, capacity or energy
* A software Constant Resistance (CR) mode
* Better loop controls for cycling programs
   (nested loops, loops until capacity drops)
* Shows more parameters (dV, dA, etc)

## Usage
The Tabs Charge and Discharge are more or less self-explanatory, select the battery type amd set/adjust the voltage, current as well as other parameters and press start to activate.
### Steps
Using steps you can perform several steps like
`charge to a defined voltage using a defined current`
`wait for some time`
`discharge to a defined value e.g.  current or voltage`
`loop for a defined number`
Use F3 to load a step file or Shift-F3 to edit the loaded or a default stepfile.


