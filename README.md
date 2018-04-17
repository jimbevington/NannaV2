# 'Nanna' - a UI Offline Granular Synthesis

Implements a graphical user interface to Lopez-Lezcarno's [*grani*](https://ccrma.stanford.edu/~nando/clm/grani/) instrument for Common Lisp Music.

The UI is built in Max/MSP, communicating with *grani* via [*Slippery Chicken's*](http://www.michael-edwards.org/sc) OSC listener.

Both Slippery Chicken and Max/MSP are required to run the app.

**Note: The current version was written for personal use in a composition project.** **An improved, lightweight and less buggy version is in the works.**

Any questions: ping me here or at [bevingtonaudio@gmail.com](mailto:bevingtonaudio@gmail.com).

## Instructions

* open *jb-Nanna-Grani.lsp* with Slippery Chicken
* rename File Locations as appropriate

in the REPL, 

* compile *jb-Nanna-Grani.lsp*
* in Slippery Chicken, run *(osc-call)*

Open jb-Nanna-Grani.maxpat in Max/MSP

* Draw some Envelopes
* Load Source File
* Set the Output Duration
* Set Output Filenames
* Click "Gen Single File" or "Gen 5 Files"

