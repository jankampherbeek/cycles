# Enigma - Cycles

## Programmers manual version 0.1

Information for programmers about the program *Enigma Cycles*.

### Introduction

Enigma Cycles (from now on *Cycles* for short) supports the calculation and presentation of several cycles. For a description of the functionality, please check the User Manual.

This document is for developers who want to use the code, create an own version or are just curious.

Cycles is free software and open source. You are allowed to use it provided your own software is also free and open source. The same goes for the use of the Swiss Ephemeris, a library for astronomical calculations that is used by Cycles. Check the file copyright.txt in the root of the source for a formal description about restrictions and possibilities regarding copyright.

Please note that my experience as a programmer is mainly with Java. Cycles is written in *Free Pascal*. I have used several Pascal versions before but that was mostly a long time ago. So I am actually learning on the job. This will not result in the best code possible but I will try to improve it consistently.

### Setting up your development environment

The source of Cycles is written in Free Pascal, an open source implementation of Object Pascal and to a high degree compatible with Delphi. However, I do use some constructions that are not supported by Delphi. If you want to use Delphi, you will need to make some changes in the code. 

If you want to check the code of Cycles and compile the program yourself, you should use Free Pascal and the standard IDE called *Lazarus*.

#### Free Pascal and Lazarus

You can install Lazarus which includes a current version of FreePascal. Download Lazarus from https://www.lazarus-ide.org/  and navigate to *Downloads*. Make sure you select *Windows (32 and 64 Bits)*. 

Lazarus also works on Linux but I did not test that.

The installation itself is straightforward. One point to keep in mind: do not install in the standard *Program files* folder. This will cause troubles if you want to update and updates happen frequently.  The standard folder *c:\lazarus* is the best way to go.

#### Additional packages

It is easy to install additional packages in Lazarus. This will cause the environment to be rebuild, but that is an automatic process.

Cycles uses the following additional packages:

- BGRABitmap (graphics, supports anti-aliasing)
- BGRAControls (required for BGRABitmap)
- 

#### External software and data

Enigma uses software by the Swiss Ephemeris (SE) to perform most astronomical calculations. You need to install the following:

- The file swedll32.dll
- The datafiles from the SE.

You can use the dll and datafiles that are part of the installation package of Enigma-Cycles. The dll should be in the root of your Lazarus project. The datafiles should be in a folder *se* that is one level up, so at the same level as folder for the project itself.





#### Tools

..

#### Source

..

### Architecture

..

#### Separation of concerns

..

#### Interfaces

I apply interfaces for two reasons: to facilitate testing and to support loosely coupling. I do not define an interface for each object, only if it makes sense because of the reasons I just mentioned.

Free Pascal supports two types of interfaces: com-interfaces and CORBA-interfaces. In Cycles I only use CORBA-interfaces. The name is actually misleading as CORBA is not used. These interfaces do not support reference-count, which I consider as an advantage, especially as I do not want to create an interface for everything. Actually, the CORBA-interface is comparable with interfacing in Java or Kotlin. 

CORBA-interfaces are specific for Free Pascal and not supported in Delphi.

#### Testing

..



#### Ini-files and enumerated types

..



### Astronomical



#### Period of validity of celestial points

The Swiss Ephemeris supports calculations from 13201 BCE up to 17191 CE.  For several non planetary bodies a shorter period is available, depending on the current knowledge about these bodies.

The following table shows the Celestial Points that are available and the period that they are supported by the SE.

| Celestial Point(s)                                           |   Start JD |    End JD |
| :----------------------------------------------------------- | ---------: | --------: |
| Sun, Moon, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto | -3026613.5 | 5227458.5 |
| Mean Lunar Node, Mean Apogee                                 | -3100023.5 | 8000007,5 |
| True Lunar Node, Oscul. Apogee                               | -3027210.5 | 7857131.5 |
| Chiron                                                       |  1967601.5 | 3419437.5 |
| Pholus                                                       |   640648.5 | 4390615.5 |
| Nessus                                                       |   625372.5 | 2816371.5 |
| Ceres                                                        | -3026613.5 | 5224242.5 |
| Pallas, Juno                                                 | -3026613.5 | 5227458.5 |
| Vesta                                                        | -3026613.5 | 5221544.5 |
| Huya, Ixion, Orcus, Varuna                                   |   625296.5 | 2816295.5 |
| MakeMake, Haumea, Quaoar                                     |   625292.5 | 2816291.5 |
| Eris                                                         |   625384.5 | 2816383.5 |
| Sedna                                                        |   624947.5 | 2816295.5 |
|                                                              |            |           |





