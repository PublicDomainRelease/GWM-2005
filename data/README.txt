
                 MF2005-GWM Sample Problems

Data files for four test problems are provided to confirm that 
MF2005-GWM is correctly installed and running on the system.  
The tests also may be looked at as examples of how to use the program.  
The input data files are organized by sample problem:

Test name      Description of test
------------   -------------------------------------------------------
DEWATER        DEWATER sample problem, linear formulation
DEWATERMB      DEWATER sample problem, mixed-binary linear formulation
SEAWATER       SEAWATER sample problem, nonlinear formulation
SUPPLY2        SUPPLY2 sample problem, nonlinear formulation

The DEWATER, DEWATERMB, and SEAWATER problems are described in Ahlfeld
and others (2005). The SUPPLY2 problem was initially the SUPPLY problem
as described in Ahlfeld and others (2005). However, experience with the
sample problem indicated that there could be numerical-stability
issues associated with its solution. Therefore, the problem was revised
and renamed SUPPLY2. Changes made to the sample problem are described
in the file SFR_update.pdf in directory mf2005_gwm.1_1\doc.

The directory mf2005_gwm.1_1\test-win can be used to conveniently run the
tests without destroying the original results in the mf2005_gwm.1_1\data
subdirectories. The mf2005_gwm.1_1\test-win directory contains batch (BAT) 
files to run the tests.  Each test can be run by entering the name of 
the test as a command in a DOS command-prompt window with the current 
directory being mf2005_gwm.1_1\test-win or by double-clicking on the 
corresponding BAT file in Windows Explorer.  The output files that are 
created in mf2005_gwm.1_1\test-win can then be compared to those in the
mf2005_gwm.1_1\data subdirectories. The GWM output file and MODFLOW list 
file provided in the SUPPLY2 subdirectory are those generated using the
SFR Package. The user can easily switch between using either the STR or
SFR Packages for the sample problem by commenting out (using #) either the 
STR or SFR Packages in the supply2.nam file.



Reference:

Ahlfeld, D.P., Barlow, P.M., and Mulligan, A.E., 2005, GWM--A ground-water
management process for the U.S. Geological Survey modular ground-water
model (MODFLOW-2000): U.S. Geological Survey Open-File Report 2005-1072,
124 p.
