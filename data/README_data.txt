
                      GWM-2005 Sample Problems

Data files for seven test problems are provided to confirm that 
GWM-2005 is correctly installed and running on the system.  
The tests also may be looked at as examples of how to use the program.  
The input data are organized by sample problem:

Test name      Description of test
------------   -------------------------------------------------------
DEWATER        DEWATER sample problem, linear formulation
DEWATER-LGR    DEWATER sample problem with local grid refinement
DEWATERMB      DEWATER sample problem, mixed-binary linear formulation
SEAWATER       SEAWATER sample problem, nonlinear formulation
SEAWATER-LGR   SEAWATER sample problem with local grid refinement
SUPPLY2        SUPPLY sample problem, nonlinear formulation
SUPPLY-3GRID   A second SUPPLY sample problem with local grid refinement

The DEWATER, DEWATERMB, and SEAWATER problems are described in Ahlfeld
and others (2005). The SUPPLY2 problem was initially the SUPPLY problem
as described in Ahlfeld and others (2005). However, experience with the
sample problem indicated that there could be numerical-stability
issues associated with its solution. Therefore, the problem was revised
and renamed SUPPLY2, as described in Ahlfeld and others (2009).

The DEWATER-LGR, SEAWATER-LGR, and SUPPLY3-GRID problems are described
in Ahlfeld and others (2009).

The directory GWM2005.1_2_2\test-win can be used to conveniently run the
tests without destroying the original results in the GWM2005.1_2_2\data
subdirectories. The GWM2005.1_2_2\test-win directory contains batch (BAT) 
files to run the tests.  Each test can be run by entering the name of 
the test as a command in a DOS command-prompt window with the current 
directory being GWM2005.1_2_2\test-win or by double-clicking on the 
corresponding BAT file in Windows Explorer.  The output files that are 
created in GWM2005.1_2_2\test-win can then be compared to those in the
GWM2005.1_2_2\data subdirectories. Note that there are three batch files
provided for the DEWATER-LGR test problem, one each for the three levels
of horizontal grid refinement (1:1, 3:1, and 7:1).


References:

Ahlfeld, D.P., Baker, K.M., and Barlow, P.M., 2009, GWM-2005--A Groundwater-
Management Process for MODFLOW-2005 with Local Grid Refinement (LGR)
capability: U.S. Geological Survey Techniques and Methods, 6-A33, 65 p.

Ahlfeld, D.P., Barlow, P.M., and Mulligan, A.E., 2005, GWM--A ground-water
management process for the U.S. Geological Survey modular ground-water
model (MODFLOW-2000): U.S. Geological Survey Open-File Report 2005-1072,
124 p.