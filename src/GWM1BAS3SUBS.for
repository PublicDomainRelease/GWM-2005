C
C
      MODULE GWM1BAS3SUBS
C     VERSION: 21MAR2012
      USE GWM_STOP, ONLY:   GSTOP
      IMPLICIT NONE
      PRIVATE
      PUBLIC::GWM1BAS3AR,GWM1BAS3RW,GWM1BAS3RRF,
     &        GWM1BAS3DABAS7,GWM1BAS3AP
C
      INTEGER, PARAMETER :: I4B = SELECTED_INT_KIND(9)
      INTEGER, PARAMETER :: I2B = SELECTED_INT_KIND(4)
      INTEGER, PARAMETER :: SP = KIND(1.0)
      INTEGER, PARAMETER :: DP = KIND(1.0D0)
      INTEGER, PARAMETER :: LGT = KIND(.TRUE.)
C
C-----FOR ERROR HANDLING
      CHARACTER(LEN=200)::FLNM 
      CHARACTER(LEN=20)::FILACT,FMTARG,ACCARG,FILSTAT
      INTEGER(I4B)::NDUM
      REAL(SP)::RDUM
C
      CONTAINS
C***********************************************************************
      SUBROUTINE GWM1BAS3AR(IOUT,NPER,PERLEN,HCLOSE,VERSION,
     1                      MFVNAM,NGRIDS)
C***********************************************************************
C     VERSION: 09JAN2010
C     PURPOSE: READ AND PREPARE ALL INFORMATION FOR GWM
C-----------------------------------------------------------------------
      USE GWM1BAS3, ONLY : GWMOUT,SMALLEPS,IGETUNIT
      USE GWM1DCV3, ONLY : NFVAR,NEVAR,NBVAR,GWM1DCV3AR,GRDLOCDCV
      USE GWM1OBJ3, ONLY : GWM1OBJ3AR
      USE GWM1RMS3, ONLY : NRMC,NCON,NV,NDV,HCLOSEG
      USE GWM1RMS3SUBS, ONLY : GWM1RMS3AR
      USE GWM1DCC3, ONLY : GWM1DCC3AR
      USE GWM1STC3, ONLY : GWM1STC3AR,STCNUM
      USE GWM1HDC3, ONLY : GWM1HDC3AR,HDCNUM
      USE GWM1STA3, ONLY : GWM1STA3AR
      USE GWM1SMC3, ONLY : GWM1SMC3AR
      USE GLOBAL,   ONLY : GLOBALDAT
      INTEGER(I4B),INTENT(IN)::IOUT,NPER,NGRIDS
      REAL(SP),INTENT(IN)::PERLEN(:)
      REAL(SP),INTENT(IN)::HCLOSE
      CHARACTER(LEN=40),INTENT(IN)::VERSION
      CHARACTER(LEN=10),INTENT(IN)::MFVNAM
      
      INTERFACE  
C
        SUBROUTINE URDCOM(IN,IOUT,LINE)
        CHARACTER*(*) LINE
        END
C
        SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
        CHARACTER*(*) LINE
        CHARACTER*20 STRING
        CHARACTER*30 RW
        CHARACTER*1 TAB
        END
C        
      END INTERFACE
C-----LOCAL VARIABLES
      INTEGER(I4B)::LLOC,IKEYS,IKEYF,INAMES,INAMEF,I,J,INTEMP,NGWMFILE
      CHARACTER(LEN=200)::FNAME,LINE
      INTEGER(I4B)::LENVER,INDENT
      INTEGER(I4B)::NUNOPN=99
      CHARACTER(LEN=10)::KEYWORD
      CHARACTER(LEN=200),ALLOCATABLE::FNAMEOPT(:,:)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C-----DEFINE THE NUMBER OF GWM FILE TYPES 
      NGWMFILE = 8
C
C-----DEFINE THE INTERNAL HCLOSE VALUE
      IF(HCLOSE.NE.100000.0)THEN
        HCLOSEG = REAL(HCLOSE,DP)
      ELSE
        HCLOSEG = SMALLEPS
      ENDIF
C
C-----FIND A UNIT NUMBER FOR THE GWM OUTPUT FILE
      GWMOUT=IGETUNIT(7,95)
C-----LOOK FOR A FILE NAME FOR THE GWM OUTPUT FILE 
      OPEN(UNIT=NUNOPN,STATUS='SCRATCH')         ! OPEN TEMP FILE 
      INTEMP=GLOBALDAT(1)%IUNIT(56)              ! RETRIEVE PARENT GWM FILE
      CALL URDCOM(INTEMP,NUNOPN,LINE)            ! READ FIRST LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,IKEYS,IKEYF,1,NDUM,RDUM,GWMOUT,INTEMP)
      CALL URWORD(LINE,LLOC,INAMES,INAMEF,0,NDUM,RDUM,GWMOUT,INTEMP)
      IF(LINE(IKEYS:IKEYF).EQ.'OUT')THEN
        FNAME=LINE(INAMES:INAMEF)                ! ASSIGN OUT FILE NAME
      ELSE                                       ! KEYWORD WAS SOMETHING ELSE
        FNAME='GWM.OUT'                          ! USE DEFAULT NAME
        BACKSPACE(INTEMP)                        ! NEED TO RE-READ LINE
      ENDIF
C-----OPEN GWM OUTPUT FILE
      OPEN(UNIT=GWMOUT,FILE=FNAME,STATUS='UNKNOWN',ACTION='READWRITE')
C
C-----WRITE HEADER
      IF(MFVNAM.EQ.'VI')THEN                     ! CALLED FROM GWM-VI
        WRITE(GWMOUT,1005)VERSION
      ELSE
        WRITE(GWMOUT,1000)MFVNAM,VERSION
      ENDIF
      IF(NGRIDS.EQ.1)WRITE(GWMOUT,1010)INTEMP
      IF(NGRIDS.GT.1)WRITE(GWMOUT,1015)INTEMP
C
C-----WRITE ANY COMMENTS THAT WERE PRESENT BEFORE FIRST COMMAND LINE
      REWIND(NUNOPN)
   10 READ(NUNOPN,'(1X,A)',END=20)LINE
      WRITE(GWMOUT,'(1X,A)')LINE
      GO TO 10
   20 CLOSE(NUNOPN)                              ! CLOSE/ERASE TEMP FILE
C
C-----READ ALL FILE NAMES FROM GWM FILES AND STORE IN FNAMEOPT MATRIX
C     PARENT: DECVAR, OBJFNC (REQD), VARCON, SUMCON, HEDCON, STRMCON, SOLN (REQD)
C     CHILDREN: DECVAR, VARCON, HEDCON, STRMCON
C               AT LEAST ONE DECVAR IS REQD WITH ASSOCD VARCON
C
      ALLOCATE (FNAMEOPT(NGRIDS,NGWMFILE)) ! CREATE STORAGE
      FNAMEOPT=' '                               ! BLANK STORAGE
      DO 210 I=1,NGRIDS                          ! LOOP OVER ALL GRIDS
        IF(GLOBALDAT(I)%IUNIT(56).GT.0)THEN      ! GWM FILE ON THIS GRID
          INTEMP=GLOBALDAT(I)%IUNIT(56)          ! RETRIEVE UNIT NUMBER
          IF(I.GT.1)WRITE(GWMOUT,1016)INTEMP     ! WRITE HEADER FOR CHILD
 200      READ(INTEMP,'(A)',ERR=991,END=210)LINE ! READ A LINE
          LLOC=1
          CALL URWORD(LINE,LLOC,IKEYS,IKEYF,1,NDUM,RDUM,GWMOUT,INTEMP)
          CALL URWORD(LINE,LLOC,INAMES,INAMEF,0,NDUM,RDUM,
     &                  GWMOUT,INTEMP)          
          KEYWORD = LINE(IKEYS:IKEYF)            ! EXTRACT KEYWORD
          FNAME=LINE(INAMES:INAMEF)              ! EXTRACT FILE NAME
          IF(LINE(1:1).EQ.'#')THEN               ! LINE HAS BEEN COMMENTED
            WRITE(GWMOUT,'(1X,A)')LINE
          ELSEIF(KEYWORD.EQ.'DECVAR')THEN        ! CHECK FOR EACH POSSIBLE
            FNAMEOPT(I,1)=FNAME                  ! KEYWORD AND STORE
          ELSEIF(KEYWORD.EQ.'STAVAR')THEN        ! CORRESPONDING FILENAME
            FNAMEOPT(I,2)=FNAME
          ELSEIF(KEYWORD.EQ.'OBJFNC')THEN  
            FNAMEOPT(I,3)=FNAME
          ELSEIF(KEYWORD.EQ.'VARCON')THEN
            FNAMEOPT(I,4)=FNAME
          ELSEIF(KEYWORD.EQ.'SUMCON')THEN
            FNAMEOPT(I,5)=FNAME
          ELSEIF(KEYWORD.EQ.'HEDCON')THEN
            FNAMEOPT(I,6)=FNAME
          ELSEIF(KEYWORD.EQ.'STRMCON')THEN
            FNAMEOPT(I,7)=FNAME
          ELSEIF(KEYWORD.EQ.'SOLN')THEN
            FNAMEOPT(I,8)=FNAME
          ELSEIF(KEYWORD.EQ.' ')THEN
          ELSEIF(KEYWORD.EQ.'DIS'.OR.KEYWORD.EQ.'NAM'
     &                           .OR.KEYWORD.EQ.'CONTROL')THEN
            ! THESE ARE FOR GWM-VI
          ELSE
            WRITE(GWMOUT,3000,ERR=990)KEYWORD    ! INVALID KEYWORD
            CALL GSTOP(' ')
          ENDIF
          GO TO 200                              ! READ NEXT LINE OR EXIT
        ENDIF
  210 ENDDO
C
C-----PROCESS DECISION VARIABLE FILE NAME(S) IN FNAMEOPT LOCATION 1
      CALL GWM1DCV3AR(FNAMEOPT(1:NGRIDS,1),GWMOUT,NPER,NFVAR,NEVAR,
     &                  NBVAR,NDV,NGRIDS)
      IF(NFVAR.LE.0)THEN
        WRITE(GWMOUT,1030,ERR=990)
        CALL GSTOP(' ')
      ENDIF
C   
C-----PROCESS STATE VARIABLE FILE NAME(S) IN FNAMEOPT LOCATION 2
      CALL GWM1STA3AR(FNAMEOPT(1:NGRIDS,2),
     &                GWMOUT,NPER,NGRIDS)
C
C-----PROCESS OBJECTIVE FUNCTION FILE IN FNAMEOPT LOCATION 3
      IF(FNAMEOPT(1,3).NE.' ')THEN               ! OBJFNC IS OK
        CALL GWM1OBJ3AR(FNAMEOPT(1,3),GWMOUT,NPER,REAL(PERLEN,DP),
     &                  NFVAR,NEVAR,NBVAR)
      ELSE                                       ! ERROR, OBJFNC NOT ON PARENT
        IF(NGRIDS.EQ.1)WRITE(GWMOUT,1040,ERR=990)
        IF(NGRIDS.GT.1)WRITE(GWMOUT,1045,ERR=990)
        CALL GSTOP(' ')
      ENDIF
      IF(NGRIDS.GT.1)THEN
        DO 400 I=2,NGRIDS    
          IF(FNAMEOPT(I,3).NE.' ')THEN           ! AN OBJFNC IS ON CHILD
            WRITE(GWMOUT,2500,ERR=990)
            CALL GSTOP(' ')
          ENDIF
  400   ENDDO
      ENDIF
C
C-----INITIALIZE THE NUMBER OF VARIABLES AND CONSTRAINTS
      NV = NDV - 1
      NCON = 0
      NRMC = 0
      HDCNUM = 0
      STCNUM = 0
C
C-----PROCESS DECISION-VARIABLE CONSTRAINT FILE IN FNAMEOPT LOCATION 4
C     CHECK THAT EACH DECVAR FILE HAS A VARCON FILE
      J=0
      DO 500 I=1,NGRIDS
        IF((FNAMEOPT(I,1).NE.' ' .AND. FNAMEOPT(I,4).EQ.' ').OR.
     &     (FNAMEOPT(I,1).EQ.' ' .AND. FNAMEOPT(I,4).NE.' ')) J=1
  500 ENDDO    
      IF(J.GT.0)THEN
        IF(NGRIDS.EQ.1)WRITE(GWMOUT,2000,ERR=990)
        IF(NGRIDS.GT.1)WRITE(GWMOUT,2005,ERR=990)
        CALL GSTOP(' ')
      ENDIF
      CALL GWM1DCC3AR(FNAMEOPT(1:NGRIDS,4),GWMOUT,NFVAR,NEVAR,NBVAR,
     &                NV,NCON,NGRIDS)
C
C-----PROCESS SUMCON FOR PARENT IF KEYWORD PRESENT; ERROR IF KEYWORD IN CHILD
C     FILE(S) IN FNAMEOPT LOCATION 5
      IF(FNAMEOPT(1,5).NE.' ')THEN
        CALL GWM1SMC3AR(FNAMEOPT(1,5),GWMOUT,NFVAR,NEVAR,NBVAR,
     &                  NV,NCON)
      ENDIF
      IF(NGRIDS.GT.1)THEN
        DO 520 I=2,NGRIDS    
          IF(FNAMEOPT(I,5).NE.' ')THEN
            WRITE(GWMOUT,2600,ERR=990)
            CALL GSTOP(' ')
          ENDIF
  520   ENDDO
      ENDIF
C    
C-----PROCESS HEDCON FOR PARENT AND/OR CHILDREN; IN FNAMEOPT LOCATION 6
      CALL GWM1HDC3AR(FNAMEOPT(1:NGRIDS,6),GWMOUT,NPER,NV,NCON,
     &                NRMC,NGRIDS)
C
C-----PROCESS STRMCON FOR PARENT AND/OR CHILD; IN FNAMEOPT LOCATION 7
      CALL GWM1STC3AR(FNAMEOPT(1:NGRIDS,7),GWMOUT,NV,NCON,
     &                NRMC,NGRIDS)
C
C-----PROCESS SOLN FILE FOR PARENT; ERROR IF IN CHILD, DON'T STOP PROGRAM
      IF(FNAMEOPT(1,8).NE.' ')THEN
        CALL GWM1RMS3AR(FNAMEOPT(1,8),GWMOUT,NFVAR,NEVAR,NBVAR,
     &                  NDV,NV,NCON)
      ELSE
        IF(NGRIDS.EQ.1)WRITE(GWMOUT,4000,ERR=990)
        IF(NGRIDS.GT.1)WRITE(GWMOUT,4005,ERR=990)
        CALL GSTOP(' ')
      ENDIF
      IF(NGRIDS.GT.1)THEN
        DO 530 I=2,NGRIDS    
          IF(FNAMEOPT(I,8).NE.' ')THEN
            WRITE(GWMOUT,4100,ERR=990)
          ENDIF
  530   ENDDO
      ENDIF
C-----DONE READING IN INPUT DATA
      DEALLOCATE (FNAMEOPT)
      RETURN
C
 1000 FORMAT (/,34X,'MODFLOW',A,/,
     &4X,'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE',
     &' GROUNDWATER FLOW MODEL',/,17X,'Version ',A/)
 1005 FORMAT (/,34X,'GWM-VI',/,
     &4X,'U.S. GEOLOGICAL SURVEY GROUNDWATER MANAGEMENT',
     &' VERSION INDEPENDENT PROGRAM',/,17X,'Version ',A/)
 1010 FORMAT(/,/,1X,/1X,'OPENING GWM FILE FOR GWM1 -- GROUNDWATER ',
     1  'MANAGEMENT PROCESS',/,' INPUT READ FROM UNIT',I4,/)
 1015 FORMAT(/,/1X,'OPENING GWM FILE FOR GWM1 -- GROUNDWATER ',
     1  'MANAGEMENT PROCESS',/,
     2  ' PARENT GRID INPUT READ FROM UNIT',I4)
 1016 FORMAT(/,1X,'OPENING GWM FILE CHILD GRID - ',
     2  'INPUT READ FROM UNIT',I4)
 1030 FORMAT(1X,/1X,'PROGRAM STOPPED. NO FLOW RATE ',
     1  ' DECISION VARIABLES IN INPUT FILES')
 1040 FORMAT(1X,/1X,'PROGRAM STOPPED. USER MUST SPECIFY AN',
     1  ' OBJECTIVE FUNCTION FILE')
 1045 FORMAT(1X,/1X,'PROGRAM STOPPED. USER MUST SPECIFY AN',
     1  ' OBJECTIVE FUNCTION FILE IN PARENT GRID')
 2000 FORMAT(1X,/1X,/1X,'PROGRAM STOPPED. USER MUST SPECIFY A',
     1  ' DECISION-VARIABLES CONSTRAINT FILE')
 2005 FORMAT(1X,/1X,/1X,'PROGRAM STOPPED. USER MUST SPECIFY A',
     1  ' DECISION-VARIABLES CONSTRAINT FILE',
     2  ' TO ACCOMPANY EACH DECVAR FILE')
 2500 FORMAT(1X,/1X,'PROGRAM STOPPED. A CHILD GWM FILE CANNOT',
     1  ' CONTAIN AN OBJFNC KEYWORD')
 2600 FORMAT(1X,/1X,'PROGRAM STOPPED. A CHILD GWM FILE CANNOT',
     1  ' CONTAIN A SUMCON KEYWORD')
 3000 FORMAT(1X,/1X,'PROGRAM STOPPED. ',A10,' NOT A VALID CONSTRAINT',
     1  /,' OR SOLUTION KEYWORD')
 4000 FORMAT(1X,/1X,'PROGRAM STOPPED. USER MUST SPECIFY A',
     1  ' SOLUTION FILE AS THE LAST FILE IN THE GWM FILE')
 4005 FORMAT(1X,/1X,'PROGRAM STOPPED. USER MUST SPECIFY A',
     1  ' SOLUTION FILE AS THE LAST FILE IN THE GWM PARENT GRID FILE')
 4100 FORMAT(1X,/1X,'SOLUTION FILE LISTED IN A CHILD GWM FILE',
     1  ' WILL BE IGNORED.')

C
C-----ERROR HANDLING
  990 CONTINUE
C-----FILE-WRITING ERROR
      INQUIRE(GWMOUT,NAME=FLNM,FORM=FMTARG,ACCESS=ACCARG,ACTION=FILACT)
      WRITE(*,9900)TRIM(FLNM),IOUT,FMTARG,ACCARG,FILACT
 9900 FORMAT(/,1X,'*** ERROR WRITING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (GWM1BAS3AR)')
      CALL GSTOP(' ')
C
  991 CONTINUE
C-----FILE-READING ERROR
      INQUIRE(INTEMP,NAME=FLNM,FORM=FMTARG,ACCESS=ACCARG,ACTION=FILACT)
      WRITE(*,9910)TRIM(FLNM),INTEMP,FMTARG,ACCARG,FILACT
      WRITE(IOUT,9910)TRIM(FLNM),INTEMP,FMTARG,ACCARG,FILACT
 9910 FORMAT(/,1X,'*** ERROR READING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (GWM1BAS3AR)')
      CALL GSTOP(' ')
C
      END SUBROUTINE GWM1BAS3AR
C
C***********************************************************************
      SUBROUTINE GWM1BAS3RW(INUNIT,FNAME,CUNIT,NIUNIT,IOUT,VERSION,
     &                      MFVNAM,GWMSTRG)
C***********************************************************************
C     
C-----GWM1BAS3RW CREATED FROM PES1BAS6RW VERSION 19990811ERB 
C     ******************************************************************
C     VERSION: 21JAN2010 
C     PURPOSE: REWIND FILES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER(I4B),INTENT(INOUT)::INUNIT
      CHARACTER(LEN=*),INTENT(IN)::FNAME
      INTEGER(I4B),INTENT(IN)::NIUNIT,IOUT
      CHARACTER(LEN=4),INTENT(IN)::CUNIT(NIUNIT)
      CHARACTER(LEN=40),INTENT(IN)::VERSION
      CHARACTER(LEN=10),INTENT(IN)::MFVNAM
      CHARACTER(LEN=200),INTENT(IN)::GWMSTRG
      INTERFACE
C
        SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
        CHARACTER*(*) LINE
        CHARACTER*20 STRING
        CHARACTER*30 RW
        CHARACTER*1 TAB
        END

      END INTERFACE
      CHARACTER(len=40)::SPACES
      CHARACTER(len=200)::LINE
      LOGICAL(LGT)::LOP
      INTEGER(I4B)::LLOC,ITYP1,ITYP2,ISTART,ISTOP,INAM1,INAM2,I 
      INTEGER(I4B)::LENVER,INDENT,IU 
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
C1------OPEN THE NAME FILE.
      FILSTAT='OLD'
      OPEN(UNIT=INUNIT,FILE=FNAME,STATUS=FILSTAT,ACTION=ACTION(1),
     &     ERR=999)
C
C2------READ A LINE; IGNORE BLANK LINES AND COMMENT LINES.
   10 READ(INUNIT,'(A)',END=100) LINE
      IF(LINE.EQ.' ')GOTO 10
      IF(LINE(1:1).EQ.'#')GOTO 10
C
C3------DECODE THE FILE TYPE, UNIT NUMBER, AND NAME.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,NDUM,RDUM,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,RDUM,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,NDUM,RDUM,IOUT,INUNIT)
C
C4------REWIND IF MAJOR OPTION FILE (EVEN IF NOT RE-READ)
      DO 20 I=1,NIUNIT
        IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I))GOTO 30
   20 ENDDO
C
C5------REWIND IF BAS OR NONGLOBAL LIST FILE
      IF(LINE(ITYP1:ITYP2).EQ.'BAS6')GOTO 30
      IF(LINE(ITYP1:ITYP2).EQ.'LIST')GOTO 30
C
C5------NOT A MAJOR OPTION.  REWIND IF FILE TYPE IS DATA.
      IF(LINE(ITYP1:ITYP2).NE.'DATA'         .AND.
     1   LINE(ITYP1:ITYP2).NE.'DATA(BINARY)' )GOTO 10
C
C6------REWIND THE FILE
   30 REWIND(IU)
C
      IF(LINE(ITYP1:ITYP2).EQ.'LIST')THEN        ! WRITE NEW HEADER
        SPACES=' '
        LENVER=LEN_TRIM(VERSION)
        INDENT=40-(LENVER+8)/2
        WRITE(IOUT,1000) MFVNAM,SPACES(1:INDENT),VERSION(1:LENVER)
        WRITE(IOUT,1010) GWMSTRG
      ELSE
        WRITE(IOUT,3000)LINE(INAM1:INAM2),LINE(ITYP1:ITYP2),IU
      ENDIF
      GOTO 10
C
C7------END OF NAME FILE.
  100 CONTINUE
      INQUIRE(UNIT=INUNIT,OPENED=LOP)
      IF(LOP)CLOSE(UNIT=INUNIT)
C
      RETURN
 1000   FORMAT(34X,'MODFLOW',A,/,
     &       6X,'U.S. GEOLOGICAL SURVEY MODULAR',
     &       ' FINITE-DIFFERENCE GROUNDWATER FLOW MODEL',/,
     &       A,'VERSION ',A,/)
 1010   FORMAT(/,5X,A,/)
 3000 FORMAT(1X,/1X,'REWOUND ',A,/
     1       1X,'FILE TYPE:',A,'   UNIT ',I4)
C
C-----ERROR HANDLING
  999 CONTINUE
C-----FILE-OPENING ERROR
      INQUIRE(INUNIT,NAME=FLNM,FORM=FMTARG,ACCESS=ACCARG,ACTION=FILACT)
      WRITE(*,9990)TRIM(FLNM),INUNIT,FILSTAT,FMTARG,ACCARG,FILACT
      WRITE(IOUT,9990)TRIM(FLNM),INUNIT,FILSTAT,FMTARG,ACCARG,FILACT
 9990 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE STATUS: ',A,/
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (GWM1BAS2RW)')
      CALL GSTOP(' ')
C
      END SUBROUTINE GWM1BAS3RW
C
C***********************************************************************
      SUBROUTINE GWM1BAS3RRF(INUNIT,CUNIT,VERSION,IUDIS,IUZON,IUMLT,
     2       IGRID,IUOC,HEADNG,IUPVAL,MFVNAM,FNAMEN,LASTSIM)
C***********************************************************************
C     VERSION: 22MAR2008 CREATED FROM GWF2BAS7AR
C     PURPOSE: RE-READ INFORMATION FROM FLOW PROCESS BAS FILE
C-----------------------------------------------------------------------
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,
     1                     LENUNI,IXSEC,ITRSS,INBAS,IFREFM,NODES,IOUT,
     2                     IUNIT,NIUNIT,HNEW,LBOTM,LAYCBD,LAYHDT,LAYHDS,
     3                     PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,
     4                     HOLD,IBOUND,CR,CC,CV,HCOF,RHS,BUFF,STRT
      USE PARAMMODULE,ONLY:MXPAR,MXCLST,MXINST,ICLSUM,IPSUM,
     1                     INAMLOC,NMLTAR,NZONAR,NPVAL,
     2                     B,IACTIVE,IPLOC,IPCLST,PARNAM,PARTYP,
     3                     ZONNAM,MLTNAM,INAME
      USE GWFBASMODULE,ONLY:MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,
     1                      LBHDSV,LBDDSV,LBBOSV,IBUDFL,ICBCFL,IHDDFL,
     2                      IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,ICHFLG,
     3                      DELT,PERTIM,TOTIM,HNOFLO,CHEDFM,CDDNFM,
     4                      CBOUFM,VBVL,VBNM
      USE GWM1BAS3, ONLY : ZERO
C
      INTEGER(I4B),INTENT(IN)::INUNIT,IUDIS,IUZON,IUMLT,
     &                         IGRID,IUOC,IUPVAL                   
      CHARACTER(LEN=80),INTENT(INOUT)::HEADNG(*) 
      CHARACTER(LEN= 4),INTENT(IN)::CUNIT(NIUNIT)
      CHARACTER(LEN=40),INTENT(IN)::VERSION 
      CHARACTER(LEN=10),INTENT(IN)::MFVNAM 
      CHARACTER(LEN=200),INTENT(IN)::FNAMEN
      LOGICAL,INTENT(IN)::LASTSIM
C
      INTERFACE
        SUBROUTINE U2DINT(IA,ANAME,II,JJ,K,IN,IOUT)
        CHARACTER*24 ANAME
        DIMENSION IA(JJ,II)
        CHARACTER*20 FMTIN
        CHARACTER*200 CNTRL
        CHARACTER*200 FNAME
        END
C
        SUBROUTINE U2DREL(A,ANAME,II,JJ,K,IN,IOUT)
        CHARACTER*24 ANAME
        DIMENSION A(JJ,II)
        CHARACTER*20 FMTIN
        CHARACTER*200 CNTRL
        CHARACTER*16 TEXT
        CHARACTER*200 FNAME
        END

      END INTERFACE
C-----LOCAL VARIABLES
      REAL(DP)::HNF
      INTEGER(I4B)::I,J,K,KK,N,LLOC,ISTART,ISTOP,R
      CHARACTER(LEN=200) LINE
      CHARACTER(LEN=24)::ANAME(2)=(/'          BOUNDARY ARRAY',
     &                             '            INITIAL HEAD'/)
C     ------------------------------------------------------------------
C1------VARIABLES REMAIN ALLOCATED FROM GWF2BAS7AR
C
C2------CHECK THAT ALL FILES ARE STILL OPEN - OPEN IF NOT
      CALL GWM1BAS3OPEN(INUNIT,IOUT,IUNIT,CUNIT,
     1              NIUNIT,INBAS,FNAMEN)
C
C3------PRINT A MESSAGE IDENTIFYING THE BASIC PACKAGE.
      WRITE(IOUT,1)INBAS
    1 FORMAT(1X,/1X,'BAS -- BASIC PACKAGE, VERSION 7, 5/2/2005',
     2' INPUT READ FROM UNIT ',I4)
C
C4------Initialize parameter definition variables.
      IPSUM=0
      ICLSUM=0
      INAMLOC=1
      DO 10 N=1,MXPAR
        PARNAM(N)=' '
        PARTYP(N)=' '
        IPLOC(1,N)=0
        IPLOC(2,N)=0
        IACTIVE(N)=0
   10 CONTINUE
C
C5------Allocate and read discretization data.
      CALL SGWF2BAS7ARDIS(IUDIS,IOUT)
      NODES=NCOL*NROW*NLAY
C      
C6------GLOBAL ARRAYS REMAIN ALLOCATED
C
C7------Initialize head-dependent thickness indicator to code that
C7------indicates layer is undefined.
      DO 100 I=1,NLAY
        LAYHDT(I)=-1
        LAYHDS(I)=-1
  100 CONTINUE
      WRITE(IOUT,'(//)')
C
C8------Read BAS Package file.
C8A-----READ AND PRINT COMMENTS.  SAVE THE FIRST TWO COMMENTS IN HEADNG.
      HEADNG(1)=' '
      HEADNG(2)=' '
      WRITE(IOUT,*)
      READ(INBAS,'(A)') LINE
      IF(LINE(1:1).NE.'#') GO TO 20
      HEADNG(1)=LINE(1:80)
      WRITE(IOUT,'(1X,A)') HEADNG(1)
      READ(INBAS,'(A)') LINE
      IF(LINE(1:1).NE.'#') GO TO 20
      HEADNG(2)=LINE(1:80)
      WRITE(IOUT,'(1X,A)') HEADNG(2)
      CALL URDCOM(INBAS,IOUT,LINE)
C
C8B-----LOOK FOR OPTIONS IN THE FIRST ITEM AFTER THE HEADING.
   20 IXSEC=0
      ICHFLG=0
      IFREFM=0
      IPRTIM=0
      LLOC=1
   25 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INBAS)
      IF(LINE(ISTART:ISTOP).EQ.'XSECTION') THEN
         IXSEC=1
      ELSE IF(LINE(ISTART:ISTOP).EQ.'CHTOCH') THEN
         ICHFLG=1
      ELSE IF(LINE(ISTART:ISTOP).EQ.'FREE') THEN
         IFREFM=1
         WRITE(IOUT,26)
   26    FORMAT (1X,'THE FREE FORMAT OPTION HAS BEEN SELECTED')
      ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTTIME') THEN
         IPRTIM=1
         WRITE(IOUT,7)
    7    FORMAT(1X,'THE PRINTTIME OPTION HAS BEEN SELECTED')
      END IF
      IF(LLOC.LT.200) GO TO 25
C
C8C-----PRINT A MESSAGE SHOWING OPTIONS.
      IF(IXSEC.NE.0) WRITE(IOUT,61)
   61 FORMAT(1X,'CROSS SECTION OPTION IS SPECIFIED')
      IF(ICHFLG.NE.0) WRITE(IOUT,62)
   62 FORMAT(1X,'CALCULATE FLOW BETWEEN ADJACENT CONSTANT-HEAD CELLS')
C
C8D-----INITIALIZE TOTAL ELAPSED TIME COUNTER STORAGE ARRAY COUNTER
C8D-----AND CALCULATE NUMBER OF CELLS.
      TOTIM=0.
C      
C8E-----READ BOUNDARY ARRAY(IBOUND).
      IF(IXSEC.EQ.0) THEN
        DO 280 K=1,NLAY
          KK=K
          CALL U2DINT(IBOUND(:,:,KK),ANAME(1),NROW,NCOL,KK,INBAS,IOUT)
  280   CONTINUE
      ELSE
        CALL U2DINT(IBOUND(:,:,1),ANAME(1),NLAY,NCOL,-1,INBAS,IOUT)
      END IF
C
C8F-----READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
      IF(IFREFM.EQ.0) THEN
        READ(INBAS,'(F10.0)') HNOFLO
      ELSE
        READ(INBAS,*) HNOFLO
      END IF
      HNF=HNOFLO
      WRITE(IOUT,1000) HNOFLO
C
      IF(ITRSS.EQ.0 .AND. .NOT.LASTSIM)THEN      ! STEADY STATE SIMULATION
C8G-----DON'T READ STRT - USE HNEW DIRECTLY AS INITIAL SOLUTION ESTIMATE
C9------DON'T RESET HNEW
        WRITE(IOUT,2000,ERR=990) 
C
      ELSE                                       ! NON-STEADY STATE SIMULATION
C-------FOR NON-STEADY FLOW PROCESS READ AND INITIALIZE BASIC MODEL ARRAYS
C
C8G-----READ INITIAL HEADS.
        IF(IXSEC.EQ.0) THEN
           DO 300 K=1,NLAY
             KK=K
             CALL U2DREL(STRT(:,:,KK),ANAME(2),NROW,NCOL,KK,INBAS,IOUT)
  300      CONTINUE
        ELSE
          CALL U2DREL(STRT(:,:,1),ANAME(2),NLAY,NCOL,-1,INBAS,IOUT)
        END IF
C
C9------COPY INITIAL HEADS FROM STRT TO HNEW.
        DO 400 K=1,NLAY
        DO 400 I=1,NROW
        DO 400 J=1,NCOL
          HNEW(J,I,K)=STRT(J,I,K)
          IF(IBOUND(J,I,K).EQ.0) HNEW(J,I,K)=HNF
  400   CONTINUE
      ENDIF  
C
C10-----SET UP OUTPUT CONTROL.
      CALL SGWF2BAS7I(NLAY,IUNIT(IUOC),IOUT,IFREFM,NIUNIT)
C
C11-----INITIALIZE VOLUMETRIC BUDGET ACCUMULATORS TO ZERO.
      DO 600 I=1,NIUNIT
      DO 600 J=1,4
      VBVL(J,I)=ZERO
  600 CONTINUE
C
C12-----Allocate and read Zone and Multiplier arrays
      CALL SGWF2BAS7ARMZ(IUNIT(IUZON),IUNIT(IUMLT))
C
C13-----READ PARAMETER VALUES FILE.
      CALL SGWF2BAS7ARPVAL(IUPVAL)
C
C14-----SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2BAS7PSV(IGRID)
      RETURN
 1000   FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',1PG11.5,
     1         ' AT ALL NO-FLOW NODES (IBOUND=0).')
 2000   FORMAT(1X,/1X,'INITIAL HEAD WILL BE SET TO ',
     1         'THE SOLUTION FROM THE PRIOR FLOW-PROCESS SIMULATION')
C-----ERROR HANDLING
  990 CONTINUE
C-----FILE-WRITING ERROR
      INQUIRE(IOUT,NAME=FLNM,FORM=FMTARG,ACCESS=ACCARG,ACTION=FILACT)
      WRITE(*,9900)TRIM(FLNM),IOUT,FMTARG,ACCARG,FILACT
 9900 FORMAT(/,1X,'*** ERROR WRITING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (GWM1BAS2RRF)')
      CALL GSTOP(' ')
C
      END SUBROUTINE GWM1BAS3RRF
C
C***********************************************************************
      SUBROUTINE GWM1BAS3DABAS7(IGRID)
C***********************************************************************
C     VERSION: 22MAR2008 CREATED FROM GWF2BAS7DA
C     PURPOSE: DEALLOCATE SELECTED GLOBAL DATA
C-----------------------------------------------------------------------
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
      INTEGER(I4B),INTENT(IN)::IGRID
C      
C-------ITEMS THAT WILL BE REALLOCATED IN SGWF2BAS7ARDIS
        DEALLOCATE(GLOBALDAT(IGRID)%DELR)
        DEALLOCATE(GLOBALDAT(IGRID)%DELC)
        DEALLOCATE(GLOBALDAT(IGRID)%BOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYCBD)
        DEALLOCATE(GLOBALDAT(IGRID)%LBOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%PERLEN)
        DEALLOCATE(GLOBALDAT(IGRID)%NSTP)
        DEALLOCATE(GLOBALDAT(IGRID)%TSMULT)
        DEALLOCATE(GLOBALDAT(IGRID)%ISSFLG)
C-------ITEMS THAT WILL BE REALLOCATED IN SGWF2BAS7ARMZ
        DEALLOCATE (PARAMDAT(IGRID)%ZONNAM)
        DEALLOCATE (PARAMDAT(IGRID)%IZON)
C-------ITEMS THAT WILL BE REALLOCATED IN SGWF2BAS7I
        DEALLOCATE(GWFBASDAT(IGRID)%IOFLG)
        DEALLOCATE(GWFBASDAT(IGRID)%VBVL)
        DEALLOCATE(GWFBASDAT(IGRID)%VBNM)
C
      RETURN
      END SUBROUTINE GWM1BAS3DABAS7
C
C***********************************************************************
      SUBROUTINE GWM1BAS3OPEN(INUNIT,IOUT,IUNIT,CUNIT,
     1              NIUNIT,INBAS,FNAMEN)
C***********************************************************************
C     VERSION: 22MAR2008 CREATED FROM SGWF2BAS7OPEN
C     PURPOSE: OPEN ANY FILES THAT MAY HAVE BEEN CLOSED
C-----------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'openspec.inc'
      INTEGER(I4B),INTENT(IN)::INUNIT,NIUNIT,IOUT,INBAS
      INTEGER(I4B),INTENT(IN)::IUNIT(NIUNIT)
      CHARACTER*4, INTENT(IN)::CUNIT(NIUNIT)
      CHARACTER*200,INTENT(IN)::FNAMEN
C     LOCAL VARIABLES
      CHARACTER*7 FILSTAT
      CHARACTER*20 FILACT, FMTARG, ACCARG
      CHARACTER*300 LINE, FNAME 
      CHARACTER*20 FILTYP
      LOGICAL LOP
      INTEGER(I4B)::I,ISTOP,ISTART,LLOC,ITYP1,ITYP2
      INTEGER(I4B)::N,IU,INAM1,INAM2,IOPT1,IOPT2,IFLEN
      REAL(DP)::R
C     ---------------------------------------------------------------
C
C1------RE-OPEN NAME FILE.
      OPEN (UNIT=INUNIT,FILE=FNAMEN,STATUS='OLD',ACTION=ACTION(1))
C
C2------READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
10    READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') GO TO 10
C
C3------DECODE THE FILE TYPE, UNIT NUMBER, AND NAME.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      FILTYP=LINE(ITYP1:ITYP2)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INUNIT)
      IFLEN=INAM2-INAM1+1
      FNAME(1:IFLEN)=LINE(INAM1:INAM2)
      INQUIRE(UNIT=IU,OPENED=LOP)
      IF(LOP) GO TO 10   ! ALREADY OPEN
C
C5------SET DEFAULT FILE ATTRIBUTES.
      FMTARG='FORMATTED'
      ACCARG='SEQUENTIAL'
      FILSTAT='UNKNOWN'
      FILACT=' '
C
C9------CHECK FOR "UNFORMATTED" FILE TYPE.
      IF(FILTYP.EQ.'DATA(BINARY)' .OR.
     1        FILTYP.EQ.'DATAGLO(BINARY)') THEN
         FMTARG=FORM
         ACCARG=ACCESS
C
C10-----CHECK FOR "FORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA' .OR.
     1        LINE(ITYP1:ITYP2).EQ.'DATAGLO') THEN
         FMTARG='FORMATTED'
         ACCARG='SEQUENTIAL'
C
C11-----CHECK FOR MAJOR OPTIONS.
      ELSE
        DO 20 I=1,NIUNIT
           IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I)) THEN
              FILSTAT='OLD    '
              FILACT=ACTION(1)
              GO TO 30
           END IF
20      CONTINUE
30      CONTINUE
      END IF
C
C12-----FOR DATA FILES, CHECK FOR "REPLACE" OR "OLD" OPTION
      IF (FILSTAT.EQ.'UNKNOWN') THEN
        CALL URWORD(LINE,LLOC,IOPT1,IOPT2,1,N,R,IOUT,INUNIT)
        IF (LINE(IOPT1:IOPT2).EQ.'REPLACE' .OR.
     &      LINE(IOPT1:IOPT2).EQ.'OLD')
     &      FILSTAT = LINE(IOPT1:IOPT2)
      ENDIF
      IF (FILACT.EQ.' ') FILACT=ACTION(2)
C
C13-----OPEN THE FILE NAME 
      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),FORM=FMTARG,
     1      ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,ERR=2000)
      GO TO 10
C
C14-----END OF NAME FILE.  
1000  CLOSE (UNIT=INUNIT)
C
      RETURN
C
C15-----ERROR OPENING FILE.
 2000 CONTINUE
      WRITE(*,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
      WRITE(IOUT,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
 2010 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE STATUS: ',A,/
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (SGWF2BAS7OPEN)')
      CALL GSTOP(' ')
C
      END SUBROUTINE GWM1BAS3OPEN
C
C***********************************************************************
      SUBROUTINE GWM1BAS3AP(LOC,ILGR,ICNVG,LGRCNVG,IPERT,IGRID)
C***********************************************************************
C     VERSION: 31AUGUST2009
C     PURPOSE: DETERMINE IF THE GWF PROCESS SOLUTION AT THIS
C              STEP MEETS THE CRITERIA FOR ACCEPTABILITY FOR GWM
C
C     OUTPUT:  MFCNVRG WHOSE STATUS INDICATES ACCEPTABILITY OF SOLUTION
C-----------------------------------------------------------------------
      USE GWM1BAS3, ONLY : GWM1BAS3PS,GWM1BAS3PF
      USE GWM1RMS3, ONLY : CRITMFC,MFCNVRG
      INTEGER(I4B),INTENT(IN)::LOC,ILGR,ICNVG,LGRCNVG,IPERT,IGRID
      REAL(DP)::PDIFFR
C     ------------------------------------------------------------------
C
C-----CASE WHEN ALL FLOW PROCESS RESULTS ARE CONSIDERED ACCEPTABLE
      IF(CRITMFC.LT.0)THEN
        MFCNVRG(IPERT) = .TRUE.                  ! ACCEPT SOLUTION REGARDLESS
        IF(LOC.EQ.2 .AND. IGRID.EQ.1)CALL GWM1BAS3PF(
     &  '       Flow Process Accepted Despite Convergence Failure',0,0.)
        RETURN
      ENDIF
C
C-----FIRST CALL TO GWM1BAS3AP, FLOW BALANCE INFORMATION NOT YET AVAILABLE
C     SO ONLY TEST IF GWF CONVERGENCE CRITERIA MET
      IF(LOC.EQ.1)THEN 
        IF(CRITMFC.GT.0.0)THEN
           MFCNVRG(IPERT) = .TRUE.               ! ACCEPT SOLUTION FOR NOW
        ELSEIF(CRITMFC.EQ.0.0)THEN               ! USE GWF CRITERIA
          IF(ILGR.EQ.0)THEN                      ! SINGLE GRID PROBLEM
            IF(ICNVG.EQ.1)THEN                   ! GWF CONVERGENCE IS MET FOR 
              MFCNVRG(IPERT) = .TRUE.            ! THIS TIME STEP
            ELSEIF(ICNVG.EQ.0)THEN               ! GWF CONVERGENCE NOT MET 
              MFCNVRG(IPERT) = .FALSE.    
              CALL GWM1BAS3PS(
     &        '        Flow Process Failed to Converge',0)
            ENDIF
          ELSEIF(ILGR.NE.0)THEN                  ! MULTI GRID PROBLEM
            IF(LGRCNVG.EQ.1)THEN                 ! LGR TESTS ARE MET FOR 
              MFCNVRG(IPERT) = .TRUE.            ! THIS TIME STEP
            ELSEIF(LGRCNVG.NE.1)THEN             ! LGR TESTS NOT MET THIS STEP
              MFCNVRG(IPERT) = .FALSE.                    
              CALL GWM1BAS3PS(
     &        '        Flow Process Failed to Converge',0)
            ENDIF
          ENDIF 
        ENDIF
        RETURN
      ENDIF
C
C-----THIS IS FOR THE SECOND CALL TO GWM1BAS3AP
C     GWF CONVERGENCE CRITERIA MUST HAVE BEEN SATISFIED IN FIRST CALL TO GWM1BAS3AP 
C     HERE, JUST TEST FOR FLOW BALANCE CRITERIA
      IF(ILGR.EQ.0)THEN                          ! SINGLE GRID PROBLEM
        IF(ICNVG.EQ.1)THEN                       ! GWF CONVERGENCE MET FOR 
          MFCNVRG(IPERT) = .TRUE.                ! THIS TIME STEP
        ELSEIF(ICNVG.EQ.0)THEN                   ! GWF CONVERGENCE NOT MET 
          IF(CRITMFC.EQ.0)THEN                   ! THIS IS THE CRITERIA
            MFCNVRG(IPERT) = .FALSE.                    
            CALL GWM1BAS3PS('        Flow Process Failed to Converge',0)
          ELSEIF(CRITMFC.GT.0)THEN               ! ACCEPT IF % DISCREPANCY
            CALL SGWM1BAS3AP(PDIFFR,IGRID)       ! IS MET
            IF(PDIFFR.GT.CRITMFC)THEN            ! % NOT MET FOR       
              MFCNVRG(IPERT) = .FALSE.           ! THIS TIME STEP
              CALL GWM1BAS3PS(
     &        '        Flow Process Water Balance is Not Acceptable',0)
            ELSEIF(PDIFFR.LE.CRITMFC)THEN        ! % IS MET FOR 
              MFCNVRG(IPERT) = .TRUE.            ! THIS TIME STEP
              CALL GWM1BAS3PF(
     &  '      Convergence Failed but Budget% < CRITMFC and =',1,PDIFFR)
            ENDIF
          ENDIF
        ENDIF 
      
      ELSEIF(ILGR.NE.0)THEN                      ! MULTI GRID PROBLEM
        IF(LGRCNVG.EQ.1)THEN                     ! LGR TESTS ARE MET FOR 
          MFCNVRG(IPERT) = .TRUE.                ! THIS TIME STEP
        ELSEIF(LGRCNVG.NE.1)THEN                 ! LGR TESTS NOT MET THIS STEP
          IF(CRITMFC.EQ.0)THEN                   ! THIS IS THE CRITERIA
            MFCNVRG(IPERT) = .FALSE.                    
            CALL GWM1BAS3PS('        Flow Process Failed to Converge',0)
          ELSEIF(CRITMFC.GT.0)THEN               ! ACCEPT IF % DISCREPANCY
            CALL SGWM1BAS3AP(PDIFFR,IGRID)       ! IS MET
            IF(PDIFFR.GT.CRITMFC)THEN            ! % NOT MET FOR       
              MFCNVRG(IPERT) = .FALSE.           ! THIS TIME STEP
              CALL GWM1BAS3PS(
     &        '        Flow Process Water Balance is Not Acceptable',0)
            ELSEIF(PDIFFR.LE.CRITMFC)THEN        ! % IS MET FOR 
              MFCNVRG(IPERT) = .TRUE.            ! THIS TIME STEP
              CALL GWM1BAS3PF(
     &  '      Convergence Failed but Budget% < CRITMFC and =',1,PDIFFR)
            ENDIF
          ENDIF
        ENDIF 
      ENDIF
C
      RETURN
C      
      END SUBROUTINE GWM1BAS3AP
C
C***********************************************************************
      SUBROUTINE SGWM1BAS3AP(PDIFFR,IGRID)
C***********************************************************************
C     PURPOSE: COMPUTE THE PERCENT DISCREPANCY IN FLOW RATE 
C     DERIVED FROM SUBROUTINE SGWF2BAS7V
C-----------------------------------------------------------------------
      USE GWFBASMODULE, ONLY : MSUM,VBVL
      USE GWM1BAS3,     ONLY : ZERO
      REAL(DP),INTENT(OUT)::PDIFFR
      INTEGER(I4B),INTENT(IN)::IGRID
      REAL(DP)::TOTRIN,TOTROT,DIFFR,ADIFFR,AVGRAT
      INTEGER(I4B)::L
C
      PDIFFR = ZERO
      CALL SGWF2BAS7PNT(IGRID)                 ! POINT TO GRID
C
C-----CLEAR RATE ACCUMULATORS.
      TOTRIN=ZERO
      TOTROT=ZERO
C
C-----ADD RATES (IN AND OUT) TO ACCUMULATORS.
      DO 100 L=1,MSUM-1
        TOTRIN=TOTRIN+VBVL(3,L)
        TOTROT=TOTROT+VBVL(4,L)
  100 CONTINUE
C
C-----CALCULATE DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      DIFFR=TOTRIN-TOTROT
      ADIFFR=ABS(DIFFR)
C
C-----CALCULATE PERCENT DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      PDIFFR = ZERO
      AVGRAT=(TOTRIN+TOTROT)/2.0
      IF(AVGRAT.NE.ZERO) PDIFFR=100.*ADIFFR/AVGRAT
C
      RETURN
C
      END SUBROUTINE SGWM1BAS3AP
C      
      END MODULE GWM1BAS3SUBS
