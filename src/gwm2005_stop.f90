MODULE GWM_STOP
CONTAINS
  !-----------------------------------------------------------------------------
  SUBROUTINE GSTOP(STOPMESS,IOUT)
    ! A fatal error has occurred during execution. Terminate the run.
    ! Version called from GWM-2005
    USE GWM_SUBS, ONLY: CLEAN_UP
    IMPLICIT NONE
    ! Arguments
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: STOPMESS
    INTEGER, OPTIONAL, INTENT(IN) :: IOUT
    10 FORMAT(/,1X,A)
    !
    ! Check to see if it necessary to restore original MNW2 input file
    CALL CLEAN_UP()
    !
    IF (PRESENT(STOPMESS)) THEN
      IF (STOPMESS .NE. ' ') THEN
        IF (PRESENT(IOUT)) THEN
          WRITE(IOUT,10) STOPMESS
        ENDIF
        WRITE(*,10) STOPMESS
      ENDIF
    ENDIF
    STOP
    !
  END SUBROUTINE GSTOP
  !-----------------------------------------------------------------------------
END MODULE GWM_STOP