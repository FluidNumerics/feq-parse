! FEQParse.F03
! 
! Copyright 2020 Fluid Numerics LLC
! All rights reserved.
!
! Author : Joe Schoonover ( joe@fluidnumerics.com )
!
! EquationParser defines a public class that can be used to parse and evaluate strings
! representative of equations. An equation, written in infix form, is converted to
! postfix form and evaluated using a postfix calculator.
!
! //////////////////////////////////////////////////////////////////////////////////////////////// !

MODULE FEQParse_TokenStack

USE ISO_FORTRAN_ENV

IMPLICIT NONE

INTEGER, PARAMETER :: Token_Length = 48

  TYPE Token  
    CHARACTER(Token_Length) :: tokenString
    INTEGER                 :: tokenType

    CONTAINS  
      PROCEDURE :: Equals_Token

  END TYPE Token


  TYPE TokenStack
    TYPE(Token), ALLOCATABLE :: tokens(:)
    INTEGER                  :: top_index = 0

    CONTAINS
  
      PROCEDURE :: Construct => Construct_TokenStack
      PROCEDURE :: Destruct  => Destruct_TokenStack
      
      PROCEDURE :: Push      => Push_TokenStack
      PROCEDURE :: Pop       => Pop_TokenStack

      PROCEDURE :: IsEmpty   => IsEmpty_TokenStack
      PROCEDURE :: TopToken
  
  END TYPE TokenStack


CONTAINS


  SUBROUTINE Construct_TokenStack( stack, N )
   CLASS(TokenStack), INTENT(out) :: stack
   INTEGER, INTENT(in)            :: N

     ALLOCATE( stack % tokens(1:N) )
     stack % top_index = 0

  END SUBROUTINE Construct_TokenStack

  SUBROUTINE Destruct_TokenStack( stack )
    CLASS(TokenStack), INTENT(inout) :: stack

      IF( ALLOCATED( stack % tokens ) ) DEALLOCATE( stack % tokens )
      stack % top_index = 0

  END SUBROUTINE Destruct_TokenStack

  SUBROUTINE Push_TokenStack( stack, tok ) 
    CLASS(TokenStack), INTENT(inout) :: stack
    TYPE(Token), INTENT(in)         :: tok

      stack % top_index                  = stack % top_index + 1
      stack % tokens(stack % top_index)  % tokenString = tok % tokenString
      stack % tokens(stack % top_index)  % tokenType   = tok % tokenType
 
  END SUBROUTINE Push_TokenStack

  SUBROUTINE Pop_TokenStack( stack, tok ) 
    CLASS(TokenStack), INTENT(inout) :: stack
    TYPE(Token), INTENT(out)        :: tok
    
      IF( stack % top_index <= 0 ) THEN
        PRINT *, "Attempt to pop from empty token stack"
      ELSE 
        tok % tokenString         = stack % tokens( stack % top_index ) % tokenString
        tok % tokenType           = stack % tokens( stack % top_index ) % tokenType
        stack % top_index = stack % top_index - 1
      END IF


  END SUBROUTINE Pop_TokenStack

  LOGICAL FUNCTION IsEmpty_TokenStack( stack )
    CLASS( TokenStack ) :: stack

      IsEmpty_TokenStack = .FALSE.

      IF( stack % top_index <= 0 )THEN
        IsEmpty_TokenStack = .TRUE.
      ENDIF

  END FUNCTION IsEmpty_TokenStack

  TYPE( Token ) FUNCTION TopToken( stack )
    CLASS( TokenStack ) :: stack

      IF( stack % top_index > 0 )THEN
        TopToken % tokenString = stack % tokens( stack % top_index ) % tokenString
        TopToken % tokenType   = stack % tokens( stack % top_index ) % tokenType
      ELSE
        TopToken % tokenString = ''
      ENDIF

  END FUNCTION TopToken 

  FUNCTION Equals_Token( tok1 ) RESULT( tok2 )
    CLASS(Token) :: tok1
    TYPE(Token)  :: tok2

      tok2 % tokenString = tok1 % tokenString 
      tok2 % tokenType   = tok1 % tokenType

  END FUNCTION Equals_Token

END MODULE FEQParse_TokenStack
