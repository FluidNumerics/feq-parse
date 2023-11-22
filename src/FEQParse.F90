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

MODULE FEQParse

USE ISO_FORTRAN_ENV
USE FEQParse_Functions
USE FEQParse_TokenStack
USE FEQParse_FloatStacks

IMPLICIT NONE

  REAL(real64), PARAMETER :: pi   = 4.0_real64*atan(1.0_real64)

  INTEGER, PARAMETER, PRIVATE :: Error_Message_Length = 256
  INTEGER, PARAMETER, PRIVATE :: Max_Equation_Length  = 1024 
  INTEGER, PARAMETER, PRIVATE :: Max_Function_Length  = 6
  INTEGER, PARAMETER, PRIVATE :: Max_Variable_Length  = 12 
  INTEGER, PARAMETER, PRIVATE :: Stack_Length         = 128

  ! Token types 
  INTEGER, PARAMETER, PRIVATE :: None_Token               = 0
  INTEGER, PARAMETER, PRIVATE :: Number_Token             = 1
  INTEGER, PARAMETER, PRIVATE :: Variable_Token           = 2
  INTEGER, PARAMETER, PRIVATE :: Operator_Token           = 3
  INTEGER, PARAMETER, PRIVATE :: Function_Token           = 4
  INTEGER, PARAMETER, PRIVATE :: OpeningParentheses_Token = 5
  INTEGER, PARAMETER, PRIVATE :: ClosingParentheses_Token = 6
  INTEGER, PARAMETER, PRIVATE :: Monadic_Token            = 7

  INTEGER, PARAMETER, PRIVATE :: nSeparators = 7

  CHARACTER(1), DIMENSION(7), PRIVATE  :: separators = (/ "+", "-", "*", "/", "(", ")", "^" /) 
  CHARACTER(1), DIMENSION(5), PRIVATE  :: operators  = (/ "+", "-", "*", "/", "^" /) 
  CHARACTER(1), DIMENSION(10), PRIVATE :: numbers    = (/ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" /)
  ! Private Types !

 
  PRIVATE :: IsNumber, IsVariable, IsOperator, IsSeparator


  TYPE EquationParser
    CHARACTER(Max_Equation_Length)     :: equation
    CHARACTER(Max_Variable_Length)     :: variableName
    CHARACTER(Max_Equation_Length)     :: inFixFormula
    INTEGER                            :: nIndepVars
    CHARACTER(LEN=1), ALLOCATABLE      :: indepVars(:) 
    TYPE( TokenStack )                 :: inFix
    TYPE( TokenStack )                 :: postFix
    TYPE( FEQParse_FunctionHandler )   :: func

    CONTAINS

      PROCEDURE :: Destruct => Destruct_EquationParser
      PROCEDURE :: CleanEquation
      PROCEDURE :: Tokenize
      PROCEDURE :: ConvertToPostfix

      GENERIC :: Evaluate => Evaluate_sfp32, Evaluate_sfp64
      PROCEDURE, PRIVATE :: Evaluate_sfp32, Evaluate_sfp64

      PROCEDURE :: Print_InFixTokens
      PROCEDURE :: Print_PostFixTokens

      procedure, private :: Priority

  END TYPE EquationParser

  INTERFACE EquationParser
    PROCEDURE Construct_EquationParser
  END INTERFACE EquationParser
 

CONTAINS

   FUNCTION Construct_EquationParser( equation, indepVars ) RESULT( parser )
    TYPE( EquationParser ) :: parser
    CHARACTER(*)           :: equation
    CHARACTER(1)           :: indepVars(1:)    
    ! Local
    INTEGER :: i
    CHARACTER(Error_Message_Length) :: errorMsg
    LOGICAL                         :: equationIsClean, tokenized, success
    INTEGER                         :: nIndepVars

      parser % func = FEQParse_FunctionHandler( )

      nIndepVars = SIZE(indepVars)
      ALLOCATE( parser % indepVars(1:nIndepVars) )
      parser % nIndepVars = nIndepVars
      DO i = 1, nIndepVars
        parser % indepVars(i) = indepVars(i)
      ENDDO

      parser % inFixFormula = " "
      parser % equation = equation
      errorMsg = " "

      CALL parser % CleanEquation( equationIsClean, errorMsg )

      IF( equationIsClean )THEN

        CALL parser % Tokenize( tokenized, errorMsg )

        IF( tokenized )THEN

          CALL parser % ConvertToPostFix( )
        
        ELSE

           PRINT*, TRIM( errorMsg )
           success = .false.

        ENDIF

      END IF
         
  END FUNCTION Construct_EquationParser

  SUBROUTINE Destruct_EquationParser( parser )
    IMPLICIT NONE
    CLASS(EquationParser), INTENT(inout) :: parser

      DEALLOCATE( parser % indepVars )
      parser % inFixFormula = ""
      parser % equation = ""

      CALL parser % inFix % Destruct()
      CALL parser % postFix % Destruct()
      call parser % func % Destruct()

  END SUBROUTINE Destruct_EquationParser

  SUBROUTINE CleanEquation( parser, equationCleaned, errorMsg )
    CLASS( EquationParser ), INTENT(inout)       :: parser
    LOGICAL, INTENT(out)                         :: equationCleaned
    CHARACTER(Error_Message_Length), INTENT(out) :: errorMsg
    ! Local
    INTEGER :: nChar, equalSignLoc, j, i
   

      equationCleaned = .FALSE.
      parser % variableName    = '#noname'

      nChar = LEN_TRIM( parser % equation )
      equalSignLoc = INDEX( parser % equation, "=" )

      IF( equalSignLoc == 0 )THEN
        errorMsg = "No equal sign found"
        RETURN
      ENDIF

      parser % variableName = TRIM( parser % equation(1:equalSignLoc-1) )

      ! Grab the formula to the right of the equal sign and left adjust the formula
      parser % inFixFormula = parser % equation(equalSignLoc+1:)
      parser % inFixFormula = ADJUSTL(parser % inFixFormula)
  

      ! Remove any spaces
      j = 1
      DO i = 1, LEN_TRIM(parser % inFixFormula)
        IF( parser % inFixFormula(i:i) /= " " )THEN
          parser % inFixFormula(j:j) = parser % inFixFormula(i:i)
          j = j + 1
        ENDIF
      ENDDO

      parser % inFixFormula(j:Max_Equation_Length) = " "
  
      equationCleaned = .TRUE.

  END SUBROUTINE CleanEquation

  SUBROUTINE Tokenize( parser, tokenized, errorMsg )
    CLASS( EquationParser ), INTENT(inout) :: parser
    LOGICAL, INTENT(out)                   :: tokenized
    CHARACTER(Error_Message_Length)        :: errorMsg
    ! Local
    INTEGER :: i, j 
 

      tokenized = .FALSE.
      errorMsg  = " "

      CALL parser % infix % Construct( Stack_Length )

      i = 1
      DO WHILE( parser % inFixFormula(i:i) /= " " )

        IF( IsVariable( parser % inFixFormula(i:i), parser % indepVars, parser % nIndepVars ) )THEN
          
          parser % inFix % top_index = parser % inFix % top_index + 1
          parser % inFix % tokens( parser % inFix % top_index ) % tokenString = parser % inFixFormula(i:i)
          parser % inFix % tokens( parser % inFix % top_index ) % tokenType   = Variable_Token 
          i = i+1

          ! Next item must be an operator, closing parentheses, or end of equation

          IF( .NOT. IsOperator( parser % infixFormula(i:i) ) .AND. &
              parser % inFixFormula(i:i) /= ")" .AND. parser % inFixFormula(i:i) /= " "  )THEN

            errorMsg = "Missing operator or closing parentheses after token : "//&
                       TRIM( parser % inFix % tokens( parser % inFix % top_index ) % tokenString )
            RETURN

          ENDIF

        ELSEIF( IsNumber( parser % inFixFormula(i:i) ) )THEN

          parser % inFix % top_index = parser % inFix % top_index + 1
          parser % inFix % tokens( parser % inFix % top_index ) % tokenString = ''


          IF( parser % inFixFormula(i:i) == 'p' .OR. parser % inFixFormula(i:i) == 'P' )THEN

            ! Conditional for using built in "pi" definition
            parser % inFix % tokens( parser % inFix % top_index ) % tokenString(1:2) = parser % inFixFormula(i:i+1)
            j = 2

          ELSE

            j = 0
            DO WHILE( IsNumber( parser % inFixFormula(i+j:i+j) ) )

              parser % inFix % tokens( parser % inFix % top_index ) % tokenString(j+1:j+1) = parser % inFixFormula(i+j:i+j) 
              j = j+1

            ENDDO

          ENDIF

          parser % inFix % tokens( parser % inFix % top_index ) % tokenType = Number_Token
        
          i = i + j

          ! Next item must be an operator or a closing parentheses
          IF( .NOT. IsOperator( parser % infixFormula(i:i) ) .AND. &
              parser % inFixFormula(i:i) /= ")" .AND. parser % inFixFormula(i:i) /= " " )THEN

            errorMsg = "Missing operator or closing parentheses after token : "//&
                       TRIM( parser % inFix % tokens( parser % inFix % top_index ) % tokenString )
            RETURN

          ENDIF

        ELSEIF( IsSeparator( parser % inFixFormula(i:i) ) )THEN


          parser % inFix % top_index = parser % inFix % top_index + 1 
          parser % inFix % tokens( parser % inFix % top_index ) % tokenString = parser % inFixFormula(i:i)

          IF( parser % inFixFormula(i:i) == "(" )THEN
            parser % inFix % tokens( parser % inFix % top_index ) % tokenType   = OpeningParentheses_Token 
          ELSEIF( parser % inFixFormula(i:i) == ")" )THEN
            parser % inFix % tokens( parser % inFix % top_index ) % tokenType   = ClosingParentheses_Token 
          ELSE
            parser % inFix % tokens( parser % inFix % top_index ) % tokenType   = Operator_Token 
          ENDIF

          i = i + 1


        ELSEIF( parser % func % IsFunction( parser % inFixFormula(i:i) ) )THEN
          
          parser % inFix % top_index = parser % inFix % top_index + 1
          parser % inFix % tokens( parser % inFix % top_index ) % tokenString = ''

          j = FindLastFunctionIndex( parser % inFixFormula(i:i+Max_Function_Length-1) )

          parser % inFix % tokens( parser % inFix % top_index ) % tokenString = parser % inFixFormula(i:i+j)
          parser % inFix % tokens( parser % inFix % top_index ) % tokenType   = Function_Token 
          i = i+j+1

          ! Check to see if the next string
          IF( parser % inFixFormula(i:i) /= "(" )THEN
            errorMsg = "Missing opening parentheses after token : "//&
                       TRIM( parser % inFix % tokens( parser % inFix % top_index ) % tokenString )

            RETURN
          ENDIF


        ELSE

          errorMsg = "Invalid Token : "//&
                     TRIM( parser % inFixFormula(i:i) )

          RETURN

        ENDIF

      ENDDO

      
      IF( parser % inFix % tokens(1) % tokenType == Operator_Token )THEN
         IF( TRIM( parser % inFix % tokens(1) % tokenString ) == "+" .OR. &
              TRIM( parser % inFix % tokens(1) % tokenString ) == "-" ) THEN
            parser % inFix % tokens(1) % tokenType = Monadic_Token
         END IF
      END IF
      
      DO i = 2, parser % inFix % top_index
         IF( parser % inFix % tokens(i) % tokenType == Operator_Token .AND. &
              parser % inFix % tokens(i-1) % tokenType == OpeningParentheses_Token ) THEN
            parser % inFix % tokens(i) % tokenType = Monadic_Token
         END IF
      END DO

      
      tokenized = .TRUE.

  END SUBROUTINE Tokenize


  SUBROUTINE ConvertToPostFix( parser )
    CLASS( EquationParser ), INTENT(inout) :: parser
    ! Local
    CHARACTER(Error_Message_Length) :: errorMsg
    TYPE( TokenStack )              :: operator_stack
    TYPE( Token )                   :: tok
    INTEGER                         :: i
    
      !success = .FALSE. 

      CALL parser % postfix % Construct( Stack_Length )
      CALL operator_stack % Construct( Stack_Length )
  
      DO i = 1, parser % infix % top_index
     
        IF( parser % inFix % tokens(i) % tokenType == Variable_Token .OR. &
            parser % inFix % tokens(i) % tokenType == Number_Token )THEN

          
          CALL parser % postFix % push( parser % inFix % tokens(i) )

  
        ELSEIF( parser % inFix % tokens(i) % tokenType == Function_Token )THEN

          CALL operator_stack % push( parser % inFix % tokens(i) )

        ELSEIF( parser % inFix % tokens(i) % tokenType == Operator_Token &
                .OR. parser % inFix % tokens(i) % tokenType == Monadic_Token )THEN


          IF( .NOT. operator_stack % IsEmpty( ) )THEN

            tok = operator_stack % TopToken( )
              
            DO WHILE( TRIM(tok % tokenString) /= "(" .AND. &
                      parser % Priority( TRIM(tok % tokenString) ) > &
                      parser % Priority( TRIM(parser % inFix % tokens(i) % tokenString) ) )
       
              CALL parser % postFix % push( tok )
              CALL operator_stack % pop( tok )
              tok = operator_stack % TopToken( )

            ENDDO

          ENDIF

          CALL operator_stack % push( parser % inFix % tokens(i) )

        ELSEIF( parser % inFix % tokens(i) % tokenType == OpeningParentheses_Token )THEN

          CALL operator_stack % push( parser % inFix % tokens(i) )


        ELSEIF( parser % inFix % tokens(i) % tokenType == ClosingParentheses_Token )THEN

          tok = operator_stack % TopToken( )

          DO WHILE( .NOT.( operator_stack % IsEmpty( ) ) .AND. TRIM(tok % tokenString) /= "(" )
            
            CALL parser % postFix % push( tok )
            CALL operator_stack % pop( tok )
            tok = operator_stack % TopToken( )

          ENDDO

          ! Pop the opening parenthesis
          CALL operator_stack % pop( tok )

        ENDIF

      ENDDO

      ! Pop the remaining operators
      DO WHILE( .NOT.( operator_stack % IsEmpty( ) ) )
        
        tok = operator_stack % TopToken( )
        CALL parser % postFix % push( tok )
        CALL operator_stack % pop( tok )
   
      ENDDO
      
  END SUBROUTINE ConvertToPostFix

  FUNCTION Evaluate_sfp32( parser, x ) RESULT( f )
    CLASS(EquationParser) :: parser
    REAL(real32) :: x(1:parser % nIndepVars)
    REAL(real32) :: f
    ! Local
    INTEGER :: i, k
    TYPE(Token) :: t
    TYPE(sfp32Stack) :: stack
    REAL(real32) :: v, a, b, c
         
      CALL stack % Construct( Stack_Length )

      IF( .NOT.( ALLOCATED( parser % postfix % tokens ) ) )THEN

        f = 0.0_real32

      ELSE

        DO k = 1, parser % postfix % top_index 
  
          t = parser % postfix % tokens(k) % Equals_Token( )
  
          SELECT CASE ( t % tokenType )
           
            CASE( Number_Token )
  
              IF( t % tokenString == "pi" .OR. t % tokenString == "PI" )     THEN
                 v = pi
              ELSE
                READ( t % tokenString, * ) v
              END IF
  
              CALL stack % Push( v )
                 
            CASE ( Variable_Token )
  
              DO i = 1, parser % nIndepVars
                IF( TRIM( t % tokenString ) == parser % indepVars(i) )THEN
                  CALL stack % Push( x(i) )
                  EXIT
                ENDIF
              ENDDO
  
            CASE ( Operator_Token )
  
              CALL stack % Pop( a )
              CALL stack % Pop( b )
  
              SELECT CASE ( TRIM(t % tokenString) )
  
                 CASE ( "+" )
  
                    c = a + b
  
                 CASE ( "-" )
  
                    c = b - a
  
                 CASE ( "*" )
  
                    c = a*b
  
                 CASE ( "/" )
  
                    c = b/a
  
                 CASE ( "^" )
  
                    c = b**a
                 CASE DEFAULT
  
              END SELECT
  
              CALL stack % Push( c )
              
           CASE ( Function_Token )
  
              CALL stack % Pop( a )
  
              call parser % func % f_of_x( TRIM(t % tokenString), a, b )
  
              CALL stack % Push( b )
              
           CASE ( Monadic_Token )
  
             IF( TRIM(t % tokenString) == "-" )     THEN
  
                CALL stack % Pop( a )
                a = -a
                CALL stack % Push( a )
  
             END IF
             
           CASE DEFAULT
  
         END SELECT
  
       END DO
  
       CALL stack % Pop( a )
       f = a
  
       CALL stack % Destruct( )

     ENDIF
         
  END FUNCTION Evaluate_sfp32

  FUNCTION Evaluate_sfp64( parser, x ) RESULT( f )
    CLASS(EquationParser) :: parser
    REAL(real64) :: x(1:parser % nIndepVars)
    REAL(real64) :: f
    ! Local
    INTEGER :: i, k
    TYPE(Token) :: t
    TYPE(sfp64Stack) :: stack
    REAL(real64) :: v, a, b, c

      CALL stack % Construct( Stack_Length )

      IF( .NOT.( ALLOCATED( parser % postfix % tokens ) ) )THEN

        f = 0.0_real64

      ELSE

        DO k = 1, parser % postfix % top_index 
  
          t = parser % postfix % tokens(k) % Equals_Token( )
          
          SELECT CASE ( t % tokenType )
           
            CASE( Number_Token )
  
              IF( t % tokenString == "pi" .OR. t % tokenString == "PI" )     THEN
                 v = pi
              ELSE
                READ( t % tokenString, * ) v
              END IF
  
              CALL stack % Push( v )
                 
            CASE ( Variable_Token )
  
              DO i = 1, parser % nIndepVars
                IF( TRIM( t % tokenString ) == parser % indepVars(i) )THEN
                  CALL stack % Push( x(i) )
                  EXIT
                ENDIF
              ENDDO
  
            CASE ( Operator_Token )
  
              CALL stack % Pop( a )
              CALL stack % Pop( b )
  
              SELECT CASE ( TRIM(t % tokenString) )
  
                 CASE ( "+" )
  
                    c = a + b
  
                 CASE ( "-" )
  
                    c = b - a
  
                 CASE ( "*" )
  
                    c = a*b
  
                 CASE ( "/" )
  
                    c = b/a
  
                 CASE ( "^" )
  
                    c = b**a
                 CASE DEFAULT
  
              END SELECT
  
              CALL stack % Push( c )
              
           CASE ( Function_Token )
  
              CALL stack % Pop( a )
  
              call parser % func % f_of_x( TRIM(t % tokenString), a, b )
  
              CALL stack % Push( b )
              
           CASE ( Monadic_Token )
  
             IF( TRIM(t % tokenString) == "-" )     THEN
  
                CALL stack % Pop( a )
                a = -a
                CALL stack % Push( a )
  
             END IF
             
           CASE DEFAULT
  
         END SELECT
  
       END DO
  
       CALL stack % Pop( a )
       f = a
  
       CALL stack % Destruct( )

     ENDIF
         
  END FUNCTION Evaluate_sfp64

  SUBROUTINE Print_InfixTokens( parser )
    CLASS( EquationParser ), INTENT(in) :: parser
    ! Local
    INTEGER :: i

      DO i = 1, parser % inFix % top_index
        PRINT*, TRIM( parser % inFix % tokens(i) % tokenString )
      ENDDO

  END SUBROUTINE Print_InfixTokens

  SUBROUTINE Print_PostfixTokens( parser )
    CLASS( EquationParser ), INTENT(in) :: parser
    ! Local
    INTEGER :: i

      DO i = 1, parser % postFix % top_index
        PRINT*, TRIM( parser % postFix % tokens(i) % tokenString ), parser % postFix % tokens(i) % tokenType
      ENDDO

  END SUBROUTINE Print_PostfixTokens

  ! Support Functions !

  LOGICAL FUNCTION IsSeparator( eqChar )
    CHARACTER(1) :: eqChar
    ! Local
    INTEGER :: i

      IsSeparator = .FALSE.
      DO i = 1, nSeparators 

        IF( eqChar == separators(i) )THEN
          IsSeparator = .TRUE.
        ENDIF

      ENDDO

  END FUNCTION IsSeparator

  LOGICAL FUNCTION IsNumber( eqChar )
    CHARACTER(1) :: eqChar
    ! Local
    INTEGER :: i

      IsNumber = .FALSE.

      IF( eqChar == '.' .OR. eqChar == 'p' .OR. eqChar == 'P' )THEN
        IsNumber = .TRUE.
        RETURN
      ENDIF
         
      DO i = 1, 10

        IF( eqChar == numbers(i) )THEN
          IsNumber = .TRUE.
        ENDIF

      ENDDO

  END FUNCTION IsNumber

  LOGICAL FUNCTION IsVariable( eqChar, variables, nvariables )
    CHARACTER(1) :: eqChar
    INTEGER      :: nvariables
    CHARACTER(1) :: variables(1:nvariables)
    ! Local
    INTEGER :: i

      IsVariable = .FALSE.
      DO i = 1, nvariables

        IF( eqChar == variables(i) )THEN
          IsVariable = .TRUE.
        ENDIF

      ENDDO

  END FUNCTION IsVariable

  LOGICAL FUNCTION IsOperator( eqChar )
    CHARACTER(1) :: eqChar
    ! Local
    INTEGER :: i

      IsOperator = .FALSE.
      DO i = 1, 5

        IF( eqChar == operators(i) )THEN
          IsOperator = .TRUE.
        ENDIF

      ENDDO

  END FUNCTION IsOperator

  INTEGER FUNCTION Priority(parser, operatorString )
    class(EquationParser) :: parser
    CHARACTER(1) :: operatorString


      IF( parser % func % IsFunction( operatorString ) )THEN

        Priority = 4

      ELSEIF( operatorString == '^' )THEN

        Priority = 3

      ELSEIF( operatorString == '*' .OR. operatorString == '/' )THEN

        Priority = 2

      ELSEIF( operatorString == '+' .OR. operatorString == '-' )THEN
   
        Priority = 1
 
      ELSE

        Priority = 0
      
      ENDIF

  END FUNCTION Priority

END MODULE FEQParse
