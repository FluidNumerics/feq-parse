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

module FEQParse

    use iso_fortran_env
    use FEQParse_Functions
    use FEQParse_TokenStack
    use FEQParse_FloatStacks

    implicit none

    real(real64), parameter :: pi_real64 = 4.0_real64 * atan(1.0_real64)
    real(real32), parameter :: pi_real32 = 4.0_real32 * atan(1.0_real32)

    integer, parameter, private :: Error_Message_Length = 256
    integer, parameter, private :: Stack_Length = 256

    ! Token types
    enum, bind(c)
        enumerator :: None_Token = 0
        enumerator :: Number_Token = 1
        enumerator :: Variable_Token = 2
        enumerator :: Operator_Token = 3
        enumerator :: Function_Token = 4
        enumerator :: OpeningParentheses_Token = 5
        enumerator :: ClosingParentheses_Token = 6
        enumerator :: Monadic_Token = 7
    end enum
    
    integer, parameter, private :: nSeparators = 7

    character(1), private  :: separators(7) = ['+', '-', '*', '/', "(", ")", '^']
    character(1), private  :: operators(5) = ['+', '-', '*', '/', '^']
    character(1), private :: numbers(10) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    ! Private Types !

    private

    type, public :: EquationParser
        character(:), allocatable                   :: equation
        character(:), allocatable                   :: variableName
        character(:), allocatable                   :: inFixFormula
        integer                                     :: nIndepVars
        type(IndepVar), dimension(:), allocatable   :: indepVars
        type(TokenStack)                            :: inFix
        type(TokenStack)                            :: postFix
    contains
        procedure :: CleanEquation
        procedure :: Tokenize
        procedure :: ConvertToPostfix

        generic ::  Evaluate => Evaluate_sfp32, Evaluate_sfp64, &
                    Evaluate_r1fp32, Evaluate_r1fp64, &
                    Evaluate_r2fp32, Evaluate_r2fp64, &
                    Evaluate_r3fp32, Evaluate_r3fp64, &
                    Evaluate_r4fp32, Evaluate_r4fp64
        procedure, private :: Evaluate_sfp32, Evaluate_sfp64
        procedure, private :: Evaluate_r1fp32, Evaluate_r1fp64
        procedure, private :: Evaluate_r2fp32, Evaluate_r2fp64
        procedure, private :: Evaluate_r3fp32, Evaluate_r3fp64
        procedure, private :: Evaluate_r4fp32, Evaluate_r4fp64

        procedure :: Print_InFixTokens
        procedure :: Print_PostFixTokens

        procedure, private :: Priority

    end type EquationParser

    type IndepVar
        character(:), allocatable :: value
    end type

    interface EquationParser
        procedure Construct_EquationParser
    end interface EquationParser

contains

    function Construct_EquationParser(equation, indepVars) result(parser)
        type(EquationParser) :: parser
        character(*)         :: equation
        character(*)         :: indepVars(:)
        ! Local
        integer :: i
        character(Error_Message_Length) :: errorMsg
        logical                         :: equationIsClean, tokenized, success
        integer                         :: nIndepVars

        call InitializeFunctions()

        nIndepVars = size(indepVars)
        allocate (parser%indepVars(1:nIndepVars))
        parser%nIndepVars = nIndepVars
        do i = 1, nIndepVars
            parser%indepVars(i)%value = trim(indepVars(i))
        end do

        parser%equation = trim(equation)
        if (allocated(parser%inFixFormula)) deallocate (parser%inFixFormula)
        allocate (character(len(parser%equation) + maxFunctionLength) :: parser%inFixFormula)
        parser%variableName = '#noname'
        errorMsg = ' '

        call parser%CleanEquation(equationIsClean, errorMsg)

        if (equationIsClean) then

            call parser%Tokenize(tokenized, errorMsg)

            if (tokenized) then

                call parser%ConvertToPostFix()

            else

                print*,trim(errorMsg)
                success = .false.

            end if

        end if

    end function Construct_EquationParser

    subroutine CleanEquation(parser, equationCleaned, errorMsg)
        class(EquationParser), intent(inout)         :: parser
        logical, intent(out)                         :: equationCleaned
        character(Error_Message_Length), intent(out) :: errorMsg
        ! Local
        integer :: nChar, equalSignLoc, j, i, n
        character(:), allocatable :: infixformula

        equationCleaned = .false.

        equalSignLoc = index(parser%equation, '=')
        if (equalSignLoc == 0) then
            errorMsg = 'No variable name found.'
        else
            parser%variableName = parser%equation(1:equalSignLoc - 1)
        end if

        !Replace ** for ^
        parser%equation = ReplaceStr(parser%equation, '**', '^')
        nChar = len_trim(parser%equation)

        ! Grab the formula to the right of the equal sign and left adjust the formula
        inFixFormula = parser%equation(equalSignLoc + 1:)
        ! Remove any spaces
        j = 1
        n = len(inFixFormula)
        do i = 1, n
            if (inFixFormula(i:i) /= ' ') then
                parser%inFixFormula(j:j) = inFixFormula(i:i)
                j = j + 1
            end if
        end do

        parser%inFixFormula(j:) = ' '
        equationCleaned = .true.

    end subroutine CleanEquation

    pure recursive function ReplaceStr(string, search, substitute) result(modifiedString)
        implicit none
        character(*), intent(in)  :: string, search, substitute
        character(:), allocatable :: modifiedString
        integer                       :: i, stringLen, searchLen
        stringLen = len(string)
        searchLen = len(search)
        if (stringLen == 0 .or. searchLen == 0) then
            modifiedString = ''
            return
        elseif (stringLen < searchLen) then
            modifiedString = string
            return
        end if
        i = 1
        do
            if (string(i:i + searchLen - 1) == search) then
                modifiedString = string(1:i - 1)//substitute//replaceStr(string(i + searchLen:stringLen), search, substitute)
                exit
            end if
            if (i + searchLen > stringLen) then
                modifiedString = string
                exit
            end if
            i = i + 1
            cycle
        end do
    end function ReplaceStr

    subroutine Tokenize(parser, tokenized, errorMsg)
        class(EquationParser), intent(inout) :: parser
        logical, intent(out)                   :: tokenized
        character(Error_Message_Length)        :: errorMsg
        ! Local
        integer :: i, j, k
        integer, allocatable :: maxVarLen, varLen

        tokenized = .false.
        errorMsg = ' '

        call parser%infix%Construct(Stack_Length)

        maxVarLen = 0
        do k = 1, parser%nIndepVars
            maxVarLen = max(maxVarLen, len(parser%indepVars(k)%value))
        end do

        i = 1
        do while (parser%inFixFormula(i:i) /= ' ')
            varLen = maxVarLen
            if (IsFunction(j, parser%infixFormula(i:i + maxFunctionLength - 1))) then

                parser%inFix%top_index = parser%inFix%top_index + 1
                parser%inFix%tokens(parser%inFix%top_index)%tokenString = parser%inFixFormula(i:i + j - 1)
                parser%inFix%tokens(parser%inFix%top_index)%tokenType = Function_Token
                i = i + j

                ! Check to see if the next string
                if (parser%inFixFormula(i:i) /= '(') then
                    errorMsg = 'Missing opening parentheses after token : '// &
                               trim(parser%inFix%tokens(parser%inFix%top_index)%tokenString)

                    return
                end if

            elseif (IsVariable(varLen, parser%inFixFormula(i:i + varLen - 1), parser%indepVars, parser%nIndepVars)) then
                parser%inFix%top_index = parser%inFix%top_index + 1
                parser%inFix%tokens(parser%inFix%top_index)%tokenString = parser%inFixFormula(i:i + varLen - 1)
                parser%inFix%tokens(parser%inFix%top_index)%tokenType = Variable_Token
                i = i + varLen

                ! Next item must be an operator, closing parentheses, or end of equation

                if (.not. IsOperator(parser%infixFormula(i:i)) .and. &
                    parser%inFixFormula(i:i) /= ')' .and. parser%inFixFormula(i:i) /= ' ') then

                    errorMsg = 'Missing operator or closing parentheses after token : '// &
                               trim(parser%inFix%tokens(parser%inFix%top_index)%tokenString)
                    return

                end if

            elseif (IsNumber(parser%inFixFormula(i:i))) then

                parser%inFix%top_index = parser%inFix%top_index + 1
                parser%inFix%tokens(parser%inFix%top_index)%tokenString = ''

                if (parser%inFixFormula(i:i) == 'p' .or. parser%inFixFormula(i:i) == 'P') then

                    ! Conditional for using built in 'pi' definition
                    parser%inFix%tokens(parser%inFix%top_index)%tokenString(1:2) = parser%inFixFormula(i:i + 1)
                    j = 2

                else

                    j = 0
                    do while (IsNumber(parser%inFixFormula(i + j:i + j)))

                        parser%inFix%tokens(parser%inFix%top_index)%tokenString(j + 1:j + 1) = parser%inFixFormula(i + j:i + j)
                        j = j + 1

                    end do

                end if

                parser%inFix%tokens(parser%inFix%top_index)%tokenType = Number_Token

                i = i + j

                ! Next item must be an operator or a closing parentheses
                if (.not. IsOperator(parser%infixFormula(i:i)) .and. &
                    parser%inFixFormula(i:i) /= ')' .and. parser%inFixFormula(i:i) /= ' ') then

                    errorMsg = 'Missing operator or closing parentheses after token : '// &
                               trim(parser%inFix%tokens(parser%inFix%top_index)%tokenString)
                    return

                end if

            elseif (IsSeparator(parser%inFixFormula(i:i))) then

                parser%inFix%top_index = parser%inFix%top_index + 1
                parser%inFix%tokens(parser%inFix%top_index)%tokenString = parser%inFixFormula(i:i)

                if (parser%inFixFormula(i:i) == '(') then
                    parser%inFix%tokens(parser%inFix%top_index)%tokenType = OpeningParentheses_Token
                elseif (parser%inFixFormula(i:i) == ')') then
                    parser%inFix%tokens(parser%inFix%top_index)%tokenType = ClosingParentheses_Token
                else
                    parser%inFix%tokens(parser%inFix%top_index)%tokenType = Operator_Token
                end if

                i = i + 1
            else

                errorMsg = 'Invalid Token : '// &
                           trim(parser%inFixFormula(i:i))

                return

            end if
        end do

        if (parser%inFix%tokens(1)%tokenType == Operator_Token) then
            if (trim(parser%inFix%tokens(1)%tokenString) == '+' .or. &
                trim(parser%inFix%tokens(1)%tokenString) == '-') then
                parser%inFix%tokens(1)%tokenType = Monadic_Token
            end if
        end if

        do i = 2, parser%inFix%top_index
            if (parser%inFix%tokens(i)%tokenType == Operator_Token .and. &
                parser%inFix%tokens(i - 1)%tokenType == OpeningParentheses_Token) then
                parser%inFix%tokens(i)%tokenType = Monadic_Token
            end if
        end do

        tokenized = .true.

    end subroutine Tokenize

    subroutine ConvertToPostFix(parser)
        class(EquationParser), intent(inout) :: parser
        ! Local
        character(Error_Message_Length) :: errorMsg
        type(TokenStack)              :: operator_stack
        type(Token)                   :: tok
        integer                       :: i

        !success = .FALSE.

        call parser%postfix%Construct(Stack_Length)
        call operator_stack%Construct(Stack_Length)

        do i = 1, parser%infix%top_index
            !print*, parser % inFix % tokens(i) % tokenString
            if (parser%inFix%tokens(i)%tokenType == Variable_Token .or. &
                parser%inFix%tokens(i)%tokenType == Number_Token) then

                call parser%postFix%push(parser%inFix%tokens(i))

            elseif (parser%inFix%tokens(i)%tokenType == Function_Token) then

                call operator_stack%push(parser%inFix%tokens(i))

            elseif (parser%inFix%tokens(i)%tokenType == Operator_Token &
                    .or. parser%inFix%tokens(i)%tokenType == Monadic_Token) then

                if (.not. operator_stack%IsEmpty()) then

                    tok = operator_stack%TopToken()
                    
                    do while (trim(tok%tokenString) /= '(' .and. &
                              parser%Priority(tok) > &
                              parser%Priority(parser%inFix%tokens(i)))

                        call parser%postFix%push(tok)
                        call operator_stack%pop(tok)
                        tok = operator_stack%TopToken()

                    end do

                end if

                call operator_stack%push(parser%inFix%tokens(i))

            elseif (parser%inFix%tokens(i)%tokenType == OpeningParentheses_Token) then

                call operator_stack%push(parser%inFix%tokens(i))

            elseif (parser%inFix%tokens(i)%tokenType == ClosingParentheses_Token) then

                tok = operator_stack%TopToken()

                do while (.not. (operator_stack%IsEmpty()) .and. tok%tokenString(1:1) /= '(')

                    call parser%postFix%push(tok)
                    call operator_stack%pop(tok)
                    tok = operator_stack%TopToken()

                end do

                ! Pop the opening parenthesis
                call operator_stack%pop(tok)

            end if

        end do

        ! Pop the remaining operators
        do while (.not. (operator_stack%IsEmpty()))

            tok = operator_stack%TopToken()
            call parser%postFix%push(tok)
            call operator_stack%pop(tok)

        end do

    end subroutine ConvertToPostFix

    function Evaluate_sfp32(parser, x) result(f)
        class(EquationParser) :: parser
        real(real32) :: x(1:parser%nIndepVars)
        real(real32) :: f
        ! Local
        integer :: i, k
        type(Token) :: t
        type(sfp32Stack) :: stack
        real(real32) :: v, a, b, c

        call stack%Construct(Stack_Length)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)

                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real32
                else
                    read (t%tokenString, *) v
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a
    end function Evaluate_sfp32

    function Evaluate_sfp64(parser, x) result(f)
        class(EquationParser) :: parser
        real(real64) :: x(1:parser%nIndepVars)
        real(real64) :: f
        ! Local
        integer :: i, k
        type(Token) :: t
        type(sfp64Stack) :: stack
        real(real64) :: v, a, b, c

        call stack%Construct(Stack_Length)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)

                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real64
                else
                    read (t%tokenString, *) v
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a
    end function Evaluate_sfp64

    function Evaluate_r1fp32(parser, x) result(f)
        class(EquationParser) :: parser
        real(real32) :: x(:, :)
        real(real32) :: f(lbound(x, 1):ubound(x, 1))
        ! Local
        integer :: i, k
        type(Token) :: t
        type(r1fp32Stack) :: stack
        real(real32) :: vnumber
        real(real32), allocatable :: v(:)
        real(real32), allocatable :: a(:)
        real(real32), allocatable :: b(:)
        real(real32), allocatable :: c(:)

        allocate (v(lbound(x, 1):ubound(x, 1)), &
                  a(lbound(x, 1):ubound(x, 1)), &
                  b(lbound(x, 1):ubound(x, 1)), &
                  c(lbound(x, 1):ubound(x, 1)))

        call stack%Construct(Stack_Length, v)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)
                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real32
                else
                    read (t%tokenString, *) vnumber
                    v = vnumber
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(:, i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a
        deallocate (v, a, b, c)

    end function Evaluate_r1fp32

    function Evaluate_r1fp64(parser, x) result(f)
        class(EquationParser) :: parser
        real(real64) :: x(:, :)
        real(real64) :: f(lbound(x, 1):ubound(x, 1))
        ! Local
        integer :: i, k
        type(Token) :: t
        type(r1fp64Stack) :: stack
        real(real64) :: vnumber
        real(real64), allocatable :: v(:)
        real(real64), allocatable :: a(:)
        real(real64), allocatable :: b(:)
        real(real64), allocatable :: c(:)

        allocate (v(lbound(x, 1):ubound(x, 1)), &
                  a(lbound(x, 1):ubound(x, 1)), &
                  b(lbound(x, 1):ubound(x, 1)), &
                  c(lbound(x, 1):ubound(x, 1)))

        call stack%Construct(Stack_Length, v)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)

                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real64
                else
                    read (t%tokenString, *) vnumber
                    v = vnumber
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(:, i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a
        deallocate (v, a, b, c)

    end function Evaluate_r1fp64

    function Evaluate_r2fp32(parser, x) result(f)
        class(EquationParser) :: parser
        real(real32) :: x(:, :, :)
        real(real32) :: f(lbound(x, 1):ubound(x, 1), &
                          lbound(x, 2):ubound(x, 2))
        ! Local
        integer :: i, k
        type(Token) :: t
        type(r2fp32Stack) :: stack
        real(real32) :: vnumber
        real(real32), allocatable :: v(:, :)
        real(real32), allocatable :: a(:, :)
        real(real32), allocatable :: b(:, :)
        real(real32), allocatable :: c(:, :)
        integer :: l1, l2, u1, u2

        l1 = lbound(x, 1)
        l2 = lbound(x, 2)
        u1 = ubound(x, 1)
        u2 = ubound(x, 2)
        allocate (v(l1:u1, l2:u2), &
                  a(l1:u1, l2:u2), &
                  b(l1:u1, l2:u2), &
                  c(l1:u1, l2:u2))

        call stack%Construct(Stack_Length, v)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)
                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real32
                else
                    read (t%tokenString, *) vnumber
                    v = vnumber
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(:, :, i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a

        deallocate (v, a, b, c)

    end function Evaluate_r2fp32

    function Evaluate_r2fp64(parser, x) result(f)
        class(EquationParser) :: parser
        real(real64) :: x(:, :, :)
        real(real64) :: f(lbound(x, 1):ubound(x, 1), &
                          lbound(x, 2):ubound(x, 2))
        ! Local
        integer :: i, k
        type(Token) :: t
        type(r2fp64Stack) :: stack
        real(real64) :: vnumber
        real(real64), allocatable :: v(:, :)
        real(real64), allocatable :: a(:, :)
        real(real64), allocatable :: b(:, :)
        real(real64), allocatable :: c(:, :)
        integer :: l1, l2, u1, u2

        l1 = lbound(x, 1)
        l2 = lbound(x, 2)
        u1 = ubound(x, 1)
        u2 = ubound(x, 2)
        allocate (v(l1:u1, l2:u2), &
                  a(l1:u1, l2:u2), &
                  b(l1:u1, l2:u2), &
                  c(l1:u1, l2:u2))

        call stack%Construct(Stack_Length, v)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)

                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real64
                else
                    read (t%tokenString, *) vnumber
                    v = vnumber
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(:, :, i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a

        deallocate (v, a, b, c)

    end function Evaluate_r2fp64

    function Evaluate_r3fp32(parser, x) result(f)
        class(EquationParser) :: parser
        real(real32) :: x(:, :, :, :)
        real(real32) :: f(lbound(x, 1):ubound(x, 1), &
                          lbound(x, 2):ubound(x, 2), &
                          lbound(x, 3):ubound(x, 3))
        ! Local
        integer :: i, k
        type(Token) :: t
        type(r3fp32Stack) :: stack
        real(real32) :: vnumber
        real(real32), allocatable :: v(:, :, :)
        real(real32), allocatable :: a(:, :, :)
        real(real32), allocatable :: b(:, :, :)
        real(real32), allocatable :: c(:, :, :)
        integer :: l1, l2, l3, u1, u2, u3

        l1 = lbound(x, 1)
        l2 = lbound(x, 2)
        l3 = lbound(x, 3)
        u1 = ubound(x, 1)
        u2 = ubound(x, 2)
        u3 = ubound(x, 3)
        allocate (v(l1:u1, l2:u2, l3:u3), &
                  a(l1:u1, l2:u2, l3:u3), &
                  b(l1:u1, l2:u2, l3:u3), &
                  c(l1:u1, l2:u2, l3:u3))

        call stack%Construct(Stack_Length, v)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)
                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real32
                else
                    read (t%tokenString, *) vnumber
                    v = vnumber
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(:, :, :, i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a

        deallocate (v, a, b, c)

    end function Evaluate_r3fp32

    function Evaluate_r3fp64(parser, x) result(f)
        class(EquationParser) :: parser
        real(real64) :: x(:, :, :, :)
        real(real64) :: f(lbound(x, 1):ubound(x, 1), &
                          lbound(x, 2):ubound(x, 2), &
                          lbound(x, 3):ubound(x, 3))
        ! Local
        integer :: i, k
        type(Token) :: t
        type(r3fp64Stack) :: stack
        real(real64) :: vnumber
        real(real64), allocatable :: v(:, :, :)
        real(real64), allocatable :: a(:, :, :)
        real(real64), allocatable :: b(:, :, :)
        real(real64), allocatable :: c(:, :, :)
        integer :: l1, l2, l3, u1, u2, u3

        l1 = lbound(x, 1)
        l2 = lbound(x, 2)
        l3 = lbound(x, 3)
        u1 = ubound(x, 1)
        u2 = ubound(x, 2)
        u3 = ubound(x, 3)
        allocate (v(l1:u1, l2:u2, l3:u3), &
                  a(l1:u1, l2:u2, l3:u3), &
                  b(l1:u1, l2:u2, l3:u3), &
                  c(l1:u1, l2:u2, l3:u3))

        call stack%Construct(Stack_Length, v)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)

                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real64
                else
                    read (t%tokenString, *) vnumber
                    v = vnumber
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(:, :, :, i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a
        deallocate (v, a, b, c)

    end function Evaluate_r3fp64

    function Evaluate_r4fp32(parser, x) result(f)
        class(EquationParser) :: parser
        real(real32) :: x(:, :, :, :, :)
        real(real32) :: f(lbound(x, 1):ubound(x, 1), &
                          lbound(x, 2):ubound(x, 2), &
                          lbound(x, 3):ubound(x, 3), &
                          lbound(x, 4):ubound(x, 4))
        ! Local
        integer :: i, k
        type(Token) :: t
        type(r4fp32Stack) :: stack
        real(real32) :: vnumber
        real(real32), allocatable :: v(:, :, :, :)
        real(real32), allocatable :: a(:, :, :, :)
        real(real32), allocatable :: b(:, :, :, :)
        real(real32), allocatable :: c(:, :, :, :)
        integer :: l1, l2, l3, l4, u1, u2, u3, u4

        l1 = lbound(x, 1)
        l2 = lbound(x, 2)
        l3 = lbound(x, 3)
        l4 = lbound(x, 3)
        u1 = ubound(x, 1)
        u2 = ubound(x, 2)
        u3 = ubound(x, 3)
        u4 = ubound(x, 4)
        allocate (v(l1:u1, l2:u2, l3:u3, l4:u4), &
                  a(l1:u1, l2:u2, l3:u3, l4:u4), &
                  b(l1:u1, l2:u2, l3:u3, l4:u4), &
                  c(l1:u1, l2:u2, l3:u3, l4:u4))

        call stack%Construct(Stack_Length, v)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)
                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real32
                else
                    read (t%tokenString, *) vnumber
                    v = vnumber
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(:, :, :, :, i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a

        deallocate (v, a, b, c)

    end function Evaluate_r4fp32

    function Evaluate_r4fp64(parser, x) result(f)
        class(EquationParser) :: parser
        real(real64) :: x(:, :, :, :, :)
        real(real64) :: f(lbound(x, 1):ubound(x, 1), &
                          lbound(x, 2):ubound(x, 2), &
                          lbound(x, 3):ubound(x, 3), &
                          lbound(x, 4):ubound(x, 4))
        ! Local
        integer :: i, k
        type(Token) :: t
        type(r4fp64Stack) :: stack
        real(real64) :: vnumber
        real(real64), allocatable :: v(:, :, :, :)
        real(real64), allocatable :: a(:, :, :, :)
        real(real64), allocatable :: b(:, :, :, :)
        real(real64), allocatable :: c(:, :, :, :)
        integer :: l1, l2, l3, l4, u1, u2, u3, u4

        l1 = lbound(x, 1)
        l2 = lbound(x, 2)
        l3 = lbound(x, 3)
        l4 = lbound(x, 3)
        u1 = ubound(x, 1)
        u2 = ubound(x, 2)
        u3 = ubound(x, 3)
        u4 = ubound(x, 4)
        allocate (v(l1:u1, l2:u2, l3:u3, l4:u4), &
                  a(l1:u1, l2:u2, l3:u3, l4:u4), &
                  b(l1:u1, l2:u2, l3:u3, l4:u4), &
                  c(l1:u1, l2:u2, l3:u3, l4:u4))

        call stack%Construct(Stack_Length, v)

        do k = 1, parser%postfix%top_index

            t = parser%postfix%tokens(k)%Copy()

            select case (t%tokenType)

            case (Number_Token)

                if (t%tokenString == 'pi' .or. t%tokenString == 'PI') then
                    v = pi_real64
                else
                    read (t%tokenString, *) vnumber
                    v = vnumber
                end if

                call stack%Push(v)

            case (Variable_Token)

                do i = 1, parser%nIndepVars
                    if (trim(t%tokenString) == parser%indepVars(i)%value) then
                        call stack%Push(x(:, :, :, :, i))
                        exit
                    end if
                end do

            case (Operator_Token)

                call stack%Pop(a)
                call stack%Pop(b)

                select case (trim(t%tokenString))

                case ('+')

                    c = a + b

                case ('-')

                    c = b - a

                case ('*')

                    c = a * b

                case ('/')

                    c = b / a

                case ('^')

                    c = b**a
                case default

                end select

                call stack%Push(c)

            case (Function_Token)

                call stack%Pop(a)

                b = f_of_x(trim(t%tokenString), a)

                call stack%Push(b)

            case (Monadic_Token)

                if (trim(t%tokenString) == '-') then

                    call stack%Pop(a)
                    a = -a
                    call stack%Push(a)

                end if

            case default

            end select

        end do

        call stack%Pop(a)
        f = a

        deallocate (v, a, b, c)

    end function Evaluate_r4fp64

    subroutine Print_InfixTokens(parser)
        class(EquationParser), intent(in) :: parser
        ! Local
        integer :: i

        do i = 1, parser%inFix%top_index
            print*,trim(parser%inFix%tokens(i)%tokenString)
        end do

    end subroutine Print_InfixTokens

    subroutine Print_PostfixTokens(parser)
        class(EquationParser), intent(in) :: parser
        ! Local
        integer :: i

        do i = 1, parser%postFix%top_index
            print*,trim(parser%postFix%tokens(i)%tokenString), parser%postFix%tokens(i)%tokenType
        end do

    end subroutine Print_PostfixTokens

    ! Support Functions !

    logical function IsSeparator(eqChar)
        character(1) :: eqChar
        ! Local
        integer :: i

        IsSeparator = .false.
        do i = 1, nSeparators

            if (eqChar == separators(i)) then
                IsSeparator = .true.
            end if

        end do

    end function IsSeparator

    logical function IsNumber(eqChar)
        character(1) :: eqChar
        ! Local
        integer :: i

        IsNumber = .false.

        if (eqChar == '.' .or. eqChar == 'p' .or. eqChar == 'P') then
            IsNumber = .true.
            return
        end if

        do i = 1, 10

            if (eqChar == numbers(i)) then
                IsNumber = .true.
                return
            end if

        end do

    end function IsNumber

    logical function IsVariable(varlen, eqChar, variables, nvariables)
        integer, intent(inout) :: varlen
        character(*), intent(in) :: eqChar
        integer      :: nvariables
        type(IndepVar) :: variables(1:nvariables)
        ! Local
        integer :: i

        IsVariable = .false.
        varlen = 0
        if (any(separators(:) == eqChar(1:1))) return
        if (verify(eqChar(1:1), '0123456789') == 0) return

        do i = 1, nvariables

            if (index(eqChar, variables(i)%value) == 1) then
                IsVariable = .true.

                if (len(variables(i)%value) > varlen) then
                    varlen = len(variables(i)%value)
                end if
            end if
        end do
    end function IsVariable

    logical function IsOperator(eqChar)
        character(1) :: eqChar
        ! Local
        integer :: i

        IsOperator = .false.
        do i = 1, 5

            if (eqChar == operators(i)) then
                IsOperator = .true.
                return
            end if
        end do

    end function IsOperator

    logical function IsFunction(varlen, eqChar)
        integer, intent(inout) :: varlen
        character(*), intent(in) :: eqChar
        ! Local
        integer :: i

        IsFunction = .false.
        varlen = 0

        if (any(separators(:) == eqChar(1:1))) return
        if (verify(eqChar(1:1), '0123456789') == 0) return

        do i = 1, nFunctions
            if (Functions(i)%len <= len(eqChar)) then
                if (Functions(i) == eqChar(:Functions(i)%len)) then
                    IsFunction = .true.
                    varlen = max(Functions(i)%len, varlen)
                end if
            end if
        end do
    end function

    function ToLowerCase(str) result(res)
        character(*), intent(in) :: str
        character(len(str)), allocatable :: res
        integer :: i

        do i = 1, len(str)
            select case (str(i:i))
            case ('A':'Z')
                res(i:i) = achar(iachar(str(i:i)) + 32)
            case default
                res(i:i) = str(i:i)
            end select
        end do
    end function ToLowerCase

    integer function Priority(parser, toke)
        class(EquationParser) :: parser
        type(Token) :: toke

        if (toke%tokenType == Function_Token) then

            Priority = 5

        elseif (toke%tokenString(1:1) == '^') then

            Priority = 4

        elseif (toke%tokenString(1:1) == '/') then

            Priority = 3

        elseif (toke%tokenString(1:1) == '*') then

            Priority = 2

        elseif (toke%tokenString(1:1) == '+' .or. toke%tokenString(1:1) == '-') then

            Priority = 1

        else

            Priority = 0

        end if

    end function Priority

end module FEQParse
