PROGRAM Pulse_com

IMPLICIT NONE

CHARACTER (LEN=55) :: COMMON_PATH
CHARACTER (LEN=50) :: OUTPUT_NAME
CHARACTER (LEN=120) :: OUTPUT_PATH



INTEGER, PARAMETER :: k =1000
INTEGER, PARAMETER :: dk = 2*k

REAL*8 :: k2
REAL*8 :: dk2

REAL*8 :: E_0                               !Amplitude
REAL*8, DIMENSION(-k:k) :: E                                 !Pulse

REAL*8, DIMENSION(-k:k) :: phase            !Chirp phase
REAL*8 :: alpha                             !Chirp constant
REAL*8 :: alpha_pi                          !Chirp rate or FM rate, slope of chirp pulse
REAL*8 :: B_W = 15d+9                       !Bandwidth of chirp


REAL*8 :: f_c =0                            !Central frequency
REAL*8, DIMENSION(-k:k) :: f                !Instatenuous frequency, differentiation of phase

REAL*8 :: pi=ACOS(-1.0)

INTEGER :: t                                !Time
REAL*8, DIMENSION(-k:k) :: time
REAL*8  :: t_0 = 35.2d-6                    ![s] Pulse length
REAL*8  :: t_0_2


REAL*8 ::  local_os                         !Local oscillator


REAL*8, DIMENSION(-dk:dk) :: temp
COMPLEX*8, DIMENSION(-dk:dk) :: rect
COMPLEX*8, DIMENSION(-dk:dk) :: conj
COMPLEX*8, DIMENSION(-dk:dk) :: E_ref

COMPLEX :: test

INTEGER :: i,j

COMMON_PATH = "/home/changwan/SAR/"
OUTPUT_NAME = "Pulse_com.txt"

OUTPUT_PATH = TRIM(COMMON_PATH)//OUTPUT_NAME

OPEN(UNIT=20, FILE=OUTPUT_PATH, STATUS='REPLACE', ACTION='WRITE') 

!     ++++++++++++++++
!++++++Initialization++++++
!     ++++++++++++++++

E = 0
E_0 = 100


alpha_pi = alpha / pi
alpha_pi = -4.3d+11  ![Hz/s]       

t_0_2 = t_0/2.0

alpha = alpha_pi * pi


print *, "t_0/2=t_0_2 =", t_0_2
print *, "alpha_pi=",alpha_pi
print *, "alpha=",alpha
print *, "f_c=",f_c


DO t = -k, k, 1
   
   k2 = k
 
!     ++++++++++++++++++++++
!++++++phase of chipr pulse++++++
!     ++++++++++++++++++++++

     phase(t) = 2 * pi * f_c * (t_0_2*(t/k2)) + alpha * ((t_0_2*(t/k2))**2)

!     +++++++++++++++++++++++++++++++++++     
!++++++frequency property of chirp pulse++++++
!     +++++++++++++++++++++++++++++++++++

      f(t) = f_c + (alpha_pi)* (t_0_2*(t/k2))            
  
!     +++++++++++++++++++++++++++++++++++++++++++
!++++++Frequency Modulation Pulse or Chirp Pulse++++++
!     +++++++++++++++++++++++++++++++++++++++++++

      E(t) = E_0 * cos ( phase(t))

END DO


!     +++++++++++++++++++++++++++
!++++++bandwidth of chirp puplse++++++
!     +++++++++++++++++++++++++++


 !    b_w = ABS(alpha) * t_0 / pi



!     ++++++++++++++++++
!++++++reference signal++++++
!     ++++++++++++++++++
DO t = -dk, dk, 1

  IF (t >= -k .AND. t <= k) THEN
 rect(t) = 1
  ELSE
 rect(t) = 0
  END IF

END DO 


!(real_rect + j imag_rect) * (cos() + j sin())

!=[real_rect * cos() - imag_rect * sin()] + j [ imag_rect * cos() +  real_rect * sin()]



 
DO t = -dk, dk, 1   

dk2=dk

   temp(t) = -alpha * (t_0_2*(t/dk2))**2
   conj(t) = cmplx(0,temp(t))   

!   PRINT *, conj(t)

    E_ref(t) = rect(t) * exp( conj(t))       
END DO
!++++++++++++++++++++++++++++++

test =(0,1)
print *, conj(1)


!     ++++++++++++++++++++
!++++++write transmitted signal++++++
!     ++++++++++++++++++++

!DO  t =  -k, k, 1
    !PRINT *, t_0_2*(t/k2),"phase(",t,")","=",-phase(t)/pi, "E(",t,")=",E(t),"f(",t,")=",f(t)
!    WRITE(20,*) -phase(t)/pi,E(t),f(t)
!END DO

!+++++++++++++++++++++++++++++++++

!     ++++++++++++++++++++++++
!++++++write reference signal++++++
!     ++++++++++++++++++++++++

DO t = -dk, dk, 1
     WRITE(20, *) REAL(E_ref(t)), AIMAG(E_ref(t)) 
!    WRITE(20, *) E_r(t)
END DO 

!++++++++++++++++++++++++++++++++++

!DO t = -2k,2k,1  
!    WRITE(21,*) rect(t)
!END DO

!DO t = -k,k,1
   
!     local_os = 2 * cos(2*pi*f_*(t_0_2*(t/k2)))
END PROGRAM
